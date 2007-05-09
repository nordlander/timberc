module ChaseImports where

import Common
import Data.Binary
import Env(addKEnv0, nullEnv)
import Rename 
import Core
import qualified Syntax
import Decls
import PP
import qualified Core2Kindle 

chaseImports m imps             = do bms <- mapM readImport imps
                                     rms <- chaseRecursively (map impName imps) [] (concatMap recImps bms)
                                     return (bms ++ rms)
  where readImport (Syntax.Import b c) = do m <- decodeFile (modToPath(str c) ++ ".ti")
                                            return (b,True,m)
        recImps (_,_,Module _ ns _ _ _)= ns
        impName (Syntax.Import b c)    = c
        chaseRecursively vs ms []      = return ms
        chaseRecursively vs ms (r : rs)
             | elem r vs               = chaseRecursively vs ms rs
             | otherwise               = do m@(Module _ ns _ _ _) <- decodeFile (modToPath (str r) ++ ".ti")
                                            chaseRecursively (r : vs) ((False,False,m) : ms) (rs ++ ns)

data IFace = IFace [Name]                       -- imported modules
               (Map Name [Name])                -- exported record types and their selectors,
               (Map Name ([Name],Syntax.Type))  -- type synonyms
               KEnv                             -- kinds for exported tycons
               TEnv                             -- types for exported values (including sels and cons)
               PEnv                             -- 
           deriving (Show)


-- record type name to selectors, renaming to qualified form for selectors, type synonym to type
type DsEnv                = (Map Name [Name], Map Name Name, Map Name ([Name],Syntax.Type)) 
-- renaming for selector, type and term names
type RnEnv                = (Map Name Name, Map Name Name, Map Name Name) 
-- kinds and types, types for terms, witnesses   
type TcEnv                = (Types, TEnv, Binds)                             



initEnvs bms         = do ims <- mapM mkEnv bms
                          let (rs,ss,rnL,rnT,rnE,ds,te,is) = foldr mergeMod ([],[],[],[],[],Types [] [],[],Binds True [] []) ims
                          kds <- Core2Kindle.cDecls ds
                          kte <- Core2Kindle.cTEnv (tsigsOf is ++ te)
                          let cs = Core2Kindle.dataCons ds
                          return ((rs,rnL,ss),(rnL,rnT,rnE),(ds,te,is),(kds,kte,cs),kds)

mergeMod (rs1,ss1,rnL1,rnT1,rnE1,ds1,te1,is1)
         (rs2,ss2,rnL2,rnT2,rnE2,ds2,te2,is2) =
                                   (rs1 ++ rs2, ss1 ++ ss2, mergeRenamings2 rnL1 rnL2, 
                                    mergeRenamings2 rnT1 rnT2, mergeRenamings2 rnE1 rnE2,
                                    catDecls ds1 ds2, te1 ++ te2,catBinds is1 is2)

mkEnv (unQual,direct,Module v ns ds is bs)  
                          = do ks  <- renaming (dom ke)
                               ts  <- renaming (dom te')
                               rs' <- renaming (concatMap snd rs)
                               return (unMod unQual rs,unMod unQual ss, unMod unQual rs',unMod unQual ks,
                                       unMod unQual ts,ds,te',is)
  where Types ke ds'      = ds
        Binds _ te _      = bs
        (rs,ts,ss)        = iDecls env [] [] [] ds'
        te'               = if direct then ts ++ te else []
        env               = addKEnv0 ke nullEnv
        unMod b ps        = if b then [(tag0 (mName Nothing c),y) | (c,y) <- ps] ++ ps else ps
 

iDecls env rs ts ss []    = (rs,ts,ss)
iDecls env rs ts ss (p@(i,DRec False vs [] cs):ps) 
                          = iDecls env ((i,[s | (s,_) <- cs, not (isGenerated s)]):rs) (tenvSelCon env p ++ ts) ss ps
iDecls env rs ts ss (p@(i,DRec True vs [] cs):ps) 
                          = iDecls env ((i,[s | (s,_) <- cs, not (isGenerated s)]):rs) ts ss ps
iDecls env rs ts ss (p@(i,DData vs [] cs):ps)      
                          = iDecls env rs (tenvSelCon env p ++ ts) ss ps
iDecls env rs ts ss ((i,DType vs t):ps)          
                          = iDecls env rs ts ((i,(vs,c2sType t)):ss) ps
iDecls env rs ts ss (_ : ps)                     
                          = iDecls env rs ts ss ps

c2sType (TId n) 
   | isCon n		  = Syntax.TCon (tag0 n)
   | otherwise            = Syntax.TVar (tag0 n)
c2sType (TFun ts t)       = Syntax.TFun (map c2sType ts) (c2sType t)
c2sType (TAp t1 t2)       = Syntax.TAp (c2sType t1) (c2sType t2)
c2sType (TVar _)          = error "Internal error: type synonym with TVar"



class LocalTypes a where 
  localTypes :: [Name] -> a -> [Name]

instance LocalTypes a => LocalTypes [a] where
  localTypes ns ds                = concatMap (localTypes ns) ds

instance LocalTypes b => LocalTypes (a,b) where
  localTypes ns (a,b)             = localTypes ns b

instance LocalTypes Scheme where
  localTypes ns (Scheme r ps ke)  = localTypes ns1 r ++ localTypes ns1 ps
    where ns1                     = ns ++ dom ke

instance LocalTypes Rho where
  localTypes ns (R t)             = localTypes ns t
  localTypes ns (F ss r)          = localTypes ns ss ++ localTypes ns r

instance LocalTypes Type where
  localTypes ns (TId n) 
     | n `elem` ns || not (isPrivate n) = []
     | otherwise                  = [n]
  localTypes _ (TVar t)           = error "Internal: ChaseImports.localTypes: TVar in interface file"
  localTypes ns (TFun ts t)       = localTypes ns (t : ts)
  localTypes ns (TAp t1 t2)       = localTypes ns [t1, t2]

instance LocalTypes Decl where
  localTypes ns (DData vs ps cs)  = localTypes ns1 ps ++ localTypes ns1 cs
    where ns1                     = ns ++ vs
  localTypes ns (DRec _ vs ps ss) = localTypes ns1 ps ++ localTypes ns1 ss
    where ns1                     = ns ++ vs
  localTypes ns (DType vs t)      = localTypes (ns ++ vs) t

instance LocalTypes Constr where
  localTypes ns (Constr ts ps ke) = localTypes ns1 ts ++ localTypes ns1 ps
    where ns1                     = ns ++ dom ke
       

ifaceMod (Module c ns ds is bs) 
   | not(null vis)                = error ("Private types visible in interface: " ++ showids vis)
   | otherwise                    = Module c ns ds1 is1 bs1
  where Types ke te               = ds
        Binds rec1 ts1 es1        = is
        Binds rec2 ts2 _          = bs
        ds1                       = Types (filter exported ke) (filter exported' te)
        ts1'                      = filter exported ts1
        ts2'                      = filter exported ts2
        is1                       = Binds rec1 ts1' []
        bs1                       = Binds rec2 ts2' []
        vis                       = nub(localTypes [] (rng ts1' ++ rng ts2'))
        exported (n,y)            = fromMod n /= Nothing
        exported' (n,d)           = fromMod n /= Nothing && (null (localTypes [] d)) --Constructors/selectors are exported

listIface f                       = do m <- decodeFile f
                                       putStrLn (render(pr (m :: Core.Module)))

isPrivate (Name _ _ Nothing _)    = True
isPrivate _                       = False

