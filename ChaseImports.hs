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
import qualified Kindle 
import Depend

-- Data type of interface file -----------------------------------------------

data IFace = IFace [Name]                       -- imported modules
               [Defaults]                       --
               (Map Name [Name])                -- exported record types and their selectors,
               (Map Name ([Name],Syntax.Type))  -- type synonyms
               Types                            --
               Binds                            -- types for instances
               TEnv                             -- types for exported values (including sels and cons)
               Kindle.Decls                     -- Kindle form of declarations
               Kindle.TEnv                      -- Kindle type env
               (Map Name Name)                  -- Constructors to their datatype
           deriving (Show)

-- Building interface info from data collected during compilation of a module ---------------------------------

--ifaceMod :: ((Map Name [Name],Map Name ([Name],Syntax.Type)),([Name],Types,Binds,TEnv)) -> M s IFace
ifaceMod ((rs,ss),Module _ ns xs ds is bs,(kds,kte)) 
   | not(null vis)                = error ("Private types visible in interface: " ++ showids vis)
   | otherwise                    = do 
                                       let cs = Core2Kindle.dataCons ds
                                       return (IFace ns xs rs ss ds1 is' ts2' kds kte cs) 
  where Types ke te               = ds
        Binds r ts1 es1           = is
        ds1                       = Types (filter exported ke) (filter exported' te)
        is'                       = Binds r (filter exported ts1) (filter exported es1)
        ts2'                      = filter exported (tsigsOf bs)
        vis                       = nub(localTypes [] (rng (tsigsOf is') ++ rng ts2'))
        exported (n,_)            = isQualified n
        exported' p@(n,_)         = isQualified n && (not(isAbstract p)) --Constructors/selectors are exported

isPrivate (Name _ _ Nothing _)    = True
isPrivate _                       = False

isAbstract (n,DData _ _ ((c,_):_)) = isPrivate c
isAbstract (n,DRec _ _ _ ((c,_):_))= isPrivate c
isAbstract (n,_)                   = False     -- this makes abstract types without selectors/constructors non-private...


-- Getting import info ---------------------------------------------------------

chaseImports m imps             = do bms <- mapM readImport imps
                                     rms <- chaseRecursively (map impName imps) [] (concatMap (recImps . snd) bms)
                                     return (bms ++ rms)
  where readImport (Syntax.Import b c) = do ifc <- decodeFile (modToPath(str c) ++ ".ti")
                                            return (c,(b,True,ifc))
        impName (Syntax.Import b c)    = c
        chaseRecursively vs ms []      = return ms
        chaseRecursively vs ms (r : rs)
             | elem r vs               = chaseRecursively vs ms rs
             | otherwise               = do ifc@(IFace ns _ _ _ _ _ _ _ _ _)  <- decodeFile (modToPath (str r) ++ ".ti")
                                            chaseRecursively (r : vs) ((r,(False,False,ifc)) : ms) (rs ++ ns)

recImps (_,_,IFace ns _ _ _ _ _ _ _ _ _) = ns

init_order imps                        = case topSort recImps imps of
                                           Left ms  -> error ("Mutually recursive modules: " ++ showids ms)
                                           Right is -> map fst is


initEnvs bms         = do ims <- mapM mkEnv bms
                          let (rs,xs,ss,rnL,rnT,rnE,ds,is,te,kds,kte,cs) 
                                = foldr mergeMod ([],[],[],[],[],[],Types [] [],Binds False [] [],[],[],[],[]) ims
                          return ((rs,rnL,ss),(rnL,rnT,rnE),(xs,ds,te,is),(kds,kte,cs))

mergeMod (rs1,xs1,ss1,rnL1,rnT1,rnE1,ds1,is1,te1,kds1,kte1,cs1)
         (rs2,xs2,ss2,rnL2,rnT2,rnE2,ds2,is2,te2,kds2,kte2,cs2) =
                                   (rs1 ++ rs2, xs1 ++ xs2, ss1 ++ ss2, mergeRenamings2 rnL1 rnL2, 
                                    mergeRenamings2 rnT1 rnT2, mergeRenamings2 rnE1 rnE2,
                                    catDecls ds1 ds2, catBinds is1 is2, te1 ++ te2,
                                    kds1 ++ kds2,kte1 ++ kte2, cs1 ++ cs2)

mkEnv (m,(unQual,direct,IFace ns xs rs ss ds tsi te kds kte cs)) 
                          = do ks  <- renaming (dom ke)
                               ts  <- renaming (dom te'')
                               ls' <- renaming ls -- (concatMap snd rs)
                               return (unMod unQual rs, xs, unMod unQual ss, unMod unQual ls',unMod unQual ks,
                                       unMod unQual ts,ds,tsi,te',kds,kte,cs)
  where Types ke ds'      = ds
        te'               = if direct then te ++ concatMap (tenvSelCon ke) ds' else []
        te''              = if direct then te ++ concatMap (tenvCon ke) ds' else []
        ls                = [ s | (_,DRec _ _ _ cs) <- ds', (s,_) <- cs, not (isGenerated s) ]
        unMod b ps        = if b then [(tag0 (mName Nothing c),y) | (c,y) <- ps] ++ ps else ps

-- Checking that public part is closed ---------------------------------------------------------

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
       
-- Binary -------------------------------------------------------------------------------

instance Binary IFace  where
  put (IFace a b c d e f g h i j) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i >> put j
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f ->  
        get >>= \g -> get >>= \h -> get >>= \i -> get >>= \j -> return (IFace a b c d e f g h i j)

-- Printing -----------------------------------------------------------------------------

instance Pr IFace where
  pr (IFace ns xs rs ss ds1 is ts2' kds kte cs) =
                                  text "Imported/used modules: " <+> hsep (map prId ns) $$
                                  text "Default declarations: " <+> prDefault xs $$
                                  text ("Record types and their selectors: "++show rs) $$
                                  text "Type synonyms: " <+> hsep (map (prId . fst) ss) $$ 
                                  pr ds1 $$ pr is  $$ vcat (map prPair ts2') 
                                  $$$ text "Kindle declarations" 
                                  $$$ vcat (map pr kds)
                                  $$$ text "Kindle type environment" $$$ vcat (map pr kte)
                                  
prPair (n,t)                      = prId n <+> text "::" <+> pr t


listIface f                       = do ifc <- decodeFile f
                                       putStrLn (render(pr (ifc :: IFace)))
