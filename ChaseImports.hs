module ChaseImports where

import Common
import List (isPrefixOf)
import Data.Binary
import Rename 
import Core
import qualified Syntax
import Decls
import PP
import qualified Core2Kindle 
import qualified Kindle 
import Depend
import Termred
import qualified Config
import System

-- Data type of interface file -----------------------------------------------

data IFace = IFace { impsOf      :: [Name],                         -- imported/used modules
                     defaults    :: [Default Scheme],               -- exported default declarations
                     recordEnv   :: Map Name [Name],                -- exported record types and their selectors,
                     tsynEnv     :: Map Name ([Name],Syntax.Type),  -- type synonyms
                     tEnv        :: Types,                          -- Core type environment
                     insts       :: [Name],                         -- types for instances
                     valEnv      :: Binds,                          -- types for exported values (including sels and cons) and some finite eqns
                     kdeclEnv    :: Kindle.Decls                    -- Kindle form of declarations
                   }
           deriving (Show)

-- Building interface info from data collected during compilation of a module ---------------------------------
{-
Input to ifaceMod is
      - from Desugar1: exported record types with their selectors and exported type synomyms.
      - from Termred: the entire module in Core form after term reduction
      - from Core2Kindle: declarations in Kindle form.
The function checks that the public part is closed (does not mention private data) and computes
the IFace.
-}
 
ifaceMod                   :: (Map Name [Name], Map Name ([Name], Syntax.Type)) -> Module -> Kindle.Decls -> IFace
ifaceMod (rs,ss) (Module _ ns xs ds ws bss) kds
   | not(null vis)                   = errorIds "Private types visible in interface" vis
   | not(null ys)                    = errorTree "Public default declaration mentions private instance" (head ys)
--   | otherwise                       = IFace ns xs' rs ss ds1 is' bs' kds
   | otherwise                       = IFace ns xs' rs ss ds1 ws bs' kds
  where Types ke te                  = ds
  --      Binds r1 ts1 es1             = is
        Binds r2 ts2 es2             = concatBinds bss
        xs'                          = [d | d@(Default True _ _) <- xs]
        ys                           = [d | d@(Default _ i1 i2) <- xs', isPrivate i1 || isPrivate i2 ]
        ds1                          = Types (filter exported ke) (filter exported' te)
--        is'                          = Binds r1  (ws1 ++ is1) (filter exported es1)
        bs'                          = Binds r2 (filter exported ts2) (filter (\ eqn -> fin eqn &&  exported eqn) es2)
--        vis                          = nub (localTypes [] (rng (tsigsOf is') ++ rng (tsigsOf bs')))
        vis                          = nub (localTypes [] (rng (tsigsOf bs')))
--        (ws1,is1)                    = partition (\(n,_) -> isWitness n) (filter exported ts1)
        exported (n,_)               = isQualified n
        exported' p@(n,_)            = isQualified n && (not(isAbstract p)) --Constructors/selectors are exported
        fin (_,e)                    = isFinite e && null(filter isPrivate (constrs e))

isPrivate nm@(Name _ _ _ _)          = not(isQualified nm)
isPrivate _                          = False

isAbstract (_,DData _ _ ((c,_):_))   = isPrivate c
isAbstract (_,DRec _ _ _ ((c,_):_))  = isPrivate c
isAbstract (_,_)                     = False     -- this makes abstract types without selectors/constructors non-private...


-- Getting import info ---------------------------------------------------------
{-
chaseImports takes as input 
   - the list of import/use declaration from a module
   - a map of module names, for which the interfacde file has already been read, to IFace values
and returns import info for all (transitively) imported files in the form of
  a map from names of imported modules to triples containing
   - an indication of whether the import/use is direct or indirect (True means direct)
   - an indication of whether it is import or use (True means import)
   - the interface info
-}
type ImportInfo                      =  (Bool, Bool, IFace)

chaseImports                         :: [Syntax.Import] -> Map Name IFace -> IO (Map Name ImportInfo, Map Name IFace)
chaseImports imps ifs                = do bms <- mapM (readImport ifs) imps
                                          let newpairs = [p | (p,True) <- bms]
                                          rms <- chaseRecursively ifs (map impName imps) [] (concatMap (transImps . snd) newpairs)
                                          return (map fst bms ++ rms, [(c,ifc) | (c,(_,_,ifc)) <- newpairs])
  where readIfile ifs c              = case lookup c ifs of
                                        Just ifc -> return (ifc,False)
                                        Nothing -> do (ifc,f) <- decodeModule f
                                                      putStrLn ("[reading " ++ show f ++ "]")
                                                      return (ifc,True)
                                          where f = modToPath(str c) ++ ".ti"
        readImport ifs (Syntax.Import b c) 
                                     = do (ifc,isNew) <- readIfile ifs c
                                          return ((c,(b,True,ifc)),isNew)
        impName (Syntax.Import b c)  = c
        chaseRecursively ifs vs ms []= return ms
        chaseRecursively ifs vs ms (r : rs)
             | elem r vs             = chaseRecursively ifs vs ms rs
             | otherwise             = do (ifc,isNew)  <- readIfile ifs r
                                          chaseRecursively ifs (r : vs) ((r,(False,False,ifc)) : ms) 
                                                           ((if isNew then impsOf ifc else []) ++ rs)
                                            
transImps (_,_,ifc)                  = impsOf ifc

init_order imps                      = case topSort transImps imps of
                                         Left ms  -> errorIds "Mutually recursive modules" ms
                                         Right is -> map fst is

decodeModule f                       = (do ifc <- decodeFile f
                                           return (ifc,f)) `catch`  (\e -> do ifc <- decodeFile libf
                                                                              return (ifc,libf))
  where libf                         = Config.libDir ++ "/" ++ f


-- Building environments in which to compile the current module -----------------------------------------------
{- 
   Input to initEnvs is a map as built by chaseImports;
   output is four tuples of data suitable for various compiler passes.
-}

type Desugar1Env     = (Map Name [Name], Map Name Name, Map Name ([Name], Syntax.Type))
type RenameEnv       = (Map Name Name, Map Name Name, Map Name Name)
type CheckEnv        = ([Default Scheme], Types, [Name], Binds)
type KindleEnv       = Map Name Kindle.Decl

initEnvs             :: Map a ImportInfo -> M s (Desugar1Env, RenameEnv, CheckEnv, KindleEnv)
initEnvs bms         = do ims <- mapM (mkEnv . snd) bms
                          let (rs,xs,ss,rnL,rnT,rnE,ds,ws,bs,kds) 
                               = foldr mergeMod ([],[],[],[],[],[],Types [] [],[],Binds False [] [],[]) ims
                          return ((rs,rnL,ss),(rnL,rnT,rnE),(xs,ds,ws,bs),kds)

  where mergeMod (rs1,xs1,ss1,rnL1,rnT1,rnE1,ds1,ws1,bs1,kds1)
                 (rs2,xs2,ss2,rnL2,rnT2,rnE2,ds2,ws2,bs2,kds2) 
                                     = (rs1 ++ rs2, xs1 ++ xs2, ss1 ++ ss2, mergeRenamings2 rnL1 rnL2, 
                                       mergeRenamings2 rnT1 rnT2, mergeRenamings2 rnE1 rnE2,
                                       catDecls ds1 ds2, ws1++ws2, catBinds bs1 bs2,
                                       kds1 ++ kds2)

        mkEnv (unQual,direct,IFace ns xs rs ss ds ws bs kds)
                                     = do ks  <- renaming (dom ke)
                                          ts  <- renaming (dom te'')
                                          ls' <- renaming ls -- (concatMap snd rs)
                                          return (unMod unQual rs, xs, unMod unQual ss, unMod unQual ls',unMod unQual ks,
                                                  unMod unQual ts,ds,ws,Binds r te' es,kds)
          where Types ke ds'         = ds
                Binds r te es        = bs
                te'                  = if direct then te ++ concatMap (tenvSelCon ke) ds' else []
--                te''                 = if direct then te  ++ tsigsOf is ++ concatMap (tenvCon ke) ds' else []
                te''                 = if direct then te  ++ concatMap (tenvCon ke) ds' else []
                ls                   = [ s | (_,DRec _ _ _ cs) <- ds', (s,_) <- cs, not (isGenerated s) ]
                unMod b ps           = if b then [(tag0 (mName Nothing c),y) | (c,y) <- ps] ++ ps else ps

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
  localTypes _ (TVar t)           = internalError0 ("ChaseImports.localTypes: TVar in interface file")
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
  put (IFace a b c d e f g h) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f ->  
        get >>= \g -> get >>= \h -> return (IFace a b c d e f g h)

-- Printing -----------------------------------------------------------------------------

instance Pr IFace where
  pr (IFace ns xs rs ss ds1 ws bs kds) =
                                  text "Imported/used modules: " <+> hsep (map prId ns) $$
                                  text "Default declarations: " <+> hpr ',' xs $$
                                  -- text ("Record types and their selectors: "++show rs) $$
                                  -- text "Type synonyms: " <+> hsep (map (prId . fst) ss) $$ 
                                  text "\nType definitions\n----------------" $$ pr ds1 $$ 
--                                  text "\nCoercions and instances\n-----------------------" $$ prInsts is  $$ 
                                  text "\nTop level bindings\n------------------" $$ pr (simpVars bs) -- $$
                                  -- text "\nKindle declarations\n-------------------" $$ vcat (map pr kds)
                                  
-- prPair (n,t)                      = prId n <+> text "::" <+> pr t
   where simpVars (Binds rec te eqns) = Binds rec (map sV te) eqns
         
sV (n,t@(Scheme rh ps ke))            = case zip (filter isGenerated (idents (Scheme rh ps []))) abcSupply of
                                          [] -> (n,t)
                                          s ->  (n,subst s t) 

listIface cfg f                   = do (ifc,f) <- decodeModule f
                                       let modul = rmSuffix ".ti" f
                                           htmlfile = modul++".html"
                                       if (Config.libDir `isPrefixOf` htmlfile)
                                        then system (Config.browser cfg ++" " ++ htmlfile)
                                        else do writeFile htmlfile (render(toHTML modul (ifc :: IFace)))
                                                system (Config.browser cfg ++" " ++ htmlfile)

toHTML n (IFace ns xs rs ss ds ws bs _) = text "<html><body>\n" $$
                                          text ("<h2>API for module "++n++"</h2>\n") $$
                                          section ns "Imported modules" (hpr ',' ) $$
                                          section xs "Default declarations" (hpr ',') $$
                                          section ke' "Kind declarations" (pr . flip Types []) $$
                                          section ds' "Type declarations" (pr . Types [] . map addSubs) $$
                                          section te' "Toplevel declarations" (pr . stripTopdecls) $$
                                          text "</html>"
                      
  where section xs header f             = if null xs 
                                           then empty 
                                           else text ("<h4>"++header++"</h4>\n<pre>") $$ f xs $$ text "</pre>"
        Types ke ds'                    = ds
        ke'                             = [(n,k) | (n,k) <- ke, notElem n (dom ds')]
        Binds _ te _                    = bs
        addSubs (n,DData vs _ cs)       = (n,DData vs (map (\(_,Constr (s:_) _ _) -> s) cs1) cs2)
         where (cs1,cs2)                = partition (isGenerated . fst) cs
        addSubs (n,DRec b vs _ ss)      = (n,DRec b vs (map snd ss1) ss2)
         where (ss1, ss2)               = partition (isGenerated .fst) (map stripStar ss)
        addSubs d                       = d
        stripStar (n,Scheme rh ps ke)   = (n,Scheme rh ps (filter (( /= Star) . snd) ke))
        te'                             = map (sV . stripStar)  (filter (not . isGenerated . fst) te)
        stripTopdecls te                = bs1 ++ bs2
         where (bs1,bs2)                = partition (flip elem ws . fst ) te
                                                  
        
