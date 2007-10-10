module ChaseImports where

import Common
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

-- Data type of interface file -----------------------------------------------

data IFace = IFace { impsOf      :: [Name],                         -- imported/used modules
                     defaults    :: [Default],                      -- exported default declarations
                     recordEnv   :: Map Name [Name],                -- exported record types and their selectors,
                     tsynEnv     :: Map Name ([Name],Syntax.Type),  -- type synonyms
                     tEnv        :: Types,                          -- Core type environment
                     insts       :: Binds,                          -- types for instances
                     valEnv      :: Binds,                          -- types for exported values (including sels and cons) and some finite eqns
                     kdeclEnv    :: Kindle.Decls,                   -- Kindle form of declarations
                     ktypeEnv    :: Kindle.TEnv                     -- Kindle type environment
                   }
           deriving (Show)

-- Building interface info from data collected during compilation of a module ---------------------------------
{-
Input to ifaceMod is
      - from Desugar1: exported record types with their selectors and exported type synomyms.
      - from Termred: the entire module in Core form after term reduction
      - from Core2Kindle: declarations and type environment in Kindle form.
The function checks that the public part is closed (does not mention private data) and computes
the IFace.
-}
 
ifaceMod                   :: (Map Name [Name], Map Name ([Name], Syntax.Type)) -> Module -> (Kindle.Decls, Kindle.TEnv) -> IFace
ifaceMod (rs,ss) (Module _ ns xs ds is bs) (kds,kte)
   | not(null vis)                   = error ("Private types visible in interface: " ++ showids vis)
   | not(null ys)                    = error ("Public default declaration mentions private instance: "++ render(prDefault ys))
   | otherwise                       = IFace ns xs' rs ss ds1 is' bs' kds kte
  where Types ke te                  = ds
        Binds r1 ts1 es1             = is
        Binds r2 ts2 es2             = bs
        xs'                          = [d | d@(True,i1,i2) <- xs]
        ys                           = [d | d <- xs', localInst [(b,a) | (a,b) <- ts1] d]
        ds1                          = Types (filter exported ke) (filter exported' te)
        is'                          = Binds r1 (filter exported ts1) (filter exported es1)
        bs'                          = Binds r2 (filter exported ts2) (filter (\ eqn -> fin eqn &&  exported eqn) es2)
        vis                          = nub (localTypes [] (rng (tsigsOf is') ++ rng (tsigsOf bs')))
        exported (n,_)               = isQualified n
        exported' p@(n,_)            = isQualified n && (not(isAbstract p)) --Constructors/selectors are exported
        fin (_,e)                    = finite env0 e

localInst ts (_,i1,i2)               = isPrivate (instName i1) || isPrivate(instName i2)
  where instName (Just n,_)          = n
        instName (Nothing,t)         = fromJust (lookup t ts)

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
                                        Nothing -> do ifc <- decodeFile (modToPath(str c) ++ ".ti")
                                                      putStrLn ("[reading "++show(modToPath(str c) ++ ".ti")++"]")
                                                      return (ifc,True)
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
                                         Left ms  -> error ("Mutually recursive modules: " ++ showids ms)
                                         Right is -> map fst is


-- Building environments in which to compile the current module -----------------------------------------------
{- 
   Input to initEnvs is a map as built by chaseImports;
   output is four tuples of data suitable four various compiler passes.
-}

type Desugar1Env     = (Map Name [Name], Map Name Name, Map Name ([Name], Syntax.Type))
type RenameEnv       = (Map Name Name, Map Name Name, Map Name Name)
type CheckEnv        = ([Default], Types, Binds, Binds)
type KindleEnv       = (Map Name Kindle.Decl, Map Name Kindle.Type)

initEnvs             :: Map a ImportInfo -> M s (Desugar1Env, RenameEnv, CheckEnv, KindleEnv)
initEnvs bms         = do ims <- mapM (mkEnv . snd) bms
                          let (rs,xs,ss,rnL,rnT,rnE,ds,is,bs,kds,kte) 
                               = foldr mergeMod ([],[],[],[],[],[],Types [] [],Binds False [] [],Binds False [] [],[],[]) ims
                          return ((rs,rnL,ss),(rnL,rnT,rnE),(xs,ds,bs,is),(kds,kte))

  where mergeMod (rs1,xs1,ss1,rnL1,rnT1,rnE1,ds1,is1,bs1,kds1,kte1)
                 (rs2,xs2,ss2,rnL2,rnT2,rnE2,ds2,is2,bs2,kds2,kte2) =
                                       (rs1 ++ rs2, xs1 ++ xs2, ss1 ++ ss2, mergeRenamings2 rnL1 rnL2, 
                                        mergeRenamings2 rnT1 rnT2, mergeRenamings2 rnE1 rnE2,
                                        catDecls ds1 ds2, catBinds is1 is2, catBinds bs1 bs2,
                                        kds1 ++ kds2, kte1 ++ kte2)

        mkEnv (unQual,direct,IFace ns xs rs ss ds is bs kds kte)
                                     = do ks  <- renaming (dom ke)
                                          ts  <- renaming (dom te'')
                                          ls' <- renaming ls -- (concatMap snd rs)
                                          return (unMod unQual rs, xs, unMod unQual ss, unMod unQual ls',unMod unQual ks,
                                                  unMod unQual ts,ds,is,Binds r te' es,kds,kte)
          where Types ke ds'         = ds
                Binds r te es        = bs
                te'                  = if direct then te ++ concatMap (tenvSelCon ke) ds' else []
                te''                 = if direct then te ++ concatMap (tenvCon ke) ds' else []
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
  put (IFace a b c d e f g h i) = put a >> put b >> put c >> put d >> put e >> put f >> put g >> put h >> put i
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f ->  
        get >>= \g -> get >>= \h -> get >>= \i -> return (IFace a b c d e f g h i)

-- Printing -----------------------------------------------------------------------------

instance Pr IFace where
  pr (IFace ns xs rs ss ds1 is bs kds kte) =
                                  text "Imported/used modules: " <+> hsep (map prId ns) $$
                                  text "Default declarations: " <+> prDefault xs $$
                                  text ("Record types and their selectors: "++show rs) $$
                                  text "Type synonyms: " <+> hsep (map (prId . fst) ss) $$ 
                                  pr ds1 $$ pr is  $$ pr bs
                                  $$$ text "Kindle declarations" 
                                  $$$ vcat (map pr kds)
                                  $$$ text "Kindle type environment" $$$ vcat (map pr kte)
                                  
-- prPair (n,t)                      = prId n <+> text "::" <+> pr t


listIface f                       = do ifc <- decodeFile f
                                       putStrLn (render(pr (ifc :: IFace)))
