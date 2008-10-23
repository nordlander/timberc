module Prepare4C(prepare4c) where

import Monad
import Common
import Kindle
import PP
import qualified Core
import qualified Core2Kindle

-- Establishes that:
--   every struct function simply relays all work to a global function with an explicit "this" parameter
--   every EEnter expression has a variable as its body
--   every ENew expression occurs at the rhs of a Val binding

-- Removes the type arguments from all type constructors except Array
-- Replaces type variables with the special type constructor POLY
-- Adds type casts wherever necessary

-- Replaces type abstraction and application with BITSET parameters indicating pointer/non-pointer status

-- Replaces nullary structs with out-of-band pointers (casts from the corresponding tag value)
-- Replaces type constructor switching by switching on integer tags (embedded as pointer values or as explicit tag fields)
-- Makes field selection from matched struct variants explicit

-- Removes CBreak commands that appear directly under a CSeq (without any intervening CSwitch)
-- Removes redundant defaults in CSwitch commands

-- (Not yet):
-- Flattens the struct subtyping graph by inlining the first coercion field
-- Ditto for enums...


prepare4c e2 e3 m             = localStore (pModule e2 e3 m)


-- ===========================
-- Local environment
-- ===========================

data Env                        = Env { decls   :: Decls,                 -- all structs in scope
                                        tenv    :: TEnv,                  -- all fun and val variables in scope
                                        strinfo :: Map Name (Int,[Bool]), -- number of ptr fields + 1 / relevance flags of tvars for each struct
                                        polyenv :: Map Name (Exp,Int),    -- status location (refexp,bit-number) for type variables in scope
                                        conval  :: Map Name Int,          -- numerical values of all variant constructor names
                                        nulls   :: [Name],                -- lists all nullary struct variants
                                        tagged  :: [Name],                -- lists all struct variants that use an explicit tag field
                                        this    :: Maybe Name }

env0                            = Env { decls = [], tenv = [], strinfo = [], polyenv = [], conval = [], nulls = [], tagged = [], this = Nothing }

addDecls ds env                 = env { decls = ds ++ decls env, strinfo = info ++ strinfo env, conval = convals ++ conval env, 
                                        nulls = nullcons ++ nulls env, tagged = taggedcons ++ taggedunions ++ tagged env }
  where unioncons               = unions ds
        allvariants             = map (variants ds) unioncons
        convals                 = concat (map (`zip` [0..]) allvariants)
        nullcons                = [ n | (n, Struct _ [] (Extends _)) <- ds ]
        singlecons              = map head . filter ((==1) . length) . map (\\nullcons) $ allvariants
        taggedcons              = dom convals \\ (singlecons++nullcons)
        taggedunions            = [ n | (n,ns) <- unioncons `zip` allvariants, any (`elem` taggedcons) ns ]
        info                    = mapSnd structInfo ds
        
structInfo (Struct vs te _)     = (length (ptrFields te []), map (`elem` relevant) vs)
  where relevant                = rng (varFields te)

addTEnv te env                  = env { tenv = te ++ tenv env }

addVals te env                  = env { tenv = mapSnd ValT te ++ tenv env }

addPolyEnv vs is es env         = env { polyenv = mkPolyEnv vs is es ++ polyenv env }

-- Create a polyTag ATEnv on basis of polymorphic arity and return a mapping from each tyvar to its corresponding arg/bit
mkPolyEnv [] _ _                = []
mkPolyEnv (v:vs) (i:is) es      = (v, (es!!(i`div`32), i`mod`32)) : mkPolyEnv vs is es


setThis x env                   = env { this = Just x }

findValT xx te x                = t
  where ValT t                  = lookup'' xx te x
  
findFunT te x ts                = (vs `zip` ts, ts', t)
  where FunT vs ts' t           = lookup'' "B" te x

findStructTEnv xx env (TCon n ts)
  | isTuple n                   = (abcSupply `zip` ts, abcSupply `zip` map (ValT . tVar) (take (width n) abcSupply))
  | otherwise                   = (vs `zip` ts, te)
  where Struct vs te _          = lookup'' xx (decls env) n

findStructInfo env n
  | isTuple n                   = (width n + 1, take (width n) (repeat True))
  | otherwise                   = lookup' (strinfo env) n

findPolyTag xx env v            = lookup'' (xx ++ ": " ++ show (polyenv env)) (polyenv env) v

conLit env n                    = lInt (lookup'' "GGG" (conval env) n)

allCons env (ACon n _ _ _ : _)
  | isTuple n                   = [n]
  | otherwise                   = variants (decls env) n0
  where Struct _ _ (Extends n0) = lookup'' "Apa" (decls env) n
allCons env _                   = []

visibleArity env n              = structArity ds (structRoot ds n)
  where ds                      = decls env
  
-- =====================================
-- Replacing polymorphism with polyTags
-- =====================================
    
-- Create a list of polyTag types on basis of polymorphic arity
polyTagTypes 0                  = []
polyTagTypes a | a <= 32        = [tBITS32]
               | otherwise      = tBITS32 : polyTagTypes (a-32)


-- Create a polyTag TEnv on basis of polymorphic arity (the externally visible part)
polyTagEnv0 a | a <= 4          = [(prim GCINFO, ValT tPOLY)]
              | otherwise       = (prim GCINFO, ValT tPOLY) : 
                                  _ABCSupply `zip` map ValT (polyTagTypes a)


-- Create a polyTag TEnv on basis of polymorphic arity (the existentially quantified part)
polyTagEnv1 a                   = _abcSupply `zip` map ValT (polyTagTypes a)


-- Create a polyTag struct binding from a list of type arguments
polyTagBinds env n ts           = bs0 ++ bs1
  where mkBind (x,ValT t) e     = (x, Val t e)
        ts0                     = zipFilter vflags ts
        ts1                     = drop (visibleArity env n) ts
        l_ts0                   = length ts0
        te0                     = polyTagEnv0 l_ts0
        es0                     = polyTagArgs env ts0
        bs0 | l_ts0 <= 4        = zipWith mkBind te0 [ECall (gcInfoName n) [] (map offset es0)]
            | otherwise         = zipWith mkBind te0 (ECall (gcInfoName n) [] [] : es0)
        bs1                     = zipWith mkBind (polyTagEnv1 (length ts1)) (polyTagArgs env ts1)
        offset e                = ECall (prim IntTimes) [] [ELit (lInt (d + 2)),e]
        (d,vflags)              = findStructInfo env n


gcInfoName n@(Name s t m a)
  | isClosure n                 = Name (gcinfoSym ++ s) 0 m a
  | otherwise                   = Name (gcinfoSym ++ s) t m a
gcInfoName (Tuple n a)          = Name (gcinfoSym ++ "TUP" ++ show n) 0 Nothing a
gcInfoName (Prim p a)           = Name (gcinfoSym ++ strRep2 p) 0 Nothing a


-- Create a list of polyTag Exp arguments from a list of type arguments
polyTagArgs env []              = []
polyTagArgs env ts 
  | vars && ordered             = [ head es ]
  where vars                    = l_ts == length vs && length (nub es) == 1
        l_ts                    = length ts
        vs                      = [ n | TVar n _ <- ts ]
        (es,is)                 = unzip (map (findPolyTag "XX" env) vs)
        ordered                 = is == [0..l_ts-1]
polyTagArgs env ts              = args (length ts) ts
  where 
    args 0 []                   = []
    args a ts | a <= 32         = [arg 0 ts]
              | otherwise       = arg 0 ts0 : args (a-32) ts1
      where (ts0,ts1)           = splitAt a ts

    arg k []                    = intlit 0
    arg k (TVar n _ : ts)       = bor (arg (k+1) ts) (shift (band e (mask i)) i k)
      where (e,i)               = findPolyTag "YY" env n
    arg k (TCon (Prim p _) _ : ts)
      | p `elem` scalarPrims    = bor (mask k) (arg (k+1) ts)
    arg k (_ : ts)              = arg (k+1) ts


shift e i k | i < k             = shiftL e (ELit (lInt (k-i)))
            | i > k             = shiftR e (ELit (lInt (i-k)))
            | otherwise         = e

shiftL e1 e2                    = ECall (prim SHIFTL32) [] [e1,e2]
         
shiftR e1 e2                    = ECall (prim SHIFTR32) [] [e1,e2]

mask i                          = intlit (2^i)

bor e1 e2 | e1 == intlit 0      = e2
          | e2 == intlit 0      = e1
          | otherwise           = ECall (prim OR32) [] [e1,e2]
             
band e1 e2                      = ECall (prim AND32) [] [e1,e2]

intlit i                        = ECast tBITS32 (ELit (lInt i))


-- =============================
-- Prepare modules and types
-- =============================
    
pModule e2 dsi (Module m ns ds bs)      
                                = do -- tr (render (vcat (map pr dsi))
                                     let (_,_,_,Core.Binds _ te2 _) = e2
                                     tei <- Core2Kindle.c2kTEnv dsi te2
                                     let env1 = addTEnv (primTEnv++tei) (addDecls (primDecls++dsi) env0)
                                         env  = addTEnv (mapSnd typeOf bs) (addDecls ds env1)
                                     (bs1,bs) <- pBinds pBind env bs
                                     bs2 <- currentStore
                                     return (Module m ns (pDecls env ds) (gcinfo env ds ++ bs1 ++ bs ++ reverse bs2))


-- Prepare structs declarations
pDecls env ds                   = map f ds
  where f (n,Struct vs te _)    = (n, Struct [] (polyTagEnv0 l_vs ++ polyTagEnv1 (l_vs - a) ++ tagSig ++ mapSnd pType te) Top)
          where tagSig          = if n `elem` tagged env then [(prim Tag, ValT tInt)] else []
                l_vs            = length vs
                a               = visibleArity env n


-- gcinfo types: these must match corresponding defines in gc.c
gcSTD                           = ELit (lInt 0)
gcARRAY                         = ELit (lInt 1)
gcTUPLE                         = ELit (lInt 2)
gcBIG                           = ELit (lInt 3)
gcMUT                           = ELit (lInt 4)

-- Generate gcinfo for structs
gcinfo env ds                   = map f (prune ds (nulls env))
  where f (n,Struct vs te cs)   = (gcInfoName n, Val tPOLY (ECall (prim GCINFO) [] es))
          where es | l_vs1 <= 4 = concat [ EVar n : gcSTD : pad l_es0 (ptrFields te vs) | vs <- sampleSpaces vs1 ]
                   | otherwise  = EVar n : gcBIG : es0 ++ concat (map bitRef (varFields te)) ++ [ELit (lInt 0)]
                es0             = ptrFields te []
                l_es0           = length es0
                (d,vflags)      = findStructInfo env n
                vs1             = zipFilter vflags vs
                l_vs1           = length vs1
                idx             = vs1 `zip` [0..]
                bitRef (n,v)    = [ EVar n, ELit (lInt (i `div` 32 + 1)), ELit (lInt (i `mod` 32)) ]
                  where i       = lookup'' "DDD" idx v

pad n es                        = es ++ replicate (n - length es) (ELit (lInt 0))

ptrFields te vs                 = map EVar (dom (filter (isPtr vs) te)) ++ [ELit (lInt 0)]

sampleSpaces []                 = [[]]
sampleSpaces (v:vs)             = [ vs1 ++ vs2 | vs1 <- [[],[v]], vs2 <- sampleSpaces vs ]

isPtr vs (n,FunT _ _ _)         = False
isPtr vs (n,ValT (TVar v _))    = v `notElem` vs
isPtr vs (n,ValT (TCon k _))    = not (isScalar k)

isScalar (Prim p _)             = p `elem` scalarPrims
isScalar n                      = False

varFields te                    = [ (n,v) | (n,ValT (TVar v _)) <- te ]


-- Simplify types
pType (ValT t)                  = ValT (erase t)
pType (FunT vs ts t)            = FunT [] (polyTagTypes (length vs) ++ map erase ts) (erase t)


-- Erase polymorphism from atomic types
erase (TCon n _)                = TCon n []
erase (TVar _ _)                = tPOLY

eraseEnv te                     = mapSnd erase te


-- =============================
-- Prepare bindings and commands
-- =============================

-- Prepare bindings
pBinds f env xs                 = do (bss,xs) <- fmap unzip (mapM (f env) xs)
                                     return (concat bss, xs)


-- Prepare top-level & cmd bindings (assume code is lambda-lifted)
pBind env (x, Val t e)          = do (bs,t',e) <- pRhsExp env e 
                                     return (bs, (x, Val (erase t) (cast t t' e)))
pBind env (x, Fun vs t te c)    = do te' <- newEnv paramSym (polyTagTypes (length vs))
                                     c <- pCmd (addVals te (addPolyEnv vs [0..] (map EVar (dom te')) env)) t c
                                     return ([], (x, Fun [] (erase t) (te' ++ eraseEnv te) c))


-- Prepare struct bindings (assume code is lambda-lifted)
pSBind _ te0 env (x,Val t e)    = do (bs,e) <- pExpT env t e 
                                     return (bs, (x, Val (erase t0) (cast t0 t e)))
  where t0                      = findValT "1" te0 x
pSBind _ te0 env (x,Fun [] t te c@(CRet (ECall f [] (EThis:es))))
  | okAlready                   = return ([], (x, Fun [] t te c))
  where (_,ts0,t0)              = findFunT te0 x []
        okAlready               = t == erase t0 && rng te == map erase ts0 && es == map EVar (dom te)
pSBind ty te0 env (x,Fun vs t te c)
                                = do y <- newName thisSym
                                     te0 <- newEnv paramSym ts0
                                     te' <- newEnv paramSym (polyTagTypes (length vs))
                                     let bs0  = [ (x, Val t (cast t t0 (EVar x0))) | (x0,t0) <- te0 | (x,t) <- te ]
                                         te1  = [ if isEVar e then (x,t) else xt0 | (x, Val t e) <- bs0 | xt0 <- te0 ]
                                         bs1  = [ b | b@(_,Val _ e) <- bs0, not (isEVar e) ]
                                         te1' = te' ++ eraseEnv te1
                                         env' = addPolyEnv vs [0..] (map EVar (dom te')) (rebindPolyEnv ty y env)
                                     c <- pCmd (setThis y (addVals ((y,ty):te) env')) t0 c
                                     f <- newName functionSym
                                     addToStore (f, Fun [] t0' ((y,erase ty):te1') (cBind bs1 c))
                                     return ([], (x, Fun [] t0' te1' (CRet (ECall f [] (EThis : map EVar (dom te1'))))))
  where (_,ts0,t0)              = findFunT te0 x []
        t0'                     = erase t0


rebindPolyEnv (TCon n ts) y env = addPolyEnv vs is (map (ESel (EVar y)) _abcSupply) env
  where ts1                     = drop (visibleArity env n) ts
        (vs,is)                 = unzip [ (v,i) | (TVar v _, i) <- ts `zip` [0..] ]


-- Prepare commands
pCmd env t0 (CRet e)            = do (bs,e) <- pExpT env t0 e
                                     return (cBind bs (CRet e))
pCmd env t0 (CRun e c)          = do (bs,_,e) <- pExp env e
                                     liftM (cBind bs . CRun e) (pCmd env t0 c)
pCmd env t0 (CBind False bs c)  = do (bs1,bs) <- pBinds pBind env bs
                                     liftM (cBind bs1 . CBind False bs) (pCmd env' t0 c)
  where env'                    = addTEnv (mapSnd typeOf bs) env
pCmd env t0 (CBind True bs c)   = do (bs1,bs) <- pBinds pBind env' bs
                                     liftM (CBind True (bs1++bs)) (pCmd env' t0 c)
  where env'                    = addTEnv (mapSnd typeOf bs) env
pCmd env t0 (CUpd x e c)        = do (bs,e) <- pExpT env (findValT "2" (tenv env) x) e
                                     liftM (cBind bs . CUpd x e) (pCmd env t0 c)
pCmd env t0 (CUpdS e x e' c)    = do (bs,t1,e) <- pExp env e
                                     let (s,te) = findStructTEnv "AA" env t1
                                     (bs',e') <- pExpT env (findValT "3" te x) e'
                                     liftM (cBind bs . cBind bs' . CUpdS e x e') (pCmd env t0 c)
pCmd env t0 (CUpdA e i e' c)    = do (bs,TCon (Prim Array _) [t],e) <- pExp env e
                                     (bs',i) <- pExpT env tInt i
                                     (bs'',e') <- pExpT env tPOLY e'
                                     liftM (cBind bs . cBind bs' . cBind bs'' . CUpdA e i e') (pCmd env t0 c)
pCmd env t0 (CSwitch e alts)
  | any litA alts               = do (bs,e) <- pExpT env tInt e
                                     alts <- mapM (pAlt env e tInt t0) alts
                                     return (cBind bs (CSwitch e alts))
  | otherwise                   = do (bs,t,e) <- pExp env e
                                     alts <- mapM (pAlt env e t t0) alts
                                     let (alts0,alts1) = partition nullA [ a | a@(ACon _ _ _ _) <- alts ]
                                         altsW         = [ a | a@(AWild _) <- alts ]
                                     return (cBind bs (mkSwitch env e (alts0++absent0 altsW) (alts1++absent1 altsW)))
  where nullA (ACon k _ _ _)    = k `elem` nulls env
        nullA _                 = False
        absent                  = allCons env alts \\ [ k | ACon k _ _ _ <- alts ]
        (abs0,abs1)             = partition (`elem` nulls env) absent
        absent0 altsW           = [ ACon k [] [] d | k <- abs0, AWild d <- altsW ]
        absent1 altsW           = [ a | a <- altsW, not (null abs1) ]
        litA (ALit _ _)         = True
        litA _                  = False
pCmd env t0 (CSeq c c')         = liftM2 mkSeq (pCmd env t0 c) (pCmd env t0 c')
pCmd env t0 (CBreak)            = return CBreak
pCmd env t0 (CRaise e)          = do (bs,e) <- pExpT env tInt e
                                     return (cBind bs (CRaise e))
pCmd env t0 (CWhile e c c')     = do (bs,e) <- pExpT env tBool e
                                     c <- pCmd env t0 c
                                     liftM (cBind bs . CWhile e c) (pCmd env t0 c')
pCmd env t0 (CCont)             = return CCont


mkSwitch env e [] [ACon n _ _ c]
  | n `notElem` tagged env      = c
mkSwitch env e [] [AWild c]     = c
mkSwitch env e [] alts1         = CSwitch (ESel e (prim Tag)) (map (mkLitAlt env) alts1)
mkSwitch env e alts0@[ACon n _ _ c] []
  | allCons env alts0 == [n]    = c
mkSwitch env e alts0 []         = CSwitch (ECast tInt e) (map (mkLitAlt env) alts0)
mkSwitch env e alts0 alts1      = mkSwitch env e (alts0++[AWild d]) []
  where d                       = mkSwitch env e [] alts1


mkLitAlt env (ACon n [] [] c)   = ALit (conLit env n) c
mkLitAlt env a                  = a


-- Prepare switch alternatives
pAlt env _ _ t0 (AWild c)       = liftM AWild (pCmd env t0 c)
pAlt env _ _ t0 (ALit l c)      = liftM (ALit l) (pCmd env t0 c)
pAlt env e (TCon _ ts) t0 (ACon k vs te c)
                                = do te' <- newEnv paramSym (polyTagTypes (length vs))
                                     c <- pCmd (addPolyEnv vs [0..] (map EVar (dom te')) env) t0 (cBind (bs0 te' ++ bs1) c)
                                     return (ACon k [] [] c)
  where bs0 te                  = zipWith mkBind te _abcSupply
        bs1                     = filter (not . isDummy . fst) (zipWith mkBind te abcSupply)
        mkBind (x,t) y          = (x, Val t (ESel (ECast (TCon k (ts ++ map tVar vs)) e) y))
        
        

mkSeq c1 c2                     = case anchor c1 of
                                    (bf,CBreak) -> bf c2
                                    (bf,CCont)  -> c1
                                    _           -> CSeq c1 c2
  where anchor (CBind r bs c)   = (CBind r bs . bf, c')
          where (bf,c')         = anchor c
        anchor (CRun e c)       = (CRun e . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpd x e c)     = (CUpd x e . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpdS e x e' c) = (CUpdS e x e' . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpdA e i e' c) = (CUpdA e i e' . bf, c')
          where (bf,c')         = anchor c
        anchor c                = (id, c)



-- =============================
-- Prepare expressions
-- =============================

-- Prepare a right-hand-side expression
pRhsExp env (ENew n ts bs)      = pNewExp env n ts bs
pRhsExp env (ECast t (ENew n ts bs))
                                = do (bs',t',e) <- pNewExp env n ts bs
                                     return (bs', t, cast t t' e)
pRhsExp env e                   = pExp env e


pNewExp env n ts bs
  | n `elem` nulls env          = return ([], t0, cast t0 tInt (ELit (conLit env n)))
  | otherwise                   = do (bs1,bs) <- pBinds (pSBind t0 te0) env bs
                                     return (bs1, t0, ENew n [] (bs''++bs'++bs))
  where bs'                     = if n `elem` tagged env then [(prim Tag, Val tInt (ELit (conLit env n)))] else []
        bs''                    = polyTagBinds env n ts
        t0                      = TCon n ts
        (_,te0)                 = findStructTEnv "BB" env t0


pRefBind te0 env (x,Val _ e)    = do (bs,t,e) <- pRhsExp env e 
                                     return (bs, (x, Val (erase t0) (cast t0 t e)))
  where t0                      = findValT "1" te0 x


-- Prepare an expression in an arbitrary position and match its type with the expected one
pExpT env t0 e                  = do (bs,t,e) <- pExp env e
                                     return (bs, cast t0 t e)


cast t0 t1 e
  | u0 == u1                    = e
  | u0 == tPOLY && smallPrim u1 = ECast tPOLY (ECast tInt e)
  | smallPrim u0 && u1 == tPOLY = ECast u0 (ECast tInt e)
  | u0 == tPOLY && u1 == tFloat = ECall (prim Float2POLY) [] [e]
  | u0 == tFloat && u1 == tPOLY = ECall (prim POLY2Float) [] [e]
  | otherwise                   = ECast u0 e
  where u0                      = erase t0
        u1                      = erase t1

smallPrim (TCon (Prim p _) _)   = p `elem` smallPrims
smallPrim _                     = False


pExpTs env [] []                = return ([], [])
pExpTs env (t:ts) (e:es)        = do (bs1,e) <- pExpT env t e
                                     (bs2,es) <- pExpTs env ts es
                                     return (bs1++bs2, e:es)


-- Prepare an expression in an arbitrary position and compute its type
pExp env (EVar x)                   = return ([], findValT "4" (tenv env) x, EVar x)
pExp env (ELit l)                   = return ([], litType l, ELit l)
pExp env (EThis)                    = return ([], findValT "5" (tenv env) x, EVar x)
  where x                           = fromJust (this env)
pExp env (ESel e l)                 = do (bs,t1,e) <- pExp env e
                                         let (s,te) = findStructTEnv "CC" env t1
                                             t = findValT ("6" ++ " e: " ++ render (pr e) ++ "  te: " ++ show te) te l
                                         specialize s t bs (ESel e l)
pExp env (ECall f ts es)            = do (bs,es) <- pExpTs env ts0 es
                                         specialize s t bs (ECall f [] (polyTagArgs env ts ++ es))
  where (s,ts0,t)                   = findFunT (tenv env) f ts
pExp env (EEnter (EVar x) f ts es)  = do let t1 = findValT "7" (tenv env) x
                                         let (s,te) = findStructTEnv "DD" env t1
                                             (s',ts0,t) = findFunT te f ts
                                         (bs2,es) <- pExpTs env ts0 es
                                         specialize (s'@@s) t bs2 (EEnter (EVar x) f [] (polyTagArgs env ts ++ es))
pExp env (EEnter e f ts es)         = do (bs1,t1,e) <- pRhsExp env e
                                         let (s,te) = findStructTEnv "EE" env t1
                                             (s',ts0,t) = findFunT te f ts
                                         (bs2,es) <- pExpTs env ts0 es
                                         x <- newName tempSym
                                         specialize (s'@@s) t (bs1++bs2++[(x, Val (erase t1) e)]) (EEnter (EVar x) f [] (polyTagArgs env ts ++ es))
pExp env (ECast t e)                = do (bs,t',e) <- pExp env e
                                         return (bs, t, cast t t' e)
pExp env (ENew n ts bs)
  | n `elem` nulls env              = return ([], tInt, ELit (conLit env n))
  | otherwise                       = do (bs1,t,e) <- pNewExp env n ts bs
                                         x <- newName tempSym
                                         return (bs1++[(x, Val (erase t) e)], t, EVar x)

specialize s t bs e                 = return (bs, t', cast t' t e)
  where t'                          = subst s t
