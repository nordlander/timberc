{-# LANGUAGE FlexibleInstances, PatternGuards #-}

-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Core2Kindle(core2kindle, c2kTEnv) where
{- -}
import Control.Monad
import Common
import Core
import Name
import PP
import qualified Decls
import qualified Env
import qualified Kindle


-- =========================================================================================
-- Translation of Core modules into back-end Kindle format
-- =========================================================================================

core2kindle e2 e3 m                 = localStore (cModule e2 e3 m)


-- =========================================================================================
-- The FType format
-- =========================================================================================
    
-- This datatype is used to distinguish the types of known functions that can be called directly,
-- from those that are anonymous and thus must be translated as closures.  Furthermore, the type
-- arguments of FType constructors are supposed to be on a semi-Kindle form, where several primitive
-- type constructors have been replaced by their corresponding Kindle implementations.  These types
-- are still expressed using Core.Type syntax during the translation process, though, to facilitate
-- tracking of instantiation of type variables by means of unification.

data Result a                       = ValR a
                                    | FunR ([Kindle.Exp] -> a) [Kindle.AType]

instance Show a => Show (Result a) where
    show (ValR a)                   = "ValR (" ++ show a ++ ")"
    show (FunR f ts)                = "FunR _ " ++ show ts

rcomp f (ValR c)                    = ValR (f c)
rcomp f (FunR g ts)                 = FunR (f . g) ts

instance Pr a => Pr (Result a) where
    pr (ValR a)			    = text "ValR" <+> pr a
    pr (FunR f ts)		    = text "FunR" <+> pr (f (take (length ts) (repeat (Kindle.EVar (name0 "?")))))

-- =========================================================================================
-- Translation environment
-- =========================================================================================

data Env                            = Env { mname   :: Maybe String,
                                            decls   :: Kindle.Decls,
                                            tenv    :: Kindle.TEnv,
                                            selfN   :: Maybe Name,
                                            tArgs   :: [Kindle.AType] }


env0                                = Env { mname   = Nothing,
                                            decls   = [],
                                            tenv    = [],
                                            selfN   = Nothing,
                                            tArgs   = [] }

setMName m env                      = env { mname = Just (str m) }

addDecls ds env                     = env { decls = ds ++ decls env }

addTEnv te env                      = env { tenv = te ++ tenv env }

addATEnv te env                     = env { tenv = mapSnd Kindle.ValT te ++ tenv env }

pushSelf x env                      = env { selfN = Just x }

self env                            = fromJust (selfN env)

setTArgs env ts                     = env { tArgs = ts }

findDecl env k
  | isTuple k                       = Kindle.tupleDecl k
  | otherwise                       = lookup' (decls env) k
  

splitClosureType 0 t0               = ([], t0)
splitClosureType n t0
  | n' >= 0                         = (ts:tss, t1)
  | otherwise                       = ([], t0)
  where Kindle.TClos [] ts t        = t0
        n'                          = n - length ts
        (tss,t1)                    = splitClosureType n' t
                                             
                                         
-- =========================================================================================
-- Translation entry point
-- =========================================================================================

-- Translate a Core.Module into a Kindle.Module
cModule e2 e3 (Module m ns xs es ds ws bss)
                                    = do bs  <- cBindsList (addTEnv te1 env1) bss
                                         ds2 <- currentStore
                                         let (dsQ,dsNQ) = partition (isPublic . fst) (reverse ds2)
                                             dsThis = ds1 ++ filter (isQual m . fst) dsQ
                                             ds3 = dsThis ++ dsNQ
                                         --let fromCurrent n = not (isQualified n)|| isQual m n
                                         --    ds3 = ds1++reverse (filter (fromCurrent . fst) ds2)
                                         return (Kindle.Module m (map snd ns) te1 ds3 bs,dsThis)
  where env                         = addDecls e3 (setMName m (addTEnv Kindle.primTEnv (addDecls Kindle.primDecls env0)))
        ds1                         = cDecls ds
        env1                        = addTEnv (tenvImp e2) (addDecls ds1 env)
        te1                         = cTEnv (extsMap es)

-- Compute the imported type environment
tenvImp (Module _ _ _ es _ _ [bs])  = cTEnv (tsigsOf bs ++ extsMap es)


-- =========================================================================================
-- Translating Core type declarations into Kindle.Decls
-- =========================================================================================

cDecls (Types ke ds)                = concat (map cDecl ds)
  where cDecl (n,DRec _ vs [] ss)   = [(n, Kindle.Struct vs (cTEnv ss) Kindle.Top)]
        cDecl (n,DType vs t)        = []
        cDecl (n,DData vs [] cs)    = (n, Kindle.Struct vs [] Kindle.Union) : map (cCon (Kindle.TCon n (map Kindle.tVar vs)) vs) cs

        cCon t0 vs (c,Constr ts ps ke)
                                    = (injectCon c, Kindle.Struct (vs++vs') (cValTEnv te) (Kindle.Extends t0 vs'))
          where te                  = Kindle.conSels c `zip` (ps++ts)
                vs'                 = dom ke


injectCon (Name s t m a)            = Name ('_':s) t m a
injectCon n                         = n


-- =========================================================================================
-- Translating schemes and types into Kindle form
-- =========================================================================================

-- Translate a Core.Scheme into an Kindle.Type on basis of arity and polymorphism
cScheme (Scheme rh ps ke)               = case (dom ke, ts' ++ ts) of           -- concatenate witness params with ts
                                               ([],[]) -> Kindle.ValT t
                                               (vs,ts) -> Kindle.FunT vs ts t
  where (ts,t)                          = cRho rh
        ts'                             = map cAScheme ps
        

-- Translate a Core.Rho type
cRho (F scs rh)                         = (map cAScheme scs ++ ts, t)           -- concatenate scs with monadic params
  where (ts,t)                          = cRho' rh
cRho (R t)                              = cType t


-- Translate a Core.Rho type but treat function types as closures
cRho' (R t)                             = cType' (tFlat t)
cRho' rh                                = ([], cAScheme (Scheme rh [] []))


-- Translate a Core.Type
cType (TFun ts t)                       = (map cAType ts ++ ts', t')            -- concatenate ts with monadic params
  where (ts',t')                        = cType' (tFlat t)
cType t                                 = cType' (tFlat t)


-- Translate a Core.Type but treat function types as closures
cType' (TId (Prim Action _), [])        = ([Kindle.tTime,Kindle.tTime], Kindle.tMsg)
cType' (TId (Prim Request _), [t])      = ([Kindle.tInt], cAType t)
cType' (TId (Prim Class _), [t])        = ([Kindle.tInt], cAType t)
cType' (TId (Prim Cmd _), [s,t])        = ([Kindle.tRef (cAType s)], cAType t)
cType' (TId (Prim PMC _), [t])          = ([], cAType t)
cType' (TId n, ts) | isVar n            = ([], Kindle.TVar n (map cAType ts))
cType' (TId n, ts)                      = ([], Kindle.TCon n (map cAType ts))
cType' (Tvar n, [])                     = ([], Kindle.tInt) -- arbitrary choice, e.g. the type of
                                                            -- the list in "fst (False,[])"
cType' (Tvar n, ts)                     = ([], Kindle.TCon (tuple (length ts)) (map cAType ts))
cType' (t, _)                           = ([], cAType t)


-- Translate a Core.Type unconditionally into a Kindle.AType
cAType t0
  | null ts                             = t
  | otherwise                           = Kindle.TClos [] ts t
  where (ts, t)                         = cType t0


-- Translate a Core.Scheme unconditionally into a Kindle.AType
cAScheme sc                             = case cScheme sc of
                                               Kindle.ValT t       -> t
                                               Kindle.FunT vs ts t -> Kindle.TClos vs ts t


-- Translate a Core.Scheme unconditionally into a ValT variant of a Kindle.Type
cValScheme sc                           = Kindle.ValT (cAScheme sc)


-- Translate a Core.TEnv into an Kindle.TEnv
cTEnv'					:: [Name] -> TEnv -> Kindle.TEnv
cTEnv' forceVal te                      = map cSig te
  where cSig (x,sc) | x `elem` forceVal	= (x, cValScheme sc)
	   	    | otherwise         = (x, cScheme sc)
       

cTEnv					= cTEnv' []


-- Translate a Core.TEnv into an Kindle.ATEnv
cATEnv te                               = xs `zip` map cAScheme scs
  where (xs,scs)                        = unzip te
  
    
-- Translate a Core.TEnv into an Kindle.TEnv with only ValT types
cValTEnv te                             = xs `zip` map cValScheme scs
  where (xs,scs)                        = unzip te


-- =========================================================================================
-- Translating bindings
-- =========================================================================================

-- Translate a list of strongly connected Core binding groups into a list of Kindle bindings
cBindsList env []                       = return []
cBindsList env (bs:bss)                 = do (te,bf) <- cBinds env (dom (tsigsOf bs)) bs
                                             bs <- cBindsList (addTEnv te env) bss
                                             return (Kindle.flatBinds bf ++ bs)


-- Translate a list of (mutually recursive) Core bindings into a Kindle.CBind on basis of declared type
cBinds env called (Binds r te eqs)
  | not r || all Kindle.okRec (rng te') = do bf <- cEqs (addTEnv te' env) te' eqs
					     return (te', comb r bf)
  | otherwise                           = errorIds "Illegal value recursion" (dom te')
  where comb False bf                   = bf
        comb True bf                    = Kindle.CBind True (Kindle.flatBinds bf)
        te'                             = cTEnv' forceVal te
--        te'                             = cTEnv te
	forceVal			= dom eqs \\ called


-- Translate a list of Core equations into a list of Kindle bindings on basis of declared type
cEqs env te []				= return id
cEqs env te (eq:eqs)			= do (bf,b) <- cEq env te eq
					     bf' <- cEqs env te eqs
					     return (bf . Kindle.cBind [b] . bf')
--cEqs env te eqs                         = do (bfs,bs) <- fmap unzip (mapM (cEq env te) eqs)
--                                             return (foldr (.) id bfs, bs)

cEq env te (x,e)                        = case lookup' te x of
                                            Kindle.ValT t0 -> do
                                                (bf,e) <- cValExpT env t0 e
                                                return (bf, (x, Kindle.Val t0 e))
                                            Kindle.FunT vs0 ts0 t0 -> do
                                                (bf,vs,te,t,c) <- cFunT env vs0 ts0 t0 e
                                                return (bf, (x, Kindle.Fun vs t te c))


-- Translate a Core.Exp into a Kindle command, a return type, a type abstraction, and an argument list of given length
cFunT env vs0 ts0 t0 e                  = do (vs,te,t,c) <- cFun0 env e
                                             let s = vs0 `zip` map Kindle.tVar vs
                                             cFunT' env vs (subst s ts0) (subst s t0) te t c

cFunT' env [] ss s [] t (Kindle.CRet e) = do x <- newName tempSym
                                             (te',t',c') <- adaptFun ss s [] t (Kindle.CRet (Kindle.EVar x))
                                             return (Kindle.cBind [(x,Kindle.Val t e)], [], te', t', c')
cFunT' env [] ss s [] t c               = do f <- newName functionSym
                                             (bf,vs,te',t',c') <- cFunT' env [] ss s [] t (Kindle.CRet (Kindle.ECall f [] []))
                                             return (Kindle.cBind [(f,Kindle.Fun [] t [] c)] . bf, vs, te', t', c')
cFunT' env vs ss s te t c               = do (te',t',c') <- adaptFun ss s te t c
                                             return (id, vs, te', t', c')


-- Translate a Core.Exp with a known type into a Kindle.Exp
cValExpT env s e                        = do (bf,t,e') <- cValExp env e
					     f <- adaptVal s t
                                             return (bf, f e')

cValExpTs env [] []                     = return (id, [])
cValExpTs env (s:ss) (e:es)             = do (bf,e) <- cValExpT env s e
                                             (bf',es) <- cValExpTs env ss es
                                             return (bf . bf', e:es)

-- =============================================
-- Adapting arities
-- =============================================

adaptVars ts expected_te		= do f <- adaptVals expected_ts ts
					     return (f (map Kindle.EVar xs))
  where (xs,expected_ts)		= unzip expected_te


adaptEnv [] []                          = return (id, [])
adaptEnv (expected_t:expected_ts) ((x,t):te)
  | expected_t == t                     = do (bf,adapted_te) <- adaptEnv expected_ts te
                                             return (bf, (x,expected_t):adapted_te)
  | otherwise                           = do y <- newName tempSym
                                             f <- adaptVal t expected_t
                                             (bf,adapted_te) <- adaptEnv expected_ts te
                                             return (Kindle.cBind [(x,Kindle.Val t (f (Kindle.EVar y)))] . bf, (y,expected_t):adapted_te)


-- Adapt the arity structure of a single closure value
adaptVal (Kindle.TClos exp_vs exp_ts exp_t) (Kindle.TClos vs ts t)
					= do exp_te <- newEnv paramSym exp_ts
					     f <- adaptVal' exp_te exp_t (subst s ts) (subst s t)
					     return (\e -> Kindle.EClos exp_vs exp_t exp_te (Kindle.CRet (f e)))
  where s				= vs `zip` map Kindle.tVar exp_vs
adaptVal expected_t t	  		= adaptVal' [] expected_t [] t


adaptVal' [] expected_t [] t
  | expected_t == t                     = return id                                                                         -- A
adaptVal' expected_te expected_t [] t   = adaptVal' expected_te expected_t ts t'                                            -- B
  where Kindle.TClos [] ts t'           = t
adaptVal' expected_te expected_t ts t
  | l_expected >= l_ts         		= do -- higher-arity closure expected, or arity is correct
					     let (expected_te1,expected_te2) = splitAt l_ts expected_te
					     es <- adaptVars ts expected_te1
                                             f <- adaptVal' expected_te2 expected_t [] t
                                             return (\e -> f (Kindle.enter e [] es))                                        -- C
  | otherwise                           = do -- lower arity closure expected
					     let Kindle.TClos [] expected_ts expected_t' = expected_t
					     expected_te' <- newEnv paramSym expected_ts
                                             f <- adaptVal' (expected_te++expected_te') expected_t' ts t
                                             return (\e -> Kindle.EClos [] expected_t' expected_te' (Kindle.CRet (f e)))     -- D
  where l_ts                            = length ts
	l_expected			= length expected_te


-- Adapt the arity structure of a command result
adaptCmd expected_t t			= adaptCmd' [] expected_t [] t

adaptCmd' [] expected_t [] t
  | expected_t == t			= return Kindle.CRet
adaptCmd' expected_te expected_t [] t	= adaptCmd' expected_te expected_t ts t'
  where Kindle.TClos [] ts t'		= t
adaptCmd' expected_te expected_t ts t
  | l_expected >= l_ts			= do -- higher-arity expected, or arity is correct
					     let (expected_te1,expected_te2) = splitAt l_ts expected_te
					     es <- adaptVars ts expected_te1
					     f <- adaptCmd' expected_te2 expected_t [] t
					     return (Kindle.cmap' f . \e -> Kindle.enter' e [] es)
  | otherwise				= do -- lower arity expected
					     let Kindle.TClos [] expected_ts expected_t' = expected_t
					     expected_te' <- newEnv paramSym expected_ts
					     f <- adaptCmd' (expected_te++expected_te') expected_t' ts t
					     return (Kindle.CRet . Kindle.EClos [] expected_t' expected_te' . f)
  where l_ts				= length ts
	l_expected			= length expected_te


-- Adapt a list of values
adaptVals [] []                     	= return id
adaptVals (expected_t:expected_ts) (t:ts)     
					= do f <- adaptVal expected_t t
                                             g <- adaptVals expected_ts ts
                                             return (\(e:es) -> f e : g es)

-- Adapt a function
adaptFun expected_ts expected_t te t c
  | expected_t:expected_ts == t:rng te  = return (te, t, c)
  | l_expected >= l_te                  = do -- higher arity expected, or arity is correct
					     let (tss,t') = splitClosureType (l_expected - l_te) t
					     te' <- newEnv paramSym (concat tss)
                                             (bf,adapted_te) <- adaptEnv expected_ts (te++te')
                                             f <- adaptCmd expected_t t'
                                             return (adapted_te, expected_t, bf (Kindle.cmap' f (Kindle.cmap' (Kindle.multiEnter tss (map Kindle.EVar (dom te'))) c)))
  | otherwise {- l_expected < l_te -}	= do -- lower arity expected
					     let (te1,te2) = splitAt l_expected te
                                                 t1 = Kindle.TClos [] (rng te2) t
                                             adaptFun expected_ts expected_t te1 t1 (Kindle.CRet (Kindle.EClos [] t te2 c))
  where l_expected                      = length expected_ts
        l_te                            = length te


-- Adapt a Result term
adaptR (expected_t,ValR _) (t,ValR c)
  | expected_t == t                     = return (expected_t, ValR c)
  | otherwise                           = do f <- adaptCmd expected_t t
                                             return (expected_t, ValR (Kindle.cmap' f c))
adaptR (expected_t,FunR _ expected_ts) (t, FunR f ts)
  | expected_t:expected_ts == t:ts      = return (expected_t, FunR f expected_ts)
  | l_expected >= l_ts                  = do -- higher-arity expected, or arity is correct
					     let (tss,t') = splitClosureType (l_expected - l_ts) t
					     g <- adaptVals (ts ++ concat tss) expected_ts
                                             h <- adaptCmd expected_t t'
                                             return (expected_t, FunR (sat g h tss) expected_ts)
  where l_expected                      = length expected_ts
        l_ts                            = length ts
        sat g h tss es                  = let (es1,es2) = splitAt l_ts (g es)
                                          in Kindle.cmap' h (Kindle.cmap' (Kindle.multiEnter tss es2) (f es1))
adaptR (expected_t,FunR _ expected_ts) (t,ValR c) 
					= do let (tss,t') = splitClosureType (length expected_ts) t
					     g <- adaptVals (concat tss) expected_ts
                                             h <- adaptCmd expected_t t'
                                             return (expected_t, FunR (sat g h tss) expected_ts)
  where sat g h tss es                  = Kindle.cmap' h (Kindle.cmap' (Kindle.multiEnter tss (g es)) c)




-- =========================================================================================
-- Translating abstractions
-- =========================================================================================

-- Convert a Core.Exp into a type abstraction, a parameter list and a Kindle.Cmd
cFun0 env (ELet bs e)
  | isTAbsEncoding bs                   = do (te,t,c) <- cFun env e
                                             return (tAbsVars bs, te, t, c)
cFun0 env e                             = do (te,t,c) <- cFun env e
                                             return ([], te, t, c)
                                             

-- Convert a Core.Exp into a parameter list and a Kindle.Cmd
cFun env (ELam te0 e)                   = do (te',t,c) <- cFun (addATEnv te env) e
                                             return (te ++ te', t, c)
  where te                              = cATEnv te0
cFun env (EReq e (EDo x tx0 c))         = do (bf,e) <- cValExpT env tx e
                                             (t,c) <- cCmd (pushSelf x (addATEnv [(x,tx)] env)) c
                                             let bf' = Kindle.cBind [(x,Kindle.Val tx (Kindle.lock tx e))]
                                             y <- newName dummySym
                                             c' <- Kindle.unlock x t c
                                             return ([(y,Kindle.tInt)], t, bf (bf' c'))
  where tx                              = Kindle.tRef (cAType tx0)
cFun env (EReq e e')                    = do (bf,tx,e) <- cValExp env e
                                             x <- newName selfSym
                                             (t,c) <- cCmdExp (pushSelf x (addATEnv [(x,tx)] env)) e'
                                             y <- newName dummySym
                                             let bf' = Kindle.cBind [(x,Kindle.Val tx (Kindle.lock tx e))]
					     c' <- Kindle.unlock x t c
                                             return  ([(y,Kindle.tInt)], t, bf (bf' c'))
cFun env e@(EAct _ _)                   = cAct env id id e
cFun env e@(EAp e0 _) 
  | isPrim After e0 || isPrim Before e0 = cAct env id id e
cFun env (ETempl x tx0 te0 c)           = do (t,c) <- cCmd (pushSelf x (addATEnv [(x,Kindle.tRef tx)] (addTEnv te env))) c
                                             store <- currentStore
                                             if n `elem` dom store then return () else
                                                addToStore (n, Kindle.Struct vs te Kindle.Top)
                                             y <- newName dummySym
                                             let e = Kindle.ENew (prim Ref) [tx] [(prim STATE, Kindle.Val tx (Kindle.ENew n ts []))]
                                             return ([(y,Kindle.tInt)], t, Kindle.cBind [(x,Kindle.Val (Kindle.tRef tx) e)] c)
  where tx@(Kindle.TCon n [])           = cAType tx0  -- Type-checker guarantees tx is a struct type name
        te                              = cValTEnv te0
        vs                              = nub (tyvars te)
        ts                              = map Kindle.tVar vs
cFun env (EDo x tx0 c)                  = do (t,c) <- cCmd (pushSelf x (addATEnv [(x,tx)] env)) c
                                             return ([(x,tx)], t, c)
  where tx                              = Kindle.tRef (cAType tx0)
cFun env e                              = do (t,r) <- cBody env e
                                             case r of
                                               FunR f ts -> do xs <- newNames paramSym (length ts)
                                                               return (xs `zip` ts, t, f (map Kindle.EVar xs))
                                               ValR c    -> return ([], t, c)


-- Translate an action expression into a Core.Cmd
cAct env fa fb (EAp e0 [e,e'])
  | isPrim After e0                     = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (te,t,c) <- cAct env (const e1) fb e'
                                             return (te, t, bf c)
  | isPrim Before e0                    = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (te,t,c) <- cAct env fa (const e1) e'
                                             return (te, t, bf c)
cAct env fa fb (EAct e e')              = do (bf,tx,e) <- cValExp env e
                                             (te,t,c) <- cFun env e'
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             m  <- newName tempSym
                                             let bs' = [(prim Code, Kindle.Fun [] t te c), (prim Obj, Kindle.Val tx e)]
                                                 bs  = [(m, Kindle.Val Kindle.tMsg (Kindle.ENew (prim Msg) [] bs'))]
                                                 e1  = Kindle.ECall (prim ASYNC) [] [Kindle.EVar m, fa (Kindle.EVar a), fb (Kindle.EVar b)]
                                                 c1  = Kindle.cBind bs (Kindle.CRet e1)
                                             return ([(a,Kindle.tTime),(b,Kindle.tTime)], Kindle.tMsg, c1)
cAct env fa fb e                        = do (bf,t0,f,[ta,tb]) <- cFunExp env e
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             let c = bf (Kindle.CRet (f [fa (Kindle.EVar a), fb (Kindle.EVar b)]))
                                             return ([(a,Kindle.tTime), (b,Kindle.tTime)], Kindle.tMsg, c)


-- =========================================================================================
-- Translating let- and case expressions
-- =========================================================================================

-- Translate a Core (Pat,Exp) pair into a Kindle.Alt result
cAlt cBdy env (Alt (PLit l) e)          = do (t1,r) <- cBdy env e
                                             return (t1, rcomp (Kindle.ALit l) r)
cAlt cBdy env (Alt PWild e)             = do (t1,r) <- cBdy env e
                                             return (t1, rcomp Kindle.AWild r)
cAlt cBdy env (Alt (PCon k te) e)       = do (vs,te,t,r) <- cRhs0 cBdy env (length te0) (eLam te e)
                                             return (t, rcomp (Kindle.ACon (injectCon k) vs te) r)
  where Kindle.Struct _ te0 _           = findDecl env k


-- Translate a Core right-hand-side into a Kindle.Cmd result, a binding, and a type abstraction
cRhs0 cBdy env n (ELet bs e)
  | isTAbsEncoding bs                   = do (te,t,r) <- cRhs cBdy env n [] e
                                             return (tAbsVars bs, te, t, r)
cRhs0 cBdy env n e                      = do (te,t,r) <- cRhs cBdy env n [] e
                                             return ([], te, t, r)
                                             

-- Translate a Core right-hand-side into a Kindle.Cmd result and a binding
cRhs cBdy env n te (ELam te0 e)         = cRhs cBdy (addATEnv te1 env) n (te++te1) e
  where te1                             = cATEnv te0
cRhs cBdy env n te e
  | n == l_te                           = do (t,r) <- cBdy env e
                                             return (te, t, r)
  | n < l_te                            = do (t,r) <- cBdy env e
                                             let t' = Kindle.TClos [] (rng te2) t
                                             return (te1, t', rcomp (Kindle.CRet . Kindle.EClos [] t te2) r)
  | n > l_te                            = do (t,r) <- cBdy env e
                                             let (tss,t') = splitClosureType (n - l_te) t
                                             te' <- newEnv paramSym (concat tss)
                                             let f = Kindle.multiEnter tss (map Kindle.EVar (dom te'))
                                             return (te++te', t', rcomp (Kindle.cmap' f) r)
  where l_te                            = length te
        (te1,te2)                       = splitAt n te


-- Translate a Core.Exp into a Kindle.Cmd result
cBody env (ELet bs e)
  | not (isEncoding bs)                 = do (te,bf) <- cBinds env (entered e) bs
					     (t,r) <- cBody (addTEnv te env) e
					     return (t, rcomp bf r)
cBody env (ECase e alts)                = cCase cBody env e alts
cBody env (EAp e0 [e]) 
  | isPrim Match e0                     = do (t,r) <- cPMC cBody cExpFail env e
                                             return (t, rcomp (\c -> Kindle.CSeq c (Kindle.CRaise (Kindle.ELit (lInt 1)))) r)
cBody env e                             = do (bf,t,h) <- cExp env e
                                             case h of
                                               ValR e    -> return (t, ValR (bf (Kindle.CRet e)))
                                               FunR f ts -> return (t, FunR (bf . Kindle.CRet . f) ts)


-- Note: we don't really handle PMC terms as first class citizens, rather like constructors in a small grammar
-- of pattern-matching expressions:
-- e  ::=  ...  |  Match pm
-- pm ::=  Commit e  |  Fail  |  Fatbar pm pm  |  case e of {p -> pm} pm  |  let bs in pm
-- This syntax is followed when PMC terms are introduced in module Match, and is also respected by Termred.
--
-- However, should we for some reason want to allow abstraction over PMC terms, as in (\e -> Commit e),
-- the translation below will need to be complemented with a concrete implementation of the PMC type constructor
-- (using Maybe, for example), and corresponding general implementations of Match, Commit, Fail & Fatbar.


-- Translate a Core.Exp corresponding to a PMC term into a Kindle.Cmd result
cPMC cE cF env (ELet bs e)
  | not (isEncoding bs)                 = do (te,bf) <- cBinds env (entered e) bs
                                             (t,r) <- cPMC cE cF (addTEnv te env) e
                                             return (t, rcomp bf r)
cPMC cE cF env (ECase e alts)           = cCase (cPMC cE cF) env e alts
cPMC cE cF env (EAp e0 [e1,e2])
  | isPrim Fatbar e0                    = do r1 <- cPMC cE cF env e1
                                             r2 <- cPMC cE cF env e2
                                             let r = maxR [r1, r2]
                                             r1 <- adaptR r r1
                                             r2 <- adaptR r r2
                                             return (mkSeq r1 r2)
cPMC cE cF env (EAp e0 [e])
  | isPrim Commit e0                    = cE env e
cPMC cE cF env e0
  | isPrim Fail e0                      = cF env e0
cPMC cE cF env e                        = internalError "PMC syntax violated in Core2Kindle" e

cExpFail env e0                         = do [t] <- cTArgs env e0
                                             return (t, ValR Kindle.CBreak)

cCmdFail env e0                         = do [t] <- cTArgs env e0
                                             let Kindle.TClos [] [_] t' = t
                                             return (t', ValR Kindle.CBreak)

-- Translate the parts of a case expression into a Kindle.Cmd result
cCase cE env e (Alt (PCon k te) e':_)
  | isTuple k                           = do (bf,e) <- cValExpVar env e
                                             (te,t1,r) <- cRhs cE env (width k) [] (eLam te e')
                                             let (xs,ts) = unzip te
                                                 bs = mkBinds xs ts (map (Kindle.ESel e) (take (width k) (Kindle.conSels k)))
						 (bs1,bs2) = partition ((`elem` dups) . fst) bs
						 r' = rcomp (subst [ (x,e) | (x, Kindle.Val _ e) <- bs2 ]) r
                                             return (t1, rcomp (bf . Kindle.cBind bs1) r')
  where dups				= duplicates (evars e')
cCase cE env e alts                     = do (bf,t0,e0) <- cValExp env e
                                             rs <- mapM (cAlt cE env) alts
                                             let r = maxR rs
                                             rs <- mapM (adaptR r) rs
                                             return (mkSwitch bf rs e0)

cValExpVar env e                        = do (bf,t,e) <- cValExp env e
                                             case e of
                                                Kindle.EVar _ -> return (bf, e)
                                                _             -> do y <- newName tempSym
                                                                    return (bf . Kindle.cBind [(y, Kindle.Val t e)], Kindle.EVar y)


instance Pr (Kindle.AType, Result Kindle.Alt) where
    pr (_, a)				= pr a

maxR (r:rs)                             = max (arity r) r rs
  where arity (_,ValR _)                = 0
        arity (_,FunR _ ts)             = length ts
        max n0 r0 []                    = r0
        max n0 r0 (r:rs)
          | n > n0                      = max n r rs
          | otherwise                   = max n0 r0 rs
          where n                       = arity r


mkSwitch bf ((t,ValR c):rs) e0          = (t, ValR (bf (Kindle.CSwitch e0 alts)))
  where alts                            = c : [ alt | (_,ValR alt) <- rs ]     
mkSwitch bf ((t,FunR g ts):rs) e0       = (t, FunR (\es -> bf (Kindle.CSwitch e0 (map ($es) (g:gs)))) ts)
  where gs                              = [ g | (_,FunR g _) <- rs ]

mkSeq (t1,ValR c1) (t2,ValR c2)         = (t1, ValR (Kindle.CSeq c1 c2))
mkSeq (t1,FunR g1 ts1) (t2,FunR g2 ts2) = (t1, FunR (\es -> Kindle.CSeq (g1 es) (g2 es)) ts1)


-- =========================================================================================
-- Translating commands
-- =========================================================================================

-- Translate a Core.Cmd into a Kindle.Cmd
cCmd env (CRet e)                       = do (bf,te,e) <- freezeState env e
                                             (bf',t,e) <- cValExp (addTEnv te env) e
                                             if Kindle.isEVal e then              -- No state references or non-termination in e
                                                 return (t, bf (bf' (Kindle.CRet e)))
                                              else do
                                                 x <- newName tempSym
                                                 let bf'' = Kindle.cBind [(x, Kindle.Val t e)]
                                                 return (t, bf (bf' (bf'' (Kindle.CRet (Kindle.EVar x)))))
cCmd env (CAss x e c)                   = do (bf,te,e) <- freezeState env e
                                             (bf',e) <- cValExpT (addTEnv te env) tx e
                                             (t,c) <- cCmd env c
                                             return (t, bf (bf' (Kindle.CUpdS (stateRef env) x e c)))
  where Kindle.ValT tx                  = lookup' (tenv env) x
cCmd env (CLet bs c)                    = do (bf,te,bs) <- freezeState env bs
                                             (te',bf') <- cBinds (addTEnv te env) (entered c) bs
                                             (t,c) <- cCmd (addTEnv te' env) c
                                             return (t, bf (bf' c))
cCmd env (CGen x tx0 e c)   		= do (bf,te,e) <- freezeState env e
                                             (bf',e) <- cValExpT (addTEnv te env) tx (EAp e [EVar (self env)])
                                             (t,c) <- cCmd (addATEnv [(x,tx)] env) c
                                             return (t, bf (bf' (Kindle.cBind [(x,Kindle.Val tx e)] c)))
  where tx                              = cAType tx0
cCmd env (CExp e)                       = cCmdExp env e


-- Translate a Core.Exp in the monadic execution path into a Kindle.Cmd
cCmdExp env (ELet bs e)
  | not (isEncoding bs)                 = do (bf,te,bs) <- freezeState env bs
                                             (te',bf') <- cBinds (addTEnv te env) (entered e) bs
                                             (t,c) <- cCmdExp (addTEnv te' env) e
                                             return (t, bf (bf' c))
cCmdExp env (EAp e0 [e])  
--  | isPrim ReqToCmd e0                  = ...
  | isPrim Raise e0                     = do (bf,te,e) <- freezeState env e
                                             (bf',_,e') <- cValExp (addTEnv te env) e
                                             [t] <- cTArgs env e0
                                             let Kindle.TClos [] [_] t' = t
                                             return (t', bf (bf' (Kindle.CRaise e')))
  | isPrim Match e0                     = do (bf,te,e) <- freezeState env e
                                             (t,ValR c) <- cPMC cValCmdExp cCmdFail (addTEnv te env) e
                                             return (t, bf (Kindle.CSeq c (Kindle.CRaise (Kindle.ELit (lInt 1)))))
cCmdExp env (EDo x tx0 c)               = do (t,c) <- cCmd (pushSelf x (addATEnv [(x,Kindle.tRef tx)] env)) c
                                             return (t, Kindle.cBind [(x,Kindle.Val (Kindle.tRef tx) (Kindle.EVar (self env)))] c)
  where tx                              = cAType tx0
cCmdExp env (ECase e alts)              = do (bf,te,e) <- freezeState env e
					     (t,ValR c) <- cCase cValCmdExp (addTEnv te env) e alts
                                             return (t, bf c)
cCmdExp env e                           = do (bf,te,e) <- freezeState env e
                                             (bf',t,e) <- cValExp (addTEnv te env) (EAp e [EVar (self env)])
                                             x <- newName tempSym
                                             let bf'' = Kindle.cBind [(x, Kindle.Val t e)]
                                             return (t, bf (bf' (bf'' (Kindle.CRet (Kindle.EVar x)))))


cValCmdExp env e                        = do (t,c) <- cCmdExp env e
                                             return (t, ValR c)
                                             

-- State variables are normally translated into field selections from the current "self".  For example,
--     x := 7; result (x + 1)
-- gets translated into
--     self->x := 7; result (self->x + 1)
-- However, closure values, which may be invoked long after they are defined, must not be sensitive to state
-- mutations.  This means that "self" dereferencing operations for any contained state variables must be done 
-- when the closure is defined, not when it is invoked.  Here's a challenging example:
--     x := 7; f = \y->x+y; x := 2; result (f 1);
-- If we naively translate the x reference in f to self->x, we end up with the wrong behavior:
--     self->x := 7; int f(int y) { return self->x + y }; self->x := 2; return f(1);    -- gives 3, not 8
--
-- A correct translation can instead be obtained if we make sure that no state variables are ever referenced 
-- from within a lambda abstraction, as in the equivalent example
--     x := 7; x' = x; f = \y->x'+y; x := 2; return (f 1);                      (for some fresh variable x')
-- Achieving this is the job of function freezeState below.  It takes an expression e and returns a renaming 
-- of e with the "fragile" free variables (i.e., state vars inside lambdas) replaced by fresh variables, 
-- together with a type environment and Kindle bindings for the new variables.  For the example above, 
-- input (\y->x+y) results in output (\y->x'+y) together with the Kindle binding (int x' = self->x).  When
-- composed with the rest of the translation process, the full output becomes
--     self->x := 7; int x' = self->x; int f(int y) { return x'+y }; self->x := 2; return f(1);
-- This form is perfectly valid as input to the subsequent lambda-lifting pass, whose result will be
--     int f(int x'', int y) { return x'' + y }
--     self->x := 7; int x' = self->x; self->x := 2; return f(x',1);


class Fragile a where
    fragile :: a -> [Name]
    
instance Fragile Exp where
    fragile (ELam _ e)                  = filter isState (idents e)
    fragile (EAp e es)                  = concatMap fragile (e:es)
    fragile (ESel e l)                  = fragile e
    fragile (ERec c eqs)                = concatMap fragile (rng eqs)
    fragile (ELet bs e)                 = fragile bs ++ fragile e
    fragile (ECase e alts)              = fragile e ++ concatMap fragile alts
    fragile _                           = []

instance Fragile (Alt Exp) where
    fragile (Alt _ (ELam te e))         = fragile e
    fragile (Alt _ e)                   = fragile e
    
instance Fragile Binds where
    fragile bs                          = concatMap fragile (rng (eqnsOf bs))


freezeState env xx
  | null vs                             = return (id, [], xx)
  | otherwise                           = do vs' <- newNames paramSym (length vs)
                                             return (Kindle.cBind (zipWith3 f vs' ts vs), vs' `zip` ts, subst (vs `zip` map EVar vs') xx)
  where vs                              = nub (fragile xx)
        ts                              = map (lookup' (tenv env)) vs
        f v' (Kindle.ValT t) v          = (v', Kindle.Val t (Kindle.ESel (stateRef env) v))


-- Return a Kindle expression identifying the current state struct
stateRef env                            = Kindle.ESel (Kindle.EVar (self env)) (prim STATE)


-- =========================================================================================
-- Translating expressions
-- =========================================================================================

rename1 (Binds r te eqs) e              = do s <- mapM f xs
                                             let s' = mapSnd EVar s
                                             return (Binds r (rng s `zip` ts) (subst s ys `zip` subst s' es), subst s' e)
  where (xs,ts)                         = unzip te
        (ys,es)                         = unzip eqs
        f x                             = do n <- newNum
                                             return (x, x {tag = n})

cStructEqs env te eqs                   = do (bfs,bs) <- fmap unzip (mapM (cEq env te) eqs)
                                             return (foldr (.) id bfs, bs)


-- Translate a Core.Exp into an expression result that is either a value or a function,
-- overflowing into a list of Kindle.Binds if necessary
cExp env (ELet bs e)
  | isTAppEncoding bs                   = cExp (setTArgs env (map cAType (tAppTypes bs))) e
  | not (isTAbsEncoding bs)             = do (bs,e) <- rename1 bs e
                                             (te,bf) <- cBinds env (entered e) bs
                                             (bf',t,h) <- cExp (addTEnv te env) e
                                             return (bf . bf', t, h)
cExp env (ELit l)                       = return (id, Kindle.litType l, ValR (Kindle.ELit l))
cExp env (ERec c eqs)                   = do (bf,bs) <- cStructEqs (setTArgs env []) te' eqs
                                             return (bf, Kindle.TCon c ts, ValR (Kindle.ENew c ts bs))
  where ts                              = tArgs env
        Kindle.Struct vs te _           = findDecl env c
        te'                             = subst (vs `zip` ts) te
cExp env (EAp e0 [e])
  | isPrim ActToCmd e0                  = do (bf,t,e) <- cValExp env (EAp e [EVar (prim Inherit), EVar (prim Inherit)])
                                             [t1] <- cTArgs env e0
                                             return (bf, t, FunR (\_ -> e) [Kindle.tRef t1])
  | isPrim ReqToCmd e0                  = do (bf,t,e) <- cValExp env (EAp e [ELit (lInt 0)])
                                             [t1,t2] <- cTArgs env e0
                                             return (bf, t, FunR (\_ -> e) [Kindle.tRef t2])
  | Just t <- isCastPrim e0             = do (bf,_,e') <- cExp env e
                                             return (bf, t, rcomp (Kindle.ECast t) e')
  | isPrim RefToOID e0                  = do (bf,t,e) <- cExp env e
                                             return (bf, Kindle.tOID, rcomp (Kindle.ECast Kindle.tOID) e)
  | isPrim New e0                       = cExp env (EAp e [ELit (lInt 0)])       -- Can't occur but in CBind rhs, syntactic restriction
cExp env (EAp e0 [e,e'])
  | isPrim After e0                     = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (bf',t,f,ts) <- cFunExp env e'
                                             return (bf . bf', t, FunR (\[a,b] -> f [e1, b]) ts)
  | isPrim Before e0                    = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (bf',t,f,ts) <- cFunExp env e'
                                             return (bf . bf', t, FunR (\[a,b] -> f [a, e1]) ts)
cExp env (EAp e es) 
  | not (isPrim Match e)                = do (bf,t,f,ts) <- cFunExp env e
                                             appFun env bf t f ts es
  where appFun env bf t f ts es
          | l_ts <  l_es                = do (bf',es1) <- cValExpTs env ts es1
                                             let Kindle.TClos [] ts' t' = t 
                                             appFun env (bf . bf') t' (Kindle.enter (f es1) []) ts' es2
          | l_ts == l_es                = do (bf',es) <- cValExpTs env ts es
                                             return (bf . bf', t, ValR (f es))
          | l_ts >  l_es                = do (bf',es) <- cValExpTs env ts1 es
                                             return (bf . bf', t, FunR (f . (es++)) ts2)
          where l_ts                    = length ts
                l_es                    = length es
                (ts1,ts2)               = splitAt l_es ts
                (es1,es2)               = splitAt l_ts es
cExp env (EVar x)                       = case lookup' (tenv env) x of
                                             Kindle.ValT t
                                               | null ts         -> return (id, t, ValR e)
                                               | null ts'        -> return (id, subst s t', ValR (Kindle.enter e ts []))
                                               | otherwise       -> return (id, subst s t', FunR (Kindle.enter e ts) (subst s ts'))
                                               where Kindle.TClos vs ts' t' = t
                                                     s                      = vs `zip` ts
                                             Kindle.FunT vs ts' t 
                                               | null ts'        -> return (id, subst s t, ValR (Kindle.ECall x ts []))
                                               | otherwise       -> return (id, subst s t, FunR (Kindle.ECall x ts) (subst s ts'))
                                               where s            = vs `zip` ts
  where e                               = if stateVar (annot x) then Kindle.ESel (stateRef env) x else Kindle.EVar x
        ts                              = tArgs env
cExp env (ESel e l)                     = do (bf,e) <- cValExpT (setTArgs env []) (Kindle.TCon k ts0) e
                                             case subst (vs0 `zip` ts0) rhstype of
                                               Kindle.ValT t       -> return (bf, t, ValR (Kindle.ESel e l))
                                               Kindle.FunT vs ts t -> return (bf, subst s t, FunR (Kindle.EEnter e l ts1) (subst s ts))
                                                 where s            = vs `zip` ts1
  where (k,vs0,rhstype)                 = Kindle.typeOfSel (decls env) l
        (ts0,ts1)                       = splitAt (length vs0) (tArgs env)  -- tArgs lists *full* instantiation, not just local quantification
cExp env (ECon k) 
  | isTuple k                           = case ts of
                                              [] -> return (id, Kindle.TCon k [], ValR (Kindle.ENew k [] []))
                                              _  -> return (id, Kindle.TCon k ts, FunR (Kindle.ENew k ts . mkBinds (Kindle.conSels k) ts) ts)
  | otherwise                           = case te of
                                              [] -> return (id, t, ValR (newK []))
                                              _  -> return (id, t, FunR (newK . mkBinds (Kindle.conSels k) ts') ts')
  where ts                              = tArgs env
        Kindle.Struct vs te (Kindle.Extends t0 _) = findDecl env k
        s                               = vs `zip` ts
        ts'                             = subst s (map Kindle.rngType (rng te))
        t                               = subst s t0
        newK                            = Kindle.ECast t . Kindle.ENew (injectCon k) ts
cExp env e                              = do (vs,te,t,c) <- cFun0 env e
                                             case (vs,te) of
                                               ([],[]) 
						  | Kindle.CRet e <- c -> return (id, t, ValR e)
						  | otherwise -> do
--							x <- newName tempSym
--							return (Kindle.cBind [(x, Kindle.Fun [] t [] c)], t, ValR (Kindle.ECall x [] []))
							return (id, t, ValR (Kindle.EEnter (Kindle.EClos [] t [] c) (prim Code) [] []))
					       _  -> return (id, t', ValR (Kindle.EClos vs t te c))
                                                  where t' = Kindle.TClos vs (rng te) t
                                                  


-- Translate a Core.Exp into a Kindle value expression
cValExp env e                           = do (bf,t,h) <- cExp env e
                                             case h of
                                               ValR e -> do
                                                  return (bf, t, e)
                                               FunR f ts -> do
                                                  xs <- newNames paramSym (length ts)
                                                  let t' = Kindle.TClos [] ts t
                                                      es = map Kindle.EVar xs
                                                      te = xs `zip` ts
                                                  return (bf, t', Kindle.EClos [] t te (Kindle.CRet (f es)))


-- Translate a Core.Exp into a Kindle function
cFunExp env e                           = do (bf,t,h) <- cExp env e
                                             case h of
                                               FunR f ts -> return (bf, t, f, ts)
                                               ValR e'   -> return (bf, t', Kindle.enter e' [], ts)
                                                 where Kindle.TClos [] ts t' = t
 

-- Map a Kindle.ATEnv (unzipped) and a list of Kindle.Exps into a list of Kindle.Binds
mkBinds xs ts es                        = zipWith3 f xs ts es
  where f x t e                         = (x, Kindle.Val t e)


-- Check if expression is a primitive, possibly applied to type arguments
isPrim p (ELet bs e) 
  | isTAppEncoding bs                   = isPrim p e
isPrim p (EVar (Prim p' _))
  | p == p'                             = True
isPrim p e                              = False

isCastPrim (EVar (Prim p _))
  | p `elem` intCasts                   = Just Kindle.tInt
--  | p == IntToChar                      = Just Kindle.tChar
  | p == IntToBITS8                     = Just Kindle.tBITS8
  | p == IntToBITS16                    = Just Kindle.tBITS16
  | p == IntToBITS32                    = Just Kindle.tBITS32
  where intCasts                        = [{-CharToInt,-}BITS8ToInt,BITS16ToInt,BITS32ToInt]
isCastPrim _                            = Nothing


-- Extract type arguments from expression
cTArgs env (ELet bs e)
  | isTAppEncoding bs                   = return (map cAType (tAppTypes bs))
cTArgs env e                            = return []



-- Additional entry point for translating imported environments
c2kTEnv ds te                           = return (cTEnv te)
