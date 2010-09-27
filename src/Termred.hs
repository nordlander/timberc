{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

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

module Termred(termred) where

import Control.Monad
import Common
import Core
import PP
import Data.Char

termred (Module _ _ _ _ ds' _ [bs']) m         = redModule ds' (eqnsOf bs') m


data Ctxt                       = AppC  [Exp]
                                | SelC  Name
                                | CaseC [Alt Exp]
                                deriving (Eq,Show)

instance Pr [Ctxt] where
    pr []                       = text "NOMORE"
    pr (AppC es : c)            = text "[ ]" <+> hpr ' ' es <+> text ":" <+> pr c
    pr (SelC l : c)             = text "[ ]" <> text ('.' : show l) <+> text ":" <+> pr c
    pr (CaseC alts : c)         = text "case [ ] of ..." <+> text ":" <+> pr c

capp [] e                       = e
capp (AppC es : c) e            = capp c (EAp e es)
capp (SelC l : c) e             = capp c (ESel e l)
capp (CaseC alts : c) e         = capp c (ECase e alts)

data Env                        = Env { eqns :: Map Name Exp,
                                        cons :: [Map Name Int],
                                        sels :: TEnv
                                      }

initEnv                         = Env { eqns = [], cons = cons0, sels = [] }

cons0                           = [ [(prim TRUE, 0), (prim FALSE, 0)] , [(prim NIL, 0), (prim CONS, 2)] ]

consOf ds                       = [ map f ce | (_,DData _ _ ce) <- ds ]
  where f (c, Constr te pe _)   = (c, length te + length pe)

selsOf ds                       = concat [ ss | (_,DRec _ _ _ ss) <- ds ]

conArity env (Tuple n _)        = n
conArity env c                  = lookup' (concat (cons env)) c


complete _ [Tuple _ _]          = True
complete [] cs0                 = False
complete ([]:css) []            = True
complete ([]:css) cs0		= complete css cs0
complete (cs:css) cs0           = all (`elem`cs0) cs  ||  complete css cs0

addEqns eqs env                 = env { eqns = eqs ++ eqns env }

delEqn x env                    = env { eqns = eqns env `prune` [x] }

addDecls env ds                 = env { cons = consOf ds ++ cons env, sels = selsOf ds ++ sels env }



redModule impDecls impEqs (Module m ns xs es ds is bss)
                                = do (bss,_) <- redTopBinds env bss
                                     return (Module m ns xs es ds is bss)
  where env                     = addEqns impEqs (addDecls initEnv (tdefsOf impDecls ++ tdefsOf ds))


redTopBinds env []              = return ([], [])
redTopBinds env (bs : bss)      = do Binds r te es <- redBinds env bs
                                     (bss,vs) <- redTopBinds (addEqns es env) bss
                                     let necessary (v,_) = r || maybe (elem v vs) (const True) (fromMod v)
                                         te' = filter necessary te
                                         es' = filter necessary es
                                         bs' = Binds r te' es'
                                         bss' = if null te' then bss else bs':bss
                                     return (bss', idents es' ++ vs)
                                     


redBinds env (Binds r te eqns)  = do eqns' <- redEqns env eqns
                                     bs <- staticDelayRule env (Binds r te eqns')
                                     return bs


redEqns env eqs                 = do es' <- mapM (redExp env [] ) es
                                     return (xs `zip` es')
  where (xs,es)                 = unzip eqs


redExp env c (ELit l)           = prodExp env c (ELit (normLit l))
redExp env c (ERec k eqs)       = do es' <- mapM (redExp env []) es
                                     prodExp env c (ERec k (ls `zip` es'))
  where (ls,es)                 = unzip eqs
redExp env c (ELam te e)        = do e <- redExp env [] e
                                     prodExp env c (redEta env te e)
redExp env c (ECon k)           = prodExp env c (ECon k)
redExp env c (EVar x)           = case lookup x (eqns env) of
                                    Just e -> do e1 <- prodExp (delEqn x env) c e
                                                 if size e1 <= size c + 10
                                                    then return e1
                                                    else prodExp env c (EVar x)
                                    _      -> do prodExp env c (EVar x)
redExp env c (EAp e es)         = do es1 <- mapM (redExp env []) es
                                     (bss,es2) <- extractBinds es1
				     e3 <- redExp env (AppC es2 : c) e
                                     return (foldr ELet e3 bss)
redExp env c (ESel e s)         = redExp env (SelC s : c) e
redExp env c (ECase e alts)     = do alts' <- mapM redAlt alts
                                     redExp env (CaseC (dropRedundant env alts') : c) e
  where redAlt (Alt p e)        = do e' <- redExp env [] e
                                     return (Alt p e')
redExp env c (ELet (Binds True te eqs) e)
                                = do (te,eqs,e) <- alphaC te eqs e
                                     bs <- redBinds env (Binds True te eqs)
                                     liftM (ELet bs) (redExp env c e)
redExp env c (ELet (Binds False te eqs) e)
                                = do (te2,eqs2,e) <- alphaC te2 eqs2 e
                                     eqs2 <- redEqns env eqs2
                                     liftM (eLet te2 eqs2) (redExp env c (subst eqs1 e))
  where (eqs1,eqs2)             = safeSubst (duplicates (evars e)) (strict e) eqs
        te2                     = te `restrict` dom eqs2
redExp env _ (ETempl x t te c)  = liftM (ETempl x t te) (redCmd env c)
redExp env _ (EAct e e')        = liftM2 EAct (redExp env [] e) (redExp env [] e')
redExp env _ (EReq e e')        = liftM2 EReq (redExp env [] e) (redExp env [] e')
redExp env _ (EDo x t c)        = liftM (EDo x t) (redCmd env c)


alphaC te eqs e                 = do xs' <- mapM (newName . str) xs
                                     let s = xs `zip` map EVar xs'
                                         te' = substVars (xs `zip` xs') (dom te) `zip` rng te
                                     return (te', xs' `zip` subst s es, subst s e)
  where (xs,es)                 = unzip eqs


prodExp env c e
  | isRaise e                   = return e
prodExp env (AppC es : _) e
  | not (isPMC e) && any isRaise es
                                = return (head (filter isRaise es))
prodExp env (SelC l : c) (ERec _ eqs)
  | all terminating (rng eqs)   = prodExp env c (lookup' eqs l)
prodExp env (AppC es : c) (ELam te e)
                                = do (te2,eqs2,e) <- alphaC te2 eqs2 e
				     liftM (eLet te2 eqs2) (redExp env c (subst eqs1 e))
  where (eqs1,eqs2)             = safeSubst (duplicates (evars e)) (strict e) (dom te `zip` es)
        te2                     = te `restrict` dom eqs2
prodExp env (AppC es : c) (EVar (Prim p a))
                                = prodExp env c (redPrim p a es)
prodExp env (AppC es : c) e     = prodExp env c (EAp e es)
prodExp env (CaseC alts : c) (ECon k)
                                = redExp env c (findConAlt k [] alts)
prodExp env (CaseC alts : c) (EAp (ECon k) es)
                                = redExp env c (findConAlt k es alts)
prodExp env (CaseC alts : c) (ELit l)
                                = redExp env c (findLitAlt l alts)
prodExp env c e                 = return (capp c e)


safeSubst dups strictvs eqs     = partition safe eqs
  where 
    safe (x,e)                  = smallValue e || ({-x `notElem` dups && -} (terminating e || x `elem` strictvs))

stricts es                      = concat (map strict es)
strict (EVar x)                 = [x]
strict (EAp e es)               = strict e ++ stricts es
strict (ECon c)                 = []
strict (ELit l)                 = []
strict (ELam te e)              = []
strict (ESel e l)               = strict e
strict (ELet bs e)              = (stricts es ++ strict e) \\ xs
  where (xs,es)                 = unzip (eqnsOf bs)
strict (ERec n eqs)             = stricts (rng eqs)
strict (EAct e e')              = strict e ++ strict e'
strict (EReq e e')              = strict e ++ strict e'
strict (EDo x t c)              = []
strict (ETempl x t te c)        = []
strict (ECase e alts)           = strict e


-- Pick alt by constructor -----------------------------------------------------------------------------------

findConAlt k [] (Alt (PCon k' []) e : alts)
  | k == k'                     = e
findConAlt k es (Alt (PCon k' []) e : alts)
  | k == k'                     = eAp e es
findConAlt k es (Alt (PCon k' te) e : alts)
  | k == k'                     = eAp (eLam te e) es
findConAlt k es (Alt PWild e : alts)
                                = e
findConAlt k es (_ : alts)      = findConAlt k es alts


-- Pick alt by literal ---------------------------------------------------------------------------------------

findLitAlt l (Alt (PLit l') e : alts)
  | l == l'                     = e
findLitAlt (LStr _ "") (Alt (PCon (Prim NIL _) []) e : alts)
                                = e
findLitAlt (LStr _ (c:cs)) (Alt (PCon (Prim CONS _) []) e : alts)
                                = EAp e [ELit (LChr Nothing c), ELit (LStr Nothing cs)]
findLitAlt (LStr _ (c:cs)) (Alt (PCon (Prim CONS _) te) e : alts)
                                = EAp (ELam te e) [ELit (LChr Nothing c), ELit (LStr Nothing cs)]
findLitAlt l (Alt PWild e : alts)
                                = e
findLitAlt l (_ : alts)         = findLitAlt l alts


-- Floating out let-bindings --------------------------------------------------------------------------------

extractBinds []                 = return ([], [])
extractBinds (ELet (Binds r te eqs) e : es)   
                                = do (te,eqs,e) <- alphaC te eqs e
                                     (bss,es') <- extractBinds es
                                     return (Binds r te eqs : bss, e:es')
extractBinds (e : es)           = do (bss,es') <- extractBinds es
                                     return (bss, e:es')


-- Normalizing big integer literals --------------------------------------------------------------------------

normLit (LInt p i)
  | i >= 0x80000000             = normLit (LInt p (i - 0x100000000))
  | i < -0x80000000             = normLit (LInt p (i + 0x100000000))
  | otherwise                   = LInt p i
normLit l                       = l


-- Eta reduction ---------------------------------------------------------------------------------------------

redEta env te (EAp e es)
  | okEta e && es==map EVar xs  = e
  where okEta (ECon _)          = False
        okEta (EVar (Prim _ _)) = False
        okEta e                 = null (evars e `intersect` xs)
        xs                      = dom te
redEta env te e                 = ELam te e

-- Removing redundant wildcard alternatives -------------------------------------------------------------------

dropRedundant env alts
  | complete css cs      	= alts'
  | otherwise                   = alts
  where alts'                   = takeWhile (isConPat . patOf) alts
        cs                      = [ c | Alt (PCon c _) _ <- alts' ]
	css			= map dom (cons env)
      

-- Primitives --------------------------------------------------------------------------------------------------

redPrim Refl _ [e]                          = e
redPrim ClassRefl _ [EVar (Prim Refl _)]    = EVar (prim Refl)
redPrim ClassRefl _ [ELam [(x,_)] e]
  | e == EVar x				    = EVar (prim Refl)
redPrim LazyAnd _ [ECon (Prim TRUE _), e]   = e
redPrim LazyAnd _ [ECon (Prim FALSE _), _]  = ECon (prim FALSE)
redPrim LazyOr _ [ECon (Prim TRUE _), _]    = ECon (prim TRUE)
redPrim LazyOr _ [ECon (Prim FALSE _), e]   = e
redPrim Match a [e]                         = redMatch a e
redPrim Fatbar a [e,e']                     = redFat a e e'
redPrim UniArray a es                       = EAp (EVar (Prim UniArray a)) es
redPrim IntNeg _ [ELit (LInt _ x)]          = ELit (lInt (-x))
redPrim IntToFloat _ [ELit (LInt _ x)]      = ELit (lRat (fromInteger x))
redPrim IntToChar _ [ELit (LInt _ x)]       = ELit (lChr (chr (fromInteger x)))
redPrim FloatNeg _ [ELit (LRat _ x)]        = ELit (lRat (-x))
redPrim FloatToInt _ [ELit (LRat _ x)]      = ELit (lInt (truncate x))
redPrim CharToInt _ [ELit (LChr _ x)]       = ELit (lInt (ord x))
redPrim p a [ELit (LInt _ x), ELit (LInt _ y)]
  | p `notElem` [IntDiv,IntMod] || y /= 0   = redInt p x y
redPrim p a [ELit (LRat _ x), ELit (LRat _ y)]
  | p /= FloatDiv || y /= 0                 = redRat p x y
redPrim p a es                              = eAp (EVar (Prim p a)) es


redMatch a (ELet bs e)                      = ELet bs (redMatch a e)
redMatch a (ELam te e)                      = ELam te (redMatch a e)
redMatch a (EAp (EVar (Prim Commit _)) [e]) = e
redMatch a (ECase e alts)                   = ECase e [Alt p (redMatch a e)|Alt p e<-alts]
redMatch _ (EVar (Prim Fail a))             = EAp (EVar (Prim Raise a)) [ELit (lInt 1)]
redMatch _ e@(ELit _)                       = e
redMatch a e                                = EAp (EVar (Prim Match a)) [e]


redFat a (ELet bs e) e'                     = ELet bs (redFat a e e')
redFat a (EVar (Prim Fail _)) e             = e
redFat a e@(EAp (EVar (Prim Commit _)) _) _ = e
redFat a e e'                               = EAp (EVar (Prim Fatbar a)) [e,e']


redInt IntPlus a b              = ELit (normLit (lInt (a + b)))
redInt IntMinus a b             = ELit (normLit (lInt (a - b)))
redInt IntTimes a b             = ELit (normLit (lInt (a * b)))
redInt IntDiv a b               = ELit (lInt (a `div` b))
redInt IntMod a b               = ELit (lInt (a `mod` b))
redInt IntEQ a b                = eBool (a == b)
redInt IntNE a b                = eBool (a /= b)
redInt IntLT a b                = eBool (a < b)
redInt IntLE a b                = eBool (a <= b)
redInt IntGE a b                = eBool (a >= b)
redInt IntGT a b                = eBool (a > b)
redInt p _ _                    = internalError0 ("redInt: " ++ show p)


redRat FloatPlus a b            = ELit (lRat (a + b))
redRat FloatMinus a b           = ELit (lRat (a - b))
redRat FloatTimes a b           = ELit (lRat (a * b))
redRat FloatDiv a b             = ELit (lRat (a / b))
redRat FloatEQ a b              = eBool (a == b)
redRat FloatNE a b              = eBool (a /= b)
redRat FloatLT a b              = eBool (a < b)
redRat FloatLE a b              = eBool (a <= b)
redRat FloatGE a b              = eBool (a >= b)
redRat FloatGT a b              = eBool (a > b)
redRat p _ _                    = internalError0 ("redRat: " ++ show p)


eBool True                      = ECon (prim TRUE)
eBool False                     = ECon (prim FALSE)


-- Commands -----------------------------------------------------------------------------------------------

redCmd env (CRet e)             = liftM CRet (redExp env [] e)
redCmd env (CExp e)             = liftM CExp (redExp env [] e)
redCmd env (CGen p t (ELet bs e) c)
                                = redCmd env (CLet bs (CGen p t e c))
redCmd env (CGen p t e c)       = liftM2 (CGen p t) (redExp env [] e) (redCmd env c)
redCmd env (CLet bs@(Binds rec te eqs) c)
  | not rec && all smallValue es        -- good enough, and it avoids the need to define strictness for commands
                                = redCmd env (subst eqs c)
  | otherwise                   = do bs' <- redBinds env bs
                                     liftM (CLet bs') (redCmd env c)
  where (xs,es)                 = unzip eqs
redCmd env (CAss x e c)         = liftM2 (CAss x) (redExp env [] e) (redCmd env c)


-- Comparing terms w.r.t. sizes ---------------------------------------------------------------------------

class Size a where
    size :: a -> Int
    
instance Size a => Size [a] where
    size es                     = sum (map size es)

instance Size a => Size (Name,a) where
    size (x,e)                  = size e + 1

instance Size Exp where
    size (EVar _)               = 1
    size (ECon _)               = 1
    size (ELit _)               = 1
    size (ESel e _)             = size e + 1
    size (EAp e es)             = size e + size es + 1
    size (ELam te e)            = size e + length te
    size (ELet bs e)            = size bs + size e + 1
    size (ECase e alts)         = size e + size alts + 1
    size (ERec _ eqs)           = size eqs + 1
    size (EAct _ e)             = size e + 1
    size (EReq _ e)             = size e + 1
    size (EDo _ _ c)            = size c + 1
    size (ETempl _ _ _ c)       = size c + 1
    
instance Size Binds where
    size (Binds _ _ eqs)        = size eqs

instance Size (Alt Exp) where
    size (Alt p e)              = size e + 1

instance Size Cmd where
    size (CLet bs c)            = size bs + size c
    size (CGen _ _ e c)         = size e + size c
    size (CAss _ e c)           = size e + size c
    size (CRet e)               = size e
    size (CExp e)               = size e

instance Size Ctxt where
    size (AppC es)              = size es + 1
    size (SelC _)               = 1
    size (CaseC alts)           = size alts + 1


-- Terms that can be ignored without changing the semantics ------------------------------------------------

terminating (EVar _)            = True
terminating (ECon _)            = True
terminating (ELit _)            = True
terminating (ELam _ _)          = True
terminating (ESel e _)          = terminating e
terminating (ERec _ eqs)        = all terminating (rng eqs)
terminating (EAp (ECon _) es)   = all terminating es
terminating (EAp (EVar (Prim p _)) es)
                                = p `notElem` [IntDiv, FloatDiv, Raise] && all terminating es
terminating (ELet bs e)         = all terminating (e : rng (eqnsOf bs))
terminating (EDo _ _ _)         = True
terminating (ETempl _ _ _ _)    = True
terminating (EAct e e')         = all terminating [e,e']
terminating (EReq e e')         = all terminating [e,e']
terminating _                   = False


-- Terms that can be be inlined zero or multiple times without changing either semantics or program size --

smallValue (EVar x)             = x /= prim New && not (isState x)
smallValue (ECon _)             = True
smallValue (ELit _)             = True
smallValue e                    = False


-- Implement the static delay rule ------------------------------------------------------------------------

staticDelayRule env bs@(Binds rec te eqs)
  | not rec                     = return bs
  | rec                         = do (eqs',eqs1) <- walkEqs env te (dom eqs) eqs
                                     ts <- mapM (const (newTvar Star)) eqs1
                                     return (Binds rec (te ++ (dom eqs1 `zip` map scheme ts)) (eqs'++eqs1))

      
walkEqs env te fw []            = return ([], [])
walkEqs env te fw (eq:eqs)      = do (eq,eqs1) <- doEq te eq
                                     (eqs,eqs2) <- walkEqs env te (fw \\ [fst eq] ++ dom eqs1) eqs
                                     return (eq:eqs, eqs1++eqs2)
  where doEq te eq@(x,e)
          | null ke             = do (e,eqs) <- doExp e
                                     return ((x,e), eqs)
          | otherwise           = return ((x,e), [])
          where ke              = quant (lookup' te x)
        doAlt (Alt p e)         = do (e,eqs1) <- doExp e
                                     return (Alt p e, eqs1)
        doExp (ESel e l)
          | True {- isCoerceLabel l -}    = maybeDelay (\e -> ESel e l) e
          | otherwise           = do (e,eqs1) <- doExp e
                                     return (ESel e l, eqs1)
        doExp (ECase e alts)    = do (e,eqs1) <- doExp e
                                     (alts,eqss) <- fmap unzip (mapM doAlt alts)
                                     return (ECase e alts, eqs1++concat eqss)
        doExp (EAp e es)        = do (e,eqs1) <- doExp e
                                     (es,eqss) <- fmap unzip (mapM doExp es)
                                     return (EAp e es, eqs1++concat eqss)
        doExp (ELet (Binds r te eqs) e)
                                = do (eqs',eqss) <- fmap unzip (mapM (doEq te) eqs)
                                     (e,eqs1) <- doExp e
                                     return (ELet (Binds r te eqs') e, eqs1++concat eqss)
        doExp (ERec n eqs)      = do (eqs',eqss) <- fmap unzip (mapM (doEq (sels env)) eqs)
                                     return (ERec n eqs', concat eqss)
        doExp e                 = return (e, [])
        maybeDelay f (ESel e l)
          | True {- isCoerceLabel l -}    = maybeDelay (\e -> f (ESel e l)) e
        maybeDelay f (EVar x)
          | x `elem` fw         = do y <- newName tempSym
                                     return (EVar y, [(y, f (EVar x))])
        maybeDelay f e          = do (e,eqs1) <- doExp e
                                     return (f e, eqs1)
