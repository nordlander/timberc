module Termred(termred, redTerm) where

import Common
import Core
import Env
import PP

termred m                       = return (redModule m)


redTerm e                       = redExp env0 e
  where env0                    = []


redModule (Module m ds ie bs)   = Module m ds ie (redBinds env0 bs)
  where env0                    = []


simple (EVar _)                 = True
simple (ECon _)                 = True
simple (ESel _)                 = True
simple (ELit _)                 = True
simple (EAp (ESel _) [e])       = simple e
simple (ELam _ e)               = simple e      -- dubious...
simple _                        = False


value e                         = simple e || val (eFlat e)
  where val (ECon _, _)         = True
        val (ERec _, _)         = True
        val (ELam _ _, [])      = True
        val _                   = False


redBinds env (Binds r te eqns)  = Binds r te (mapSnd (redExp env) eqns)


redExp env e                    = redApp env e []


redApp env (ERec eqs) []        = ERec (mapSnd (redExp env) eqs)
redApp env (ETempl x te t c) [] = ETempl x te t (redCmd env c)
redApp env (EAct x c) []        = EAct x (redCmd env c)
redApp env (EReq x c) []        = EReq x (redCmd env c)
redApp env (EDo c) []           = EDo (redCmd env c)
redApp env (EAp f es) es'       = redApp env f (map (redExp env) es ++ es')
redApp env (ELam te e) es       = redBeta env te e es
redApp env (ESel s) (e:es)      = redSel env e s es
redApp env (ECase e alts d) es  = redCase env (eFlat (redExp env e)) alts d es
redApp env (ELet bs e) es
  | rec                         = ELet bs' (redApp env e es)
  | otherwise                   = redBeta env te e (map (lookup' eqs) (dom te) ++ es)
  where bs'@(Binds rec te eqs)  = redBinds env bs
redApp env (EVar x) es          = redPrim env x es
redApp env e es                 = eAp e es


redCmd env c                    = undefined -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


redBeta env ((x,t):te) b (e:es)
  | e == EVar x                 = redBeta env te b es                    -- identity subst
  | b == EVar x                 = redBeta env te e es                    -- trivial body
  | isGenerated x || simple e   = redBeta ((x,e):env) te b es            -- appears only once(??), strict, no size increase
  | otherwise                   = ELet (Binds False [(x,t)] [(x,e)]) (redBeta env te b es)
redBeta env [] b es             = redApp env b es
redBeta env te b []             = redEta te (redExp env b)



redEta [] e                     = e
redEta [(x,Scheme (R t) [] [])] (EAp e [EVar x'])
  | x == x' && simple e && x `notElem` evars e
                                = e
redEta te e                     = eLam te e                              -- do not break multi-lambdas!


redSel env (ERec eqs) s es      = case lookup s eqs of
                                    Just e -> redApp env e es
                                    Nothing -> error "Internal: redSel"
redSel env e s es               = eAp (ESel s) (e:es)


redCase env (ECon k,es') as d es
  | all value es'                = findCon env k es' as d es
redCase env (ELit l,_) as d es   = findLit env l as d es
redCase env (e,es') as d es      = eAp (ECase (eAp e (map (redExp env) es')) (mapSnd (redExp env) as) (redExp env d)) es


findCon env k es' [] d es       = redApp env d es
findCon env k es' ((PCon k',e):_) d es
  | k == k'                     = redApp env e (es'++es)
findCon env k es' (_:alts) d es = findCon env k es' alts d es


findLit env l [] d es           = redApp env d es
findLit env l ((PLit l',e):_) d es
  | l == l'                     = redApp env e es
findLit env l (_:alts) d es     = findLit env l alts d es


redIdApp env (e:es)             = redApp env e es
redIdApp env []                 = EVar (prim Refl)


redFatApp env (EVar (Prim Fail _) : es)
                                = redIdApp env es
redFatApp env (EAp (EVar (Prim Commit _)) [e] : es)
                                = e
redFatApp env es                = eAp (EVar (prim Fatbar)) es


redPrim env (Prim Refl _) es    = redIdApp env es
redPrim env (Prim Fatbar _) es  = redFatApp env es
redPrim env (Prim p _) [ELit (LInt a), ELit (LInt b)]
                                = redInt p a b
redPrim env (Prim p _) [ELit (LRat a), ELit (LRat b)]
                                = redRat p a b
redPrim env (Prim IntNeg _) [ELit (LInt a)]
                                = ELit (LInt (-a))
redPrim env (Prim FloatNeg _) [ELit (LRat a)]
                                = ELit (LRat (-a))
redPrim env x es                = case lookup x env of
                                    Just e -> redApp env e es
                                    Nothing -> eAp (EVar x) es


redInt IntPlus a b              = ELit (LInt (a + b))
redInt IntMinus a b             = ELit (LInt (a - b))
redInt IntTimes a b             = ELit (LInt (a * b))
redInt IntDiv a b               = ELit (LInt (a `div` b))
redInt IntMod a b               = ELit (LInt (a `mod` b))
redInt IntEQ a b                = eBool (a == b)
redInt IntNE a b                = eBool (a /= b)
redInt IntLT a b                = eBool (a < b)
redInt IntLE a b                = eBool (a <= b)
redInt IntGE a b                = eBool (a >= b)
redInt IntGT a b                = eBool (a > b)
redInt p _ _                    = error ("Internal: redInt " ++ show p)


redRat FloatPlus a b            = ELit (LRat (a + b))
redRat FloatPlus a b            = ELit (LRat (a + b))
redRat FloatMinus a b           = ELit (LRat (a - b))
redRat FloatTimes a b           = ELit (LRat (a * b))
redRat FloatDiv a b             = ELit (LRat (a / b))
redRat FloatEQ a b              = eBool (a == b)
redRat FloatNE a b              = eBool (a /= b)
redRat FloatLT a b              = eBool (a < b)
redRat FloatLE a b              = eBool (a <= b)
redRat FloatGE a b              = eBool (a >= b)
redRat FloatGT a b              = eBool (a > b)
redRat p _ _                    = error ("Internal: redRat " ++ show p)




eBool True                      = EVar (prim TRUE)
eBool False                     = EVar (prim FALSE)