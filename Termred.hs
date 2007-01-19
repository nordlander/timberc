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


value (EVar _)                  = True
value (ECon _)                  = True
value (ESel _)                  = True
value (ELit _)                  = True
value (EAp (ECon c) es)         = all value es
value (ERec _ eqs)              = all (value . snd) eqs
value (ELam _ _)                = True
value _                         = False


redBinds env (Binds r te eqns)  = Binds r te (mapSnd (redExp env) eqns)


redExp env (ESel s)             = ESel s
redExp env (ECon c)             = ECon c
redExp env (ELit l)             = ELit l
redExp env (ERec c eqs)         = ERec c (mapSnd (redExp env) eqs)
redExp env (ETempl x t te c)    = ETempl x t te (redCmd env c)
redExp env (EAct e e')          = EAct (redExp env e) (redExp env e')
redExp env (EReq e e')          = EReq (redExp env e) (redExp env e')
redExp env (EDo x t c)          = EDo x t (redCmd env c)
redExp env (ELam te e)          = ELam te (redExp env e)
redExp env (EAp e es)           = redApp env e (map (redExp env) es)
redExp env e                    = redApp env e []

redApp env (ELam te e) es       = redBeta env te e es
redApp env (ESel s) [e]         = redSel env e s
redApp env (ECase e alts d) es  = redCase env (eFlat (redExp env e)) alts d es
redApp env (ELet bs e) es
  | rec                         = ELet bs' (redApp env e es)
  | otherwise                   = redBeta env te e (map (lookup' eqs) (dom te) ++ es)
  where bs'@(Binds rec te eqs)  = redBinds env bs
redApp env (EVar (Prim p a)) es = redPrim env p a es
redApp env (EVar x) es          = case lookup x env of
                                      Just e -> redApp env e es
                                      Nothing -> eAp (EVar x) es
redApp env (ESig (EVar x) t) es = case lookup x env of
                                      Just e -> redApp env e es
                                      Nothing -> eAp (ESig (EVar x) t) es
redApp env (ESig e t) es        = eAp (ESig (redExp env e) t) es
redApp env e es                 = eAp (redExp env e) es


redCmd env (CRet e)             = CRet (redExp env e)
redCmd env (CExp e)             = CExp (redExp env e)
redCmd env (CGen p t e c)       = CGen p t (redExp env e) (redCmd env c)
redCmd env (CLet bs c)          = CLet (redBinds env bs) (redCmd env c)
redCmd env (CAss x e c)         = CAss x (redExp env e) (redCmd env c)


redBeta env ((x,t):te) b (e:es)
  | b == EVar x                 = redBeta env te e es                    -- trivial body
  | isGenerated x || simple e   = redBeta ((x,e):env) te b es            -- appears only once(??), strict, no size increase
  | otherwise                   = ELet (Binds False [(x,t)] [(x,e)]) (redBeta env te b es)
redBeta env [] b []             = redExp env b


redSel env (ERec c eqs) s       = case lookup s eqs of
                                    Just e -> redExp env e
                                    Nothing -> error "Internal: redSel"
redSel env e s                  = eAp (ESel s) [e]


redCase env (ECon k,es') alts d es
  | all value es'                = findCon env k es' alts d es
redCase env (ELit l,_) alts d es = findLit env l alts d es
redCase env (e,es') alts d es    = eAp (ECase (eAp e (map (redExp env) es')) (mapSnd (redExp env) alts) (redExp env d)) es


findCon env k es' [] d es       = redApp env d es
findCon env k es' ((PCon k',e):_) d es
  | k == k'                     = redApp env e (es'++es)
findCon env k es' (_:alts) d es = findCon env k es' alts d es


findLit env l [] d es           = redApp env d es
findLit env l ((PLit l',e):_) d es
  | l == l'                     = redApp env e es
findLit env l (_:alts) d es     = findLit env l alts d es


redPrim env Refl _ [e]                          = e
redPrim env Fatbar _ [e,e']                     = redFat env e e'
redPrim env p a [ELit (LInt x), ELit (LInt y)]  = redInt p x y
redPrim env p a [ELit (LRat x), ELit (LRat y)]  = redRat p x y
redPrim env IntNeg _ [ELit (LInt x)]            = ELit (LInt (-x))
redPrim env FloatNeg _ [ELit (LRat x)]          = ELit (LRat (-x))
redPrim env p a es                              = eAp (EVar (Prim p a)) es


redFat env (EVar (Prim Fail _)) e               = e
redFat env (EAp (EVar (Prim Commit _)) [e]) _   = e
redFat env e e'                                 = EAp (EVar (prim Fatbar)) [e,e']


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
