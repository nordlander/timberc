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


simple (EVar _ _)               = True
simple (ECon _ _)               = True
simple (ELit _)                 = True
simple (ESel e _ _)             = simple e
simple (ELam _ e)               = simple e      -- dubious...
simple _                        = False


value (EVar _ _)                = True
value (ECon _ _)                = True
value (ELit _)                  = True
value (ESel e _ _)              = value e
value (EAp (ECon c _) es)       = all value es
value (ERec _ eqs)              = all (value . snd) eqs
value (ELam _ _)                = True
value _                         = False


redBinds env (Binds r te eqns)  = Binds r te (mapSnd (redExp env) eqns)


redExp env e@(ECon _ _)         = e
redExp env e@(ELit _)           = e
redExp env (ERec c eqs)         = ERec c (mapSnd (redExp env) eqs)
redExp env (ETempl x t te c)    = ETempl x t te (redCmd env c)
redExp env (EAct e e')          = EAct (redExp env e) (redExp env e')
redExp env (EReq e e')          = EReq (redExp env e) (redExp env e')
redExp env (EDo x t c)          = EDo x t (redCmd env c)
redExp env (ELam te e)          = redEta env te e
redExp env (EAp e es)           = redApp env e (map (redExp env) es)
redExp env e                    = redApp env e []

redApp env (ELam te e) es       = redBeta env te e es
redApp env (ESel e s t) es      = redSel env (redExp env e) s t es
redApp env (ECase e alts d) es  = redCase env (eFlat (redExp env e)) alts d es
redApp env (ELet bs e) es
  | rec                         = ELet bs' (redApp env e es)
  | otherwise                   = redBeta env te e (map (lookup' eqs) (dom te) ++ es)
  where bs'@(Binds rec te eqs)  = redBinds env bs
redApp env (EVar (Prim p a) _) es 
                                = redPrim env p a es
redApp env (EVar x t) es        = case lookup x env of
                                      Just e -> redApp env e es
                                      Nothing -> eAp (EVar x t) es
redApp env e es                 = eAp (redExp env e) es


redEta env te (EAp e es)
  | es == map eVar (dom te)     = redExp env e
redEta env te e                 = ELam te (redExp env e)


redCmd env (CRet e)             = CRet (redExp env e)
redCmd env (CExp e)             = CExp (redExp env e)
redCmd env (CGen p t e c)       = CGen p t (redExp env e) (redCmd env c)
redCmd env (CLet bs c)          = CLet (redBinds env bs) (redCmd env c)
redCmd env (CAss x e c)         = CAss x (redExp env e) (redCmd env c)


redBeta env ((x,t):te) (EVar x' Nothing) (e:es)
  | x == x'                     = redBeta env te e es                    -- trivial body
redBeta env ((x,t):te) b (e:es)
  | isGenerated x || simple e   = redBeta ((x,e):env) te b es            -- appears only once(??), strict, no size increase
  | otherwise                   = ELet (Binds False [(x,t)] [(x,e)]) (redBeta env te b es)
redBeta env [] b []             = redExp env b


redSel env (ERec c eqs) s t es  = case lookup s eqs of
                                    Just e -> redApp env e es
                                    Nothing -> error "Internal: redSel"
redSel env e s t es             = eAp (ESel e s t) es


redCase env (ECon k _,es') alts d es
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
redPrim env p a es                              = eAp (eVar (Prim p a)) es


redFat env (EVar (Prim Fail _) _) e             = e
redFat env (EAp (EVar (Prim Commit _) _) [e]) _ = e
redFat env e e'                                 = EAp (eVar (prim Fatbar)) [e,e']


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




eBool True                      = eVar (prim TRUE)
eBool False                     = eVar (prim FALSE)
