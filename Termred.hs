module Termred(termred, redTerm) where

import Common
import Core
import Env
import Depend
import PP

termred m                       = return (redModule m)


redTerm env e                   = redExp env e


redModule (Module m ds ie bs)   = Module m ds ie' (redBinds env1 bs)
  where env0                    = []
        ie'                     = redBinds env0 ie
        env1                    = f (groupBinds ie')
        f []                    = []
        f (Binds r te eqs : bs) = if r then f bs else eqs ++ f bs


reallySimple (EVar _ _)         = True
reallySimple (ECon _ _)         = True
reallySimple (ELit _)           = True
reallySimple (ESel e _ _)       = reallySimple e
reallySimple (EAp (EVar (Prim _ _) _) [e1,e2])
                                = reallySimple e1 && reallySimple e2
reallySimple (ELam te e)        = reallySimple e && all (`elem` dom te) (evars e)
reallySimple e                  = False

simple (ELam _ e)               = simple e
simple e                        = reallySimple e

value (EVar _ _)                = True
value (ECon _ _)                = True
value (ELit _)                  = True
value (ESel e _ _)              = value e
value (EAp (ECon c _) es)       = all value es
value (ERec _ eqs)              = all (value . snd) eqs
value (ELam _ _)                = True
value _                         = False


redBinds env (Binds r te eqns)  = Binds r te (redEqns env eqns)

redEqns env []                  = []
redEqns env ((x,e):eqns)
  | reallySimple e'             = (x,e') : redEqns ((x,e'):env) eqns
  | otherwise                   = (x,e') : redEqns env eqns
  where e'                      = redExp env e


redExp env (ERec c eqs)         = ERec c (mapSnd (redExp env) eqs)
redExp env (ETempl x t te c)    = ETempl x t te (redCmd env c)
redExp env (EAct e e')          = EAct (redExp env e) (redExp env e')
redExp env (EReq e e')          = EReq (redExp env e) (redExp env e')
redExp env (EDo x t c)          = EDo x t (redCmd env c)
redExp env (ELam te e)          = redEta env te e
redExp env (ESel e s t)         = redSel env (redExp env e) s t
redExp env (ECase e alts d)     = redCase env (eFlat (redExp env e)) alts d
redExp env (ELet bs e)
  | rec                         = ELet bs' (redExp env e)
  | otherwise                   = redBeta env te e (map (lookup' eqs) (dom te))
  where bs'@(Binds rec te eqs)  = redBinds env bs
redExp env e@(EVar (Prim {}) _) = e
redExp env e@(EVar (Tuple {})_) = e
redExp env e@(EVar x _)         = case lookup x env of
                                      Just e' -> e'
                                      Nothing -> e
redExp env (EAp e es)           = redApp env (redExp env e) (map (redExp env) es)
redExp env e                    = e


redApp env (ELam te e) es       = redBeta env te e es
redApp env (ECase e alts d) es  = ECase e (map f alts) (redApp env d es)
  where f (p,e)                 = (p, redApp env e es)
redApp env (ELet bs e) es       = ELet bs (redApp env e es)
redApp env (EVar (Prim p a) _) es 
                                = redPrim env p a es
redApp env e es                 = EAp e es


redEta env te (EAp e es)
  | ok e                        = redExp env e
  where ok (ECon _ _)           = False
        ok (EVar (Prim _ _) _)  = False
        ok _                    = map (redExp env) es == map eVar (dom te)
redEta env te e                 = ELam te (redExp env e)


redCmd env (CRet e)             = CRet (redExp env e)
redCmd env (CExp e)             = CExp (redExp env e)
redCmd env (CGen p t e c)       = CGen p t (redExp env e) (redCmd env c)
redCmd env (CLet bs c)          = CLet (redBinds env bs) (redCmd env c)
redCmd env (CAss x e c)         = CAss x (redExp env e) (redCmd env c)


redBeta env ((x,t):te) (EVar y _) (e:es)
  | x == y                      = redBeta env te e es                    -- trivial body
redBeta env ((x,t):te) b (e:es)
  | isGenerated x || simple e   = redBeta ((x,e):env) te b es            -- appears only once(??), strict, no size increase
  | otherwise                   = ELet (Binds False [(x,t)] [(x,e)]) (redBeta env te b es)
redBeta env [] b []             = redExp env b


redSel env (ERec c eqs) s t
  | all simple (rng eqs)        = case lookup s eqs of
                                    Just e -> e
                                    Nothing -> error "Internal: redSel"
redSel env e s t                = ESel e s t


redCase env (ECon k _,es) alts d
  | all value es                 = findCon env k es alts d
redCase env (ELit l,_) alts d    = findLit env l alts d
redCase env (e,es) alts d        = ECase (eAp e es) (mapSnd (redExp env) alts) (redExp env d)


findCon env k es [] d           = redExp env d
findCon env k es ((PCon k',e):_) d
  | k == k'                     = redExp env e
findCon env k es (_:alts) d     = findCon env k es alts d


findLit env l [] d              = redExp env d
findLit env l ((PLit l',e):_) d 
  | l == l'                     = redExp env e
findLit env l (_:alts) d        = findLit env l alts d


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
