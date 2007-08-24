module Termred(termred, redTerm) where

import Common
import Core
import Depend
import PP
import Char

termred (_,_,impIe) m           = return (redModule impIe m)


redTerm insts e                 = redExp (Env {eqns = insts, args = []}) e

data Env                        = Env { eqns :: Map Name Exp,
                                        args :: [Name]
                                      }

addArgs env vs                  = env { args = vs ++ args env }

addEqns env eqs                 = env { eqns = eqs ++ eqns env }


redModule impIe (Module m ns ds ie bs) 
                                = Module m ns ds (redBinds env0 ie) (redBinds env1 bs)
  where env0                    = Env { eqns = eqnsOf impIe, args = [] }
        ie'                     = redBinds env0 ie
        env1                    = addEqns env0 (f (groupBinds ie'))
        f []                    = []
        f (Binds r te eqs : bs) = if r then f bs else filter (finite env0 . snd) eqs ++ f bs


-- can be safely ignored without changing cbv semantics
value (EVar _ _)                = True
value (ECon _ _)                = True
value (ELit _)                  = True
value (ESel e _ _)              = value e
value (EAp (EVar (Prim IntDiv _) _) [e1,e2])
                                = value e1 && nonzero e2
value (EAp (EVar (Prim FloatDiv _) _) [e1,e2])
                                = value e1 && nonzero e2
value (EAp (EVar (Prim _ _) _) es)
                                = all value es
value (EAp (EVar (Tuple _ _) _) es)
                                = all value es
value (EAp (ECon c _) es)       = all value es
value (ELam _ _)                = True
value (ERec _ eqs)              = all (value . snd) eqs
value e                         = False

nonzero (ELit (LInt n))         = n /= 0
nonzero (ELit (LRat n))         = n /= 0
nonzero _                       = False


-- may be safely inlined (can't lead to infinite expansion even if part of a recursive binding group)
finite env (EVar (Prim _ _) _)  = True
finite env (EVar (Tuple _ _) _) = True
finite env (EVar x _)           = x `elem` args env
finite env (ECon _ _)           = True
finite env (ELit _)             = True
finite env (ESel e _ _)         = finite env e
finite env (ELam te e)          = finite (addArgs env (dom te)) e
finite env (ERec _ eqs)         = all (finite env) (rng eqs)
finite env (EAp e es)           = all (finite env) (e:es)
finite env (ELet bs e)          = fin bs && finite (addArgs env (bvars bs)) e
  where fin (Binds True _ _)    = False
        fin (Binds _ _ eqns)    = all (finite env . snd) eqns
finite env (ECase e alts d)     = finite env e && all (finite env . snd) alts && finite env d
finite env e                    = False



redBinds env (Binds r te eqns)  = Binds r te (redEqns env eqns)

redEqns env []                  = []
redEqns env ((x,e):eqns)
  | finite env e'               = (x,e') : redEqns (addEqns env [(x,e')]) eqns    -- no risk of infinite inlining
  | otherwise                   = (x,e') : redEqns env eqns
  where e'                      = redExp env e


redExp env (ERec c eqs)         = ERec c (mapSnd (redExp env) eqs)
redExp env (ETempl x t te c)    = ETempl x t te (redCmd env c)
redExp env (EAct e e')          = EAct (redExp env e) (redExp env e')
redExp env (EReq e e')          = EReq (redExp env e) (redExp env e')
redExp env (EDo x t c)          = EDo x t (redCmd env c)
redExp env (ELam te e)          = redEta env te (redExp env e)
redExp env (ESel e s t)         = redSel env (redExp env e) s t
redExp env (ECase e alts d)     = redCase env (redExp env e) alts d
redExp env (ELet bs e)
  | rec                         = ELet bs' (redExp env e)
  | otherwise                   = redBeta env te e (map (lookup' eqs) (dom te))
  where bs'@(Binds rec te eqs)  = redBinds env bs
redExp env e@(EVar (Prim {}) _) = e
redExp env e@(EVar (Tuple {})_) = e
redExp env e@(EVar x _)         = case lookup x (eqns env) of
                                      Just e' -> e'
                                      _       -> e
redExp env (EAp e es)           = redApp env (redExp env e) (map (redExp env) es)
redExp env e                    = e


-- reduce an application e es (head and args already individually reduced)
redApp env (EVar (Prim p a) _) es 
                                = redPrim env p a es
redApp env e@(EVar x _) es      = case lookup x (eqns env) of
                                       Just e' -> redApp env e' es      -- SHOULD REALLY ALPHA-CONVERT HERE!!!
                                       Nothing -> EAp e es
redApp env (ELam te e) es       = redBeta env te e es
redApp env (ECase e alts d) es  = ECase e (map f alts) (redApp env d es)
  where f (p,e)                 = (p, redApp env e es)
redApp env (ELet bs e) es       = ELet bs (redApp env e es)
redApp env e es                 = EAp e es


-- perform beta reduction (if possible)
redBeta env ((x,t):te) (EVar y _) (e:es)
  | x == y                      = redBeta env te e es                      -- trivial body
redBeta env ((x,t):te) b (e:es)
  | isGenerated x               = redBeta (addEqns env [(x,e)]) te b es    -- must be a witness, is a value & appears only once
  | finite env e && value e     = tr' ("Safe: " ++ show x) $ redBeta (addEqns env [(x,e)]) te b es  -- can be safely ignored
  | otherwise                   = tr' ("Unsafe: " ++ show x ++ show e) $ ELet (Binds False [(x,t)] [(x,e)]) (redBeta env te b es)
redBeta env [] b []             = redExp env b


redEta env te (EAp e es)
  | ok e                        = redExp env e
  where ok (ECon _ _)           = False
        ok (EVar (Prim _ _) _)  = False
        ok _                    = map (redExp env) es == map eVar (dom te)
redEta env te e                 = ELam te (redExp (addArgs env (dom te)) e)


redSel env e@(EVar x _) s t     = case lookup x (eqns env) of
                                    Just e' -> redSel env e' s t
                                    Nothing -> ESel e s t
redSel env (ERec c eqs) s t
  | all value (rng eqs)         = case lookup s eqs of
                                    Just e -> e
                                    Nothing -> error "Internal: redSel"
redSel env e s t                = ESel e s t


redCase env e@(EVar x _) alts d = case lookup x (eqns env) of
                                    Just e' -> redCase env e' alts d
                                    Nothing -> ECase e (mapSnd (redExp env) alts) (redExp env d)
redCase env (ELit l) alts d     = findLit env l alts d
redCase env e alts d            = case eFlat e of
                                    (ECon k _, es) -> findCon env k es alts d
                                    _              -> ECase e (mapSnd (redExp env) alts) (redExp env d)


findCon env k es [] d           = redExp env d
findCon env k es ((PCon k',e):_) d
  | k == k'                     = redExp env (eAp e es)
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
redPrim env IntToFloat _ [ELit (LInt x)]        = ELit (LRat (fromInteger x))
redPrim env IntToChar _ [ELit (LInt x)]         = ELit (LChr (chr (fromInteger x)))
redPrim env FloatNeg _ [ELit (LRat x)]          = ELit (LRat (-x))
redPrim env FloatToInt _ [ELit (LRat x)]        = ELit (LInt (truncate x))
redPrim env CharToInt _ [ELit (LChr x)]         = ELit (LInt (toInteger (ord x)))
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


redCmd env (CRet e)             = CRet (redExp env e)
redCmd env (CExp e)             = CExp (redExp env e)
redCmd env (CGen p t e c)       = CGen p t (redExp env e) (redCmd env c)
redCmd env (CLet bs c)          = CLet (redBinds env bs) (redCmd env c)
redCmd env (CAss x e c)         = CAss x (redExp env e) (redCmd env c)


