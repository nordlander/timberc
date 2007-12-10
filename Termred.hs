module Termred(termred, redTerm, finite, env0, constrs) where

import Monad
import Common
import Core
import Depend
import PP
import Char

termred (_,_,bs',is') m         = redModule (eqnsOf is' ++ eqnsOf bs') m


redTerm coercions e             = redExp (Env {eqns = coercions, args = []}) e

data Env                        = Env { eqns :: Map Name Exp,
                                        args :: [Name]
                                      }

env0                            = Env { eqns = [], args = [] }

addArgs env vs                  = env { args = vs ++ args env }

addEqns env eqs                 = env { eqns = eqs ++ eqns env }


redModule impEqs (Module m ns xs ds ie bs)
                                = do es1' <- redEqns env0 es1
                                     let env1 = addEqns env0 (finiteEqns env0 es1')
                                         env2 = addEqns env1 (finiteEqns env1 impEqs)
                                     ie' <- redBinds env2 ie
                                     let env3 = addEqns env2 (finiteEqns env2 (eqnsOf ie'))
                                     es2' <- redEqns env3 es2
                                     let es' = es1' ++ es2'
                                         vs  = idents es'
                                         necessary (v,_) = maybe (elem v vs) (\_ -> True) (fromMod v)
                                     return (Module m ns xs ds ie' (Binds r (filter necessary te) (filter necessary es')))
  where envFree (_,e)           = all isSafeId (idents e) && isSmall e
        Binds r te es           = bs
        (es1,es2)               = partition envFree es
        isSafeId (Prim _ _)     = True
        isSafeId x              = isGenerated x && not(isTemp x)
     
{-
Definition of isSafeId should be reconsidered. Which generated names are safe? 
-}   

finiteEqns env eq               = filter (finite env . snd) eq


-- can be safely ignored without changing cbv semantics
value (EVar _)                  = True
value (ECon _)                  = True
value (ELit _)                  = True
value (ESel e _)                = value e
value (EAp (EVar (Prim IntDiv _)) [e1,e2])
                                = value e1 && nonzero e2
value (EAp (EVar (Prim FloatDiv _)) [e1,e2])
                                = value e1 && nonzero e2
value (EAp (EVar (Prim _ _)) es)
                                = all value es
value (EAp (EVar (Tuple _ _)) es)
                                = all value es
value (EAp (ECon c) es)         = all value es
value (ELam _ _)                = True
value (ERec _ eqs)              = all (value . snd) eqs
value e                         = False

nonzero (ELit (LInt _ n))       = n /= 0
nonzero (ELit (LRat _ n))       = n /= 0
nonzero _                       = False


-- may be safely inlined (can't lead to infinite expansion even if part of a recursive binding group)
finite env (EVar (Prim c _))    = c `notElem` [ListArray, ConstArray, CloneArray, UpdateArray]
finite env (EVar (Tuple _ _))   = True
finite env (EVar x)             = x `elem` args env || maybe False (finite env )(lookup x (eqns env))
finite env (ECon _)             = True
finite env (ELit _)             = True
finite env (ESel e _)           = finite env e
finite env (ELam te e)          = finite (addArgs env (dom te)) e
finite env (ERec _ eqs)         = all (finite env) (rng eqs)
finite env (EAp e es)           = all (finite env) (e:es)
finite env (ELet bs e)          = fin bs && finite (addArgs env (bvars bs)) e
  where fin (Binds True _ _)    = False
        fin (Binds _ _ eqns)    = all (finite env . snd) eqns
finite env (ECase e alts d)     = finite env e && all (finite env . snd) alts && finite env d
finite env e                    = False



redBinds env (Binds r te eqns)  = liftM (Binds r te) (redEqns env eqns)


redEqns env []                  = return []
redEqns env ((x,e):eqns)        = do e' <- redExp env e
                                     let env' = if finite env e' && isSmall e 
                                                then addEqns env [(x,e')]     -- no risk of infinite inlining
                                                else env
                                     liftM ((x,e'):) (redEqns env' eqns)


redExp env (ERec c eqs)         = do es' <- mapM (redExp env) es
                                     return (ERec c (ls `zip` es'))
  where (ls,es)                 = unzip eqs
redExp env (ETempl x t te c)    = liftM (ETempl x t te) (redCmd env c)
redExp env (EAct e e')          = liftM2 EAct (redExp env e) (redExp env e')
redExp env (EReq e e')          = liftM2 EReq (redExp env e) (redExp env e')
redExp env (EDo x t c)          = liftM (EDo x t) (redCmd env c)
redExp env (ELam te e)          = do e <- redExp env e
                                     redEta env te e
redExp env (ESel e s)           = do e <- redExp env e
                                     redSel env e s
redExp env (ECase e alts d)     = do e <- redExp env e
                                     redCase env e alts d
redExp env (ELet bs e)          = do bs'@(Binds rec te eqs) <- redBinds env bs
                                     if rec then
                                        liftM (ELet bs') (redExp env e)
                                      else
                                        redBeta env te e (map (lookup' eqs) (dom te))
redExp env e@(EVar (Prim {}))   = return e
redExp env e@(EVar (Tuple {}))  = return e
redExp env e@(EVar x)           = case lookup x (eqns env) of
                                      Just e' -> alphaConvert e'
                                      _       -> return e
redExp env (EAp e es)           = do e <- redExp env e
                                     es <- mapM (redExp env) es
                                     redApp env e es
redExp env e                    = return e


isRaise (EAp (EVar (Prim Raise _)) [_])
                                = True
isRaise _                       = False


-- reduce an application e es (head and args already individually reduced)
redApp env e es
  | not (null es')              = return (head es')
  where es'                     = filter isRaise (e:es)
redApp env (EVar (Prim p a)) es 
                                = return (redPrim p a es)
redApp env e@(EVar x) es        = case lookup x (eqns env) of
                                       Just e' -> do e' <- alphaConvert e'; redApp env e' es
                                       Nothing -> return (EAp e es)
redApp env (ELam te e) es       = redBeta env te e es
redApp env (ECase e alts d) es  = liftM2 (ECase e) (redAlts env (mapSnd (`EAp` es) alts)) (redApp env d es)
redApp env (ELet bs e) es       = liftM (ELet bs) (redApp env e es)
redApp env e es                 = return (EAp e es)


-- perform beta reduction (if possible)
redBeta env ((x,t):te) (EVar y) (e:es)
  | x == y                      = redBeta env te e es                      -- trivial body
redBeta env ((x,t):te) b (e:es)
  | isGenerated x               = redBeta (addEqns env [(x,e)]) te b es    -- must be a witness, is a value & appears only once
  | finite env e && value e     = redBeta (addEqns env [(x,e)]) te b es    -- can be safely ignored
  | otherwise                   = liftM (ELet (Binds False [(x,t)] [(x,e)])) (redBeta env te b es)
redBeta env [] b []             = redExp env b


redEta env te (EAp e es)        = do es <- mapM (redExp env) es
                                     e <- redExp env e
                                     if okEta e && es == map EVar (dom te) then
                                        return e
                                      else do
                                        liftM (ELam te) (redApp env e es)
  where okEta (ECon _)          = False
        okEta (EVar (Prim _ _)) = False
        okEta _                 = True
redEta env te e                 = liftM (ELam te) (redExp (addArgs env (dom te)) e)


redSel env e s
  | isRaise e                   = return e
redSel env e@(EVar x) s         = case lookup x (eqns env) of
                                    Just e' -> do e' <- alphaConvert e'; redSel env e' s
                                    Nothing -> return (ESel e s)
redSel env (ERec c eqs) s
  | all value (rng eqs)         = case lookup s eqs of
                                    Just e  -> return e
                                    Nothing -> internalError0 "redSel: did not find selector" s
redSel env e s                  = return (ESel e s)


redCase env e alts d
  | isRaise e                   = return e
redCase env e@(EVar x) alts d   = case lookup x (eqns env) of
                                    Just e' -> do e' <- alphaConvert e'; redCase env e' alts d
                                    Nothing -> liftM2 (ECase e) (redAlts env alts) (redExp env d)
redCase env (ELit l) alts d     = findLit env l alts d
redCase env e alts d            = case eFlat e of
                                    (ECon k, es) -> findCon env k es alts d
                                    _            -> liftM2 (ECase e) (redAlts env alts) (redExp env d)

redAlts env alts                = do es <- mapM (redExp env) es
                                     return (ps `zip` es)
  where (ps,es)                 = unzip alts


findCon env k es [] d           = redExp env d
findCon env k es ((PCon k',e):_) d
  | k == k'                     = redExp env (eAp e es)
findCon env k es (_:alts) d     = findCon env k es alts d


findLit env l [] d              = redExp env d
findLit env l ((PLit l',e):_) d 
  | l == l'                     = redExp env e
findLit env l (_:alts) d        = findLit env l alts d


redPrim Refl _ [e]                          = e
redPrim Match a es                          = redMatch a es
redPrim Fatbar a [e,e']                     = redFat a e e'
redPrim ConstArray a es                     = EAp (EVar (Prim ConstArray a)) es
redPrim p _ [ELit (LInt _ x), ELit (LInt _ y)]  = redInt p x y
redPrim p a [ELit (LRat _ x), ELit (LRat _ y)]  = redRat p x y
redPrim IntNeg _ [ELit (LInt _ x)]            = ELit (LInt Nothing (-x))
redPrim IntToFloat _ [ELit (LInt _ x)]        = ELit (LRat Nothing (fromInteger x))
redPrim IntToChar _ [ELit (LInt _ x)]         = ELit (LChr Nothing (chr (fromInteger x)))
redPrim FloatNeg _ [ELit (LRat _ x)]          = ELit (LRat Nothing (-x))
redPrim FloatToInt _ [ELit (LRat _ x)]        = ELit (LInt Nothing (truncate x))
redPrim CharToInt _ [ELit (LChr _ x)]         = ELit (LInt Nothing (toInteger (ord x)))
redPrim p a es                              = eAp (EVar (Prim p a)) es


redMatch a [ELet bs e]                      = ELet bs (redMatch a [e])
redMatch a [EAp (EVar (Prim Commit _)) [e]] = e
redMatch _ [EVar (Prim Fail a)]             = EAp (EVar (Prim Raise a)) [ELit (LInt Nothing 1)]
redMatch _ [e@(ELit _)]                     = e
redMatch a es                               = EAp (EVar (Prim Match a)) es


redFat a (ELet bs e) e'                     = ELet bs (redFat a e e')
redFat a (EVar (Prim Fail _)) e             = e
redFat a (EAp (EVar (Prim Commit _)) [e]) _ = e
redFat a e e'                               = EAp (EVar (Prim Fatbar a)) [e,e']


redInt IntPlus a b              = ELit (LInt Nothing (a + b))
redInt IntMinus a b             = ELit (LInt Nothing (a - b))
redInt IntTimes a b             = ELit (LInt Nothing (a * b))
redInt IntDiv a b               = ELit (LInt Nothing (a `div` b))
redInt IntMod a b               = ELit (LInt Nothing (a `mod` b))
redInt IntEQ a b                = eBool (a == b)
redInt IntNE a b                = eBool (a /= b)
redInt IntLT a b                = eBool (a < b)
redInt IntLE a b                = eBool (a <= b)
redInt IntGE a b                = eBool (a >= b)
redInt IntGT a b                = eBool (a > b)
redInt p _ _                    = internalError0 ("redInt: unknown primitive " ++ show p)


redRat FloatPlus a b            = ELit (LRat Nothing (a + b))
redRat FloatMinus a b           = ELit (LRat Nothing (a - b))
redRat FloatTimes a b           = ELit (LRat Nothing (a * b))
redRat FloatDiv a b             = ELit (LRat Nothing (a / b))
redRat FloatEQ a b              = eBool (a == b)
redRat FloatNE a b              = eBool (a /= b)
redRat FloatLT a b              = eBool (a < b)
redRat FloatLE a b              = eBool (a <= b)
redRat FloatGE a b              = eBool (a >= b)
redRat FloatGT a b              = eBool (a > b)
redRat p _ _                    = internalError0 ("redRat " ++ show p)


eBool True                      = ECon (prim TRUE)
eBool False                     = ECon (prim FALSE)


redCmd env (CRet e)             = liftM CRet (redExp env e)
redCmd env (CExp e)             = liftM CExp (redExp env e)
redCmd env (CGen p t e c)       = liftM2 (CGen p t) (redExp env e) (redCmd env c)
redCmd env (CLet bs c)          = liftM2 CLet (redBinds env bs) (redCmd env c)
redCmd env (CAss x e c)         = liftM2 (CAss x) (redExp env e) (redCmd env c)



-- Constructor presence

isSmall e                       = length (constrs e) < 5

class Constrs a where
    constrs :: a -> [Name]

instance Constrs Binds where
    constrs (Binds rec te eqns) = constrs (map snd eqns)

instance Constrs a => Constrs [a] where
    constrs xs = concatMap constrs xs

instance Constrs Exp where
    constrs (ECon c)             = [c]
    constrs (ESel e l)           = constrs e
    constrs (ELam te e)          = constrs e
    constrs (EAp e e')           = constrs e ++ constrs e'
    constrs (ELet bs e)          = constrs bs ++ constrs e
    constrs (ECase e alts def)   = constrs e ++ constrs alts ++ constrs def
    constrs (ERec c eqs)         = constrs (map snd eqs)
    constrs (EAct e e')          = constrs e ++ constrs e'
    constrs (EReq e e')          = constrs e ++ constrs e'
    constrs (ETempl x t te c)    = constrs c
    constrs (EDo x t c)          = constrs c
    constrs _                    = []

instance Constrs Alt where
    constrs (p,e)                = constrs e


instance Constrs Cmd where
    constrs (CLet bs c)          = constrs bs ++ constrs c
    constrs (CGen x t e c)       = constrs e ++ constrs c
    constrs (CAss x e c)         = constrs e ++ constrs c
    constrs (CRet e)             = constrs e
    constrs (CExp e)             = constrs e

