{-# LANGUAGE PatternGuards #-}

module Scp (
            scp
           ) where

import Core
import Common hiding (renaming, delete)
import PP
import Config
import Depend (groupBinds)

import Debug.Trace (trace)
import Data.Char (ord, chr)
import Control.Monad (mapAndUnzipM)

type InExp = Exp -- _Before_ transformation

type OutExp = Exp -- _After_ transformation
    
type Context = [CFrame]

data CFrame = AppCtxt [InExp]
            | PrimOpCtxt [OutExp] [InExp] -- We need this to return conveniently to the primop
            | CaseCtxt [Alt Exp]
            | SelCtxt Name
              deriving Show

emptyContext :: Context
emptyContext = []

data RhoElement = RhoE {
      freshName :: Name,
--      inFvs :: [Var],
      restExp :: Context,
      headExp :: Exp,
      compTerm :: Exp,
      compSize :: Integer
}
               
data ScpEnv = ScpE {
      hasH :: Bool,
      locEqns :: Eqns,
      locTEnv :: TEnv,
      globEqns :: Eqns,
      globTEnv :: TEnv,
      inSet :: TEnv,
      splitVars :: [Name],
      ls :: Eqns
    }

env0 = ScpE { hasH = False,
                     locEqns = [], locTEnv = [], 
                     globEqns = [], globTEnv = [],
                     inSet = [], splitVars = [], ls = [] }

scp clo (Module _ _ _ _ ds' _ [bs']) m = 
  if doScp clo
    then scpModule (tsigsOf bs') (eqnsOf bs')  m
    else return m

scpModule impCons impEqns (Module m ns xs es ds ie bss) = do
  let env = env0 { locEqns = locEqns', locTEnv = locTEnvs',
                    globEqns = impEqns, globTEnv = impCons }
  bss' <- scpBinds env bss
  return (Module m ns xs es ds ie bss')
  where go [] = ([], [])
        go ((Binds _ te eqns):t) = (eqns ++ eqns', te ++ te')
          where (eqns', te') = go t
        (locEqns', locTEnvs') = go bss

scpBinds _ l@[] = return l
scpBinds env (h:t) = do
  h' <- scpBinds' env h
  t' <- localStore (scpBinds env t)
  return (h' ++ t')

scpBinds' env (Binds r te' eqns)   = do
  mapM_ driveExp eqns
  s <- currentStore
  let (newte1, eqns1') = unzip [((n, te),(n, body)) | (Nothing, te, n, body) <- s ]
      (newte2, eqns2') = unzip $ [((n, te),(n, body)) | (Just _, te, n, body) <- s ]
      newbinds = groupBinds (Binds r newte2 eqns2')
  return (newbinds ++ [Binds r newte1 eqns1'])
   where driveExp (n@(Name n' _ _ _), e) = do
           let Just thist = lookup n te'
           if n' /= "root" -- "eqList" -- n' == "root'"
              then do
                addToStore (Nothing, thist, n, e)
                return ()
              else do
                -- tr ("Driving " ++ show' n ++ " = " ++ show' e)
--                tr ("Globeqns: " ++ show (globEqns env))
                (e', used) <- drive env e emptyContext
                -- tr ("Done driving " ++ show' e')
                addToStore (Nothing, thist, n, e')
                return ()

drive :: ScpEnv -> Exp -> Context -> M (Maybe Exp, Scheme, Name, Exp) (Exp, [(Name, Maybe Exp)])

drive env (ELit l@(LStr _ _)) ((CaseCtxt alts):c) = do
  -- tr ("driveCaseStrLit!")
  driveCaseStrLit env l c alts
drive env (ELit l) ((CaseCtxt alts):c) = do
  drive env (findLit l alts) c
drive env l@(ELit _) context@((PrimOpCtxt oes []):c) 
 | all isValue (tail oes) 
 , not (isCommit (head oes)) = do
  -- tr ("Reducing: " ++ (show' (plug [(PrimOpCtxt oes [])] l)))
  case redPrim (oes ++ [l]) of
    Nothing -> build env l context
    Just e -> drive env e c

drive env v@(EVar (Prim _ _)) context@((AppCtxt _):_) = do
  let (e, context') = makePrimOpCtxt v context
  drive env e context'
drive env e ((PrimOpCtxt [EVar (Prim Commit _)] oes):(PrimOpCtxt [EVar (Prim Match _)] ies):c) = drive env e c
drive env e (c@(PrimOpCtxt [EVar (Prim Commit _)] oes):(PrimOpCtxt [EVar (Prim Fatbar _)] ies):t) = drive env e (c:t)
drive env (EVar (Prim Fail _)) ((PrimOpCtxt [EVar (Prim Fatbar _)] [e2]):c) = 
  drive env e2 c


{- All wrong
drive env (EVar (Prim Refl _)) ((AppCtxt [arg]):c) = drive env arg c
drive env (EVar (Prim Match _)) ((AppCtxt [EVar (Prim Fail n)]):c) = 
  return (EAp (EVar (Prim Raise n)) [ELit (lInt 1)], [])
drive env (EVar (Prim Fatbar _)) ((AppCtxt [e1, (EVar (Prim Fail _))]):c) = 
  drive env e1 c
drive env (EVar (Prim Fatbar _)) ((AppCtxt [l@(EAp (EVar (Prim Commit _)) [arg]), _]):c) = 
  drive env l c
drive env (EVar (Prim LazyAnd _)) ((AppCtxt [ECon (Prim TRUE _), e2]):c) =
  drive env e2 c
drive env (EVar (Prim LazyAnd a)) ((AppCtxt [ECon (Prim FALSE _), e2]):c) = do
  n <- newName "x"
  t <- newTvar Star
  let body = ELet (Binds False [(n, scheme t)] [(n, e2)]) (eBool False)
  drive env body c
drive env (EVar (Prim LazyAnd a)) ((AppCtxt [e1, ECon (Prim TRUE _)]):c) =
  drive env e1 c
drive env (EVar (Prim LazyAnd a)) ((AppCtxt [e1, ECon (Prim FALSE _)]):c) = do
  n <- newName "x"
  t <- newTvar Star
  let body = ELet (Binds False [(n, scheme t)] [(n, e1)]) (eBool False)
  tr ("New lazyAnd body:" ++ show' body)
  drive env body c
drive env (EVar (Prim LazyOr a)) ((AppCtxt [e1, e2]):c) = error ("vojne" ++ show e1 ++ show e2)
-}
drive env (EVar v) context
 | Just body <- maybeInline env v = do
  maybeFold env v context body
 | otherwise = build env (EVar v) context

drive env (ECon n) ((CaseCtxt alts):c) = drive env (findCon n alts) c
drive env (ECon n) ((AppCtxt args):(CaseCtxt alts):context)
 | (ELam te' e) <- findCon n alts = do
  let e' = ELet (Binds False te' (zip (dom te') args)) e
  -- tr ("ECon case" ++ show' e')
  drive env e' context
 | e <- findCon n alts = do
  -- tr ("ECon case FEL" ++ show' e)
  vs <- newNames "x" (length args)
  ts <- newTvars Star (length args)
  let e' = ELet (Binds False (zip vs (map scheme ts)) (zip vs args)) e
  drive env e' context

drive env (ERec nam eqs) ((SelCtxt n):c) = do
  let e = head [e | (n', e) <- eqs, n == n']
  drive env e c

drive env (ELet (Binds _ _ []) body) c = drive env body c
drive env l@(ELet b@(Binds True te eqs) body) c = {- tr' ("eqs: " ++ (show (dom eqs)) ++ "\n" ++ concat (map show' (rng eqs))) $ -}
  drive (env {locEqns = eqs ++ locEqns env, locTEnv = te ++ locTEnv env}) body c
drive env l@(ELet (Binds False (te':te'') ((n, e):t)) b) c = do
  -- tr ("nonrec bind" ++ show' l)
  -- tr ("nonrec' bind:" ++ show' (plug c l))
  if (strict n b && linear n b) || value e || terminates env e
     then drive env (ELet (Binds False te'' t) (subst (n +-> e) b)) c
     else do
         -- tr ("nonval: " ++ show' l)
         (e', used) <- drive env e emptyContext
         -- tr ("e': " ++ show' e')
         let exp = ELet (Binds False te'' t) b
         (rest, used') <- drive (env {inSet = te':inSet env }) exp c
         let ret = ELet (Binds False [te'] [(n, e')]) rest
         return (ret, used' ++ used)


drive env (ELam te e) ((AppCtxt args):c) = 
  drive env (ELet (Binds False te (zip (dom te) args)) e) c
drive env (ELam te e) c = do
  (e', used') <- drive (env {inSet = te ++ inSet env}) e emptyContext
  (e'', used'') <- build env (ELam te e') c
  return (e'', used' ++ used'')

drive env (EAp e args) c = drive env e ((AppCtxt args):c)

drive env (ECase e alts) c = drive env e ((CaseCtxt alts):c)

drive env (ESel e n) c = drive env e ((SelCtxt n):c)

drive env (EAct e1 e2) c = do
  (e1', used1') <- drive env e1 emptyContext
  (e2', used2') <- drive env e2 emptyContext
  (e', used') <- build env (EAct e1' e2') c
  return (e', used' ++ used1' ++ used2')
drive env (EReq e1 e2) c = do
  (e1', used1') <- drive env e1 emptyContext
  (e2', used2') <- drive env e2 emptyContext
  (e', used') <- build env (EReq e1' e2') c
  return (e', used' ++ used1' ++ used2')
drive env e@(ETempl n t te cmd) c = do
  (cmd', used') <- driveCmd (env {inSet = te ++ inSet env}) cmd
  -- tr ("Skickar in: " ++ show' cmd)
  (e', used'') <- build env (ETempl n t te cmd') c
  -- tr ("Fick ut :" ++ show' e')
  return (e', used'' ++ used')
drive env e@(EDo n t cmd) c = do
  (cmd', used') <- driveCmd env cmd
  (e', used'') <- build env (EDo n t cmd') c
  return (e', used'' ++ used')

drive env e c = do
 build env e c
{-                       
drive :: ScpEnv -> Exp -> M (Maybe Exp, Scheme, Name, Exp) (Exp, [(Name, Maybe Exp)])
-- R1
drive env l@(ELit _) = return (ctxt env $ l, [])
-- R2
drive env l@(EVar n) 
  | Just e <- maybeInline env n = maybeFold env l Nothing
  | otherwise = return (ctxt env $ l, [])
-- R3
drive env c@(ECon _) = return (ctxt env $ c, [])
drive env (EAp c@(ECon _) args) = do
  (args', used') <- mapAndUnzipM (drive (env {ctxt = emptyContext})) args
  return (ctxt env $ EAp c args', concat used')
-- R5
drive env (ELam te e) = do
  (e', used') <- drive (env {ctxt = emptyContext, inSet = te ++ inSet env}) e
  return (ctxt env $ ELam te e', used')
-- RXX: Records
drive env (ERec nam eqs) = do
  let (n, e) = unzip eqs
  (e', used') <- mapAndUnzipM (drive (env {ctxt=emptyContext})) e
  return (ctxt env $ ERec nam (zip n e'), concat used')
-- RXX: Selection
drive env l@(ESel (ERec nam eqs) n) = do
--  tr ("ESel erec: " ++ show l)
  let (n', e) = head [(n', e) | (n', e) <- eqs, n == n']
  e' <- drive (env {ctxt = emptyContext}) (ctxt env $ e)
  return e'
drive env l@(ESel (EVar n) s) 
  | Just e <- maybeInline env n = 
    drive (env {ctxt = emptyContext}) (ctxt env $ (ESel e s))
drive env l@(ESel e n) 
  | annoying env l = do
--    tr ("Annoying esel: " ++ show l)
    (e', used') <- drive (env {ctxt = emptyContext}) e
    return (ctxt env $ ESel e' n, used')
  | otherwise = drive (env {ctxt = (\hole -> ctxt env $ ESel hole n)}) e
-- RXX: Matching
drive env (EAp (EVar (Prim Refl a)) [arg]) =
  drive (env {ctxt = emptyContext}) (ctxt env $ arg)
drive env (EAp (EVar (Prim Match _)) [EAp (EVar (Prim Commit _)) [arg]]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ arg)
-- Careful abort if (match (Fail))
drive env l@(EAp (EVar (Prim Match _)) [EVar (Prim Fail n)]) =
  return (EAp (EVar (Prim Raise n)) [ELit (lInt 1)], [])
drive env (EAp (EVar (Prim Match a)) [arg]) = 
  let ctxt' = (\hole -> ctxt env $ EAp (EVar (Prim Match a)) [hole])
  in drive (env {ctxt = ctxt'}) arg
drive env (EAp (EVar (Prim Commit a)) [arg]) = 
  let ctxt' = (\hole -> ctxt env $ EAp (EVar (Prim Commit a)) [hole])
  in drive (env {ctxt = ctxt'}) arg
drive env l@(EAp (EVar (Prim Fatbar _)) [(EVar (Prim Fail _)), e2]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ e2)
drive env (EAp (EVar (Prim Fatbar _)) [e1, (EVar (Prim Fail _))]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ e1)
drive env (EAp (EVar (Prim Fatbar _)) [l@(EAp (EVar (Prim Commit _)) [arg]), _]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ l)
drive env (EAp (EVar (Prim op a)) [ELit (LChr _ l)]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ redChar1 op l)
drive env l@(EAp (EVar (Prim Raise n)) [ELit (LInt _ _)]) = do
  dummy <- newName "intermediate_name"
  case strict dummy (ctxt env $ (EVar dummy)) of
     True -> return (l, [])
     _ -> return (ctxt env $ l, [])
drive env (EAp (EVar (Prim op a)) [ELit (LInt _ l)]) 
  | reducible op = drive (env {ctxt = emptyContext}) (ctxt env $ redInt1 op l)
drive env (EAp (EVar (Prim op a)) [ELit (LRat _ l)]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ redRat1 op l)
drive env l@(EAp (EVar (Prim IntDiv a)) [ELit _, ELit l2])
  | zero l2 = return (ctxt env $ l, [])
drive env l@(EAp (EVar (Prim FloatDiv a)) [ELit _, ELit l2])
  | zero l2 = return (ctxt env $ l, [])
drive env (EAp (EVar (Prim op a)) [ELit (LInt _ l1), ELit (LInt _ l2)]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ redInt2 op l1 l2)
drive env (EAp (EVar (Prim op a)) [ELit (LRat _ l1), ELit (LRat _ l2)]) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ redRat2 op l1 l2)
drive env (EAp (EVar (Prim LazyAnd a)) [ECon (Prim TRUE annot), e2]) =
  drive (env {ctxt = emptyContext}) (ctxt env $ e2)
drive env (EAp (EVar (Prim LazyAnd a)) [ECon (Prim FALSE annot), e2]) = do
  n <- newName "x"
  t <- newTvar Star
  let body = ELet (Binds False [(n, scheme t)] [(n, e2)]) (eBool False)
  drive (env {ctxt = emptyContext}) (ctxt env $ body)
drive env (EAp (EVar (Prim LazyAnd a)) [e1, ECon (Prim TRUE _)]) =
  drive (env {ctxt = emptyContext}) (ctxt env $ e1)
drive env (EAp (EVar (Prim LazyAnd a)) [e1, ECon (Prim FALSE _)]) = do
  n <- newName "x"
  t <- newTvar Star
  let body = ELet (Binds False [(n, scheme t)] [(n, e1)]) (eBool False)
  tr ("New lazyAnd body:" ++ show' body)
  drive (env {ctxt = emptyContext}) (ctxt env $ body)
drive env (EAp (EVar (Prim LazyOr a)) [e1, e2]) = error ("vojne" ++ show e1 ++ show e2)
drive env l@(EAp (EVar (Prim op a)) [e1]) 
  | annoying env l = do
    (e1', u1') <- drive (env {ctxt = emptyContext}) e1
    return (ctxt env $ EAp (EVar (Prim op a)) [e1'], u1')
  | otherwise =
    let ctxt' =  (\hole -> ctxt env $ EAp (EVar (Prim op a)) [hole])
    in drive (env {ctxt = ctxt'}) e1
drive env l@(EAp (EVar (Prim op a)) [e1, e2]) 
  | annoying env l = do
    tr ("Annoying prim: " ++ show' l)
    (e1', u1') <- drive (env {ctxt = emptyContext}) e1
    (e2', u2') <- drive (env {ctxt = emptyContext}) e2
    return (ctxt env $ EAp (EVar (Prim op a)) [e1', e2'], u1' ++ u2')
  | annoying env e1 || isNum e1 = 
    tr' ("Annoying prim2: " ++ show' e1) $
    let ctxt' =  (\hole -> ctxt env $ EAp (EVar (Prim op a)) [e1, hole])
    in drive (env {ctxt = ctxt'}) e2
  | otherwise = 
--    tr' ("Un-Annoying prim: " ++ show' l) $
    let ctxt' =  (\hole -> ctxt env $ EAp (EVar (Prim op a)) [hole, e2])
    in drive (env {ctxt = ctxt'}) e1
drive env l@(EAp (EVar (Prim op a)) [e1, e2, e3]) 
  | annoying env l = do
    (e1', u1') <- drive (env {ctxt = emptyContext}) e1
    (e2', u2') <- drive (env {ctxt = emptyContext}) e2
    (e3', u3') <- drive (env {ctxt = emptyContext}) e3
    return (ctxt env $ EAp (EVar (Prim op a)) [e1', e2', e3'], u1' ++ u2' ++ u3')
  | (annoying env e1 || isNum e1) && (annoying env e2 || isNum e2) = 
    let ctxt' = (\hole -> ctxt env $ EAp (EVar (Prim op a)) [e1, e2, hole])
    in drive (env {ctxt = ctxt'}) e3
  | (annoying env e1 || isNum e1) && (annoying env e3 || isNum e3) = 
    let ctxt' = (\hole -> ctxt env $ EAp (EVar (Prim op a)) [e1, hole, e3])
    in drive (env {ctxt = ctxt'}) e2
  | otherwise = 
    let ctxt' = (\hole -> ctxt env $ EAp (EVar (Prim op a)) [hole, e2, e3])
    in drive (env {ctxt = ctxt'}) e1
drive env e@(EAp (EVar (Prim op a)) args) = do
  error ("Prim opn: " ++ show' e)
drive env l@(EAp (EVar n) args)
  | closed env l = do
    tr ("Closed for some reason: " ++ show l ++ "ASDFSADF" ++ show (ctxt env $ l)) 
    let Just body = maybeInline env n
    fname <- newName "h"
    body' <- alphaConvert body
    let newterm = ctxt env $ EAp body' args
        env' = env {ctxt = emptyContext}
    e' <- drive env' newterm
    return e'
  | Just body <- maybeInline env n = do
    maybeFold env (EVar n) (Just args)
-- R4
  | otherwise = do
--    tr ("Lookup failed: " ++ show l)
    (args', used') <- mapAndUnzipM (drive (env {ctxt=emptyContext})) args
    return (ctxt env $ EAp (EVar n) args', concat used')
drive env l@(EAp (ELam te e) args) = 
--  tr' ("Eap Elam: " ++ show' l) $ 
  drive env (ELet (Binds False te (zip (dom te) args)) e)
drive env c@(EAp e args) 
  | annoying env c = do
    (e', used) <- drive (env {ctxt = emptyContext}) e
    (args', used') <- mapAndUnzipM (drive (env {ctxt = emptyContext})) args
    return (ctxt env $ EAp e' args', used ++ concat used')
  | otherwise = 
    drive (env {ctxt = (\hole -> ctxt env $ EAp hole args)}) e
drive env (ELet (Binds _ _ []) body) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ body)
drive env l@(ELet b@(Binds True te eqs) body) = tr' ("eqs: " ++ (show (dom eqs)) ++ "\n" ++ concat (map show' (rng eqs))) $
  drive (env {ctxt = emptyContext, locEqns = eqs ++ locEqns env, locTEnv = te ++ locTEnv env}) (ctxt env $ body)
drive env l@(ELet (Binds False (te':te'') ((n, e):t)) b) = do
  tr ("nonrec bind" ++ show' l)
  if strict n b || value e || terminates env e
     then drive env (ELet (Binds False te'' t) (subst (n +-> e) b))
     else do
--         tr ("nonval: " ++ show' l)
         (e', used) <- drive (env {ctxt = emptyContext}) e
--         tr ("e': " ++ show' e')
         if False
            then do
                let b' = (ELet (Binds False te'' t) (subst (n +-> e') b))
                tr ("nonval' : " ++ show' b')
                drive (env {ctxt = emptyContext}) (ctxt env $ b')
            else do
                let exp = ctxt env $ ELet (Binds False te'' t) b
                (rest, used') <- drive (env {ctxt = emptyContext, inSet = te':inSet env }) exp
                let ret = ELet (Binds False [te'] [(n, e')]) rest
                return (ret, used' ++ used)
drive env c@(ECase (EVar n) legs) = do
  (legs', used') <- mapAndUnzipM (driveLegCtxtDown env) legs
  return (ECase (EVar n) legs', concat used')
drive env t@(ECase (ECon n) legs) = 
  drive (env {ctxt = emptyContext}) (ctxt env $ findCon n legs)
drive env t@(ECase (EAp (ECon n) args) legs) 
  | (ELam te' e) <- findCon n legs = 
    let e' = ELet (Binds False te' (zip (dom te') args)) e
    in drive (env {ctxt = emptyContext}) (ctxt env $ e')
  | e <- findCon n legs = do
    vs <- newNames "x" (length args)
    ts <- newTvars Star (length args)
    let e' = ELet (Binds False (zip vs (map scheme ts)) (zip vs args)) e
    drive (env {ctxt = emptyContext}) (ctxt env $ e')
drive env e@(ECase (ELit l@(LStr _ _)) legs) = do
  tr ("driveCaseStrLit!")
  driveCaseStrLit env l legs
drive env e@(ECase (ELit l) legs) = do
  drive (env {ctxt = emptyContext}) (ctxt env $ findLit l legs)
drive env c@(ECase cond legs) 
  | annoying env cond = do
     (cond', used') <- drive (env {ctxt = emptyContext}) cond
     (legs', used'')  <- mapAndUnzipM (driveLegCtxtDown env) legs
     return (ECase cond' legs',  used' ++ concat used'')   
  | otherwise = drive (env {ctxt = (\hole -> ctxt env $ ECase hole legs)}) cond
drive env (EAct e1 e2) = do
  (e1', used1') <- drive (env {ctxt = emptyContext}) e1
  (e2', used2') <- drive (env {ctxt = emptyContext}) e2
  return (ctxt env $ EAct e1' e2', used1' ++ used2')
drive env (EReq e1 e2) = do
  (e1', used1') <- drive (env {ctxt = emptyContext}) e1
  (e2', used2') <- drive (env {ctxt = emptyContext}) e2
  return (ctxt env $ EReq e1' e2', used1' ++ used2')
drive env e@(ETempl n t te cmd) = do
  (cmd', used') <- driveCmd (env {ctxt = emptyContext, inSet = te ++ inSet env}) cmd
  return (ctxt env $ ETempl n t te cmd', used')
drive env e@(EDo n t cmd) = do
  (cmd', used') <- driveCmd (env {ctxt = emptyContext}) cmd
  return (ctxt env $ EDo n t cmd', used')
drive env e = error ("No matching rule " ++ show' e)


driveLegCtxtDown env (Alt (PCon n' te) e) = do
  let freshE = ctxt env $ e
--  tr ("Leg: " ++ show' freshE)
  (e', used) <- drive (env {ctxt = emptyContext, inSet = te ++ inSet env}) freshE
  return (Alt (PCon n' te) e', used)
{-
driveLegCtxtDown env (PCon n', e) = do
  let freshE = ctxt env $ e
--  tr ("Leg: " ++ show' freshE)
  (e', used) <- drive (env {ctxt = emptyContext}) freshE
  return ((pCon0 n', e'), used)
-}
driveLegCtxtDown env (Alt (PLit l) e) = do
  let freshE = ctxt env $ e
--  tr ("Leg: " ++ show' freshE)
  (e', used) <- drive (env {ctxt = emptyContext}) freshE
  return (Alt (PLit l) e', used)
driveLegCtxtDown env (Alt PWild (ELam te e)) = error ("PWild " ++ show' e)
driveLegCtxtDown env (Alt PWild e) = do
  let freshE = ctxt env $ e
--   tr ("Leg: " ++ show' freshE)
  (e', used) <- drive (env {ctxt = emptyContext}) freshE
  return (Alt PWild e', used)
-}

driveApp :: ScpEnv -> Name -> Context -> Exp -> M (Maybe Exp, Scheme, Name, Exp) (Exp, [(Name, Maybe Exp)])
driveApp env fun context body
  | Just body <- maybeInline env fun = do
      memo <- currentStore
      let l' = plug context (EVar fun)
          nont = homemb (locEqns env ++ globEqns env) (ls env) memo l'
      -- tr ("driveApp: " ++ show' l')
      -- tr ("driveApp2:" ++ show (plug context body))
--      tr ("driveApp: " ++ show (ls env))
      if not $ null nont
        then do
           -- tr ("Whistle: " ++ show' l')
--           tr ("Exp: " ++ show' l)
           -- tr ("Whistle mot: " ++ show' (head nont))
           haveWhistled nont l' fun
        else do
--           tr ("Not homemb: " ++ show' l')
--           tr ("ls: " ++ concat (map show' (map snd (ls env))))
           fname <- newName "h"
           body' <- alphaConvert body
           let env' = env { ls = (fname, l'):ls env }
           (e', used) <- drive env' body' context
           let w = [e | (n, Just e) <- used, fname == n]
               found = [n | (n, Nothing) <- used, fname == n]
           if not $ null w
              then gen env l' (head w)
              else do
                 if not $ null found
                    then do
                      let fvs = fv env l'
                          bodyfvs = fv env e' \\ fvs
                      t <- newTvar Star
                      xs <- newNames "x" (length fvs)
                      schemes <- constructTypes env fvs
                      let fscheme = case null fvs of
                                      True -> Scheme (R t) [] []
                                      False -> Scheme (F schemes (R t)) [] []
                          params = map EVar fvs
                          xs' = map EVar xs
                      newbody <- alphaConvert $ case null fvs of
                                                  True -> e'
                                                  False -> ELam (zip xs schemes)(subst (zip fvs xs') e')
                      let newterm = case null fvs of
                                      True -> EVar fname
                                      False -> EAp (EVar fname) params
--                      tr ("Foldar, kropp: " ++ show newbody)
--                      tr ("Foldar, fv: " ++ show fvs)
                      -- tr ("Foldar: " ++ show' l' ++ " to " ++ show fname)
                      -- tr ("New body:" ++ show' newbody)
--                      tr ("Adding': " ++ show (fname, fscheme))
                      case null bodyfvs of
                        True -> do
                          addToStore (Just l', fscheme, fname, newbody)
                          return (newterm, used)
                        False -> do
                          let ret = ELet (Binds True [(fname, fscheme)] [(fname, newbody)]) newterm
                          return (ret, used)
                    else return (e', used)
      where haveWhistled nont l' fun = do
              ml <- currentStore
              let 
                  gf = locEqns env ++ globEqns env
                  refl = [(n, e) | (n, e) <- nont, isHomemb gf l' e]
              if not $ null refl
                 then do
                   error "Backtracking, crap"
                   ret <- newName "intermediate_name"
                   return (EVar ret, [(fst . head $ refl, Just l')])
                 else do
                   -- Kolla upp typen for g
                   ret <- gen env l' (snd . head $ nont)
--                   tr ("haveWhistled: " ++ show' (fst ret))
                   return ret

  

plug [] e = e
plug ((AppCtxt args):c) e = plug c (EAp e args)
plug ((PrimOpCtxt oes ies):c) e = plug c (EAp (head oes) (tail oes ++ e:ies))
plug ((CaseCtxt alts):c) e = plug c (ECase e alts)
plug ((SelCtxt n):c) e = plug c (ESel e n)

makePrimOpCtxt v ((AppCtxt args):c) = (head args, (PrimOpCtxt [v] (tail args)):c)

{-
splitCaseCtxt _ [] = Nothing
splitCaseCtxt e ((AppCtxt args):c) = splitCaseCtxt (EAp e args) c
splitCaseCtxt _ ((PrimOpCtxt {}):_) = Nothing
splitCaseCtxt e c@((CaseCtxt {}):_) = Just (e, c)
splitCaseCtxt e ((SelCtxt _):_) = Nothing
-}

maybeFold env fun myctxt e = do
  memo <- currentStore
  let l' = plug myctxt (EVar fun)
      res = renamings (locEqns env ++ globEqns env) (ls env) memo l'
  if not $ null res
    then do
      -- tr ("Folding: " ++ show' l')
      let n' = head res
          fvs = fv env l'
          params = map EVar fvs
          newterm = if null fvs then EVar n' else EAp (EVar n') params
--         tr ("Folding, fvs: " ++ show n' ++ " " ++ show fvs)
      return (newterm, [(n', Nothing)])
    else driveApp env fun myctxt e

driveAlt env c (Alt p e) = do
  (e', used') <- drive (env {inSet = findBs p ++ inSet env}) e c
  return (Alt p e', used')

findBs (PCon _ te) = te
findBs _ = []

build env e ((PrimOpCtxt oes ies):c)
 | hasH env = do
  (ies', used) <- mapAndUnzipM (\e -> drive (env {hasH = False}) e emptyContext) ies
  (e', used') <- build (env {hasH = False}) (EAp (head oes) (tail oes ++ e:ies')) c
  return (e', used' ++ concat used)
 | null ies = build env (EAp (head oes) (e:tail oes)) c
 | otherwise = drive env (head ies) ((PrimOpCtxt (oes ++ [e]) (tail ies)):c)
build env e ((AppCtxt args):c) = do
  (args', used') <- mapAndUnzipM (\e -> drive env e emptyContext) args
  (e', used'') <- build (env {hasH = True}) (EAp e args') c
  return (e', used'' ++ concat used')
build env e@(EVar v) context@((CaseCtxt alts):c) = do
  -- XXXpj: Substa konstrueraren
  (alts', used') <- mapAndUnzipM (driveAlt env c) alts
  return (ECase e alts', concat used')
build env e context@((CaseCtxt alts):c) = do
  (alts', used') <- mapAndUnzipM (driveAlt env c) alts
  return (ECase e alts', concat used')
build env e ((SelCtxt n):c) = build env (ESel e n) c
build _ e [] = return (e, [])

gen env e1 e2 = do
--  tr ("Generalize : \n" ++ show' e1 ++ " vs\n" ++ show' e2)
  t <- newTvar Star
  res@(ground, s, ts) <- msg env (e1, e2, scheme t)
  (term, s', ts') <- case ground of
                   EVar n -> split' e1
                   _ -> return res
  let (ns, exps) = unzip s'
      env' = env { -- ctxt = emptyContext,
                 inSet = zip ns ts' ++ inSet env,
                 splitVars = ns ++ splitVars env}
  -- tr ("msg ground: " ++ show' term)
  -- tr ("msg subst: " ++ concat (map show' exps))
  (term', used) <- drive env' term emptyContext
--  tr ("msg groundD: " ++ show' term')
--  (exps', used') <- mapAndUnzipM (drive (env {ctxt = emptyContext, splitVars = ns ++ splitVars env})) exps
  (exps', used') <- mapAndUnzipM (\e -> drive (env {splitVars = ns ++ splitVars env}) e emptyContext) exps
--  tr ("gen0: " ++ show term)
--  tr ("gen1: " ++ show exps)
--  tr ("gen2: " ++ show exps')
  let rterm = subst (zip ns exps') term'
--  tr ("gen3: " ++ show' rterm)
  return (rterm, used ++ concat used')

split' c@(ECon {}) = return (c, nullSubst, [])
split' (ESel e n) = do
  x <- newName "intermediate_name"
  t <- newTvar Star
  return (ESel (EVar x) n, [(x, e)], [scheme t])
split' c@(EVar {}) = return (c, nullSubst, [])
split' (ELam te e) = do
  x <- newName "intermediate_name"
  t <- newTvar Star
  return (ELam te (EVar x), [(x, e)], [scheme t])
split' (EAp e args) = do
  -- XXXpj: Kolla upp typinfo om e = g.
  x <- newName "intermediate_name"
  t <- newTvar Star
  xs <- newNames "intermediate_name" (length args)
  ts <- newTvars Star (length args)
  return (EAp (EVar x) (map EVar xs), (x, e):zip xs args, (scheme t):map scheme ts)
split' (ELet (Binds rec te eq) e) = do
  x <- newName "intermediate_name"
  xs <- newNames "intermediate_name" (length eq)
  t <- newTvar Star
  ts <- newTvars Star (length eq)
  let (ns, args) = unzip eq
  return (ELet (Binds rec te (zip ns (map EVar xs))) (EVar x), (x, e):zip xs args, map scheme (t:ts))
split' (ECase e alts) = do
  x <- newName "intermediate_name"
--  xs <- newNames "intermediate_name" (length alts)
  t <- newTvar Star
--  ts <- newTvars Star (length alts)
  let -- p = map separatePat alts
--      (pats, legs) = unzip p
--      alts' = zipWith mergePat p (map EVar xs)
--  return (ECase (EVar x) alts', (x,e):zip xs legs, map scheme (t:ts))
  return (ECase (EVar x) alts, [(x,e)], [scheme t])
  where --separatePat (Alt (PCon k) (ELam te e)) = ((pCon0 k, Just te), e)
        separatePat (Alt (PCon k te) e) = ((pCon0 k, Just te), e)
        separatePat (Alt k e) = ((k, Nothing), e)
        --mergePat ((PCon k [], Just te), e) x = Alt (pCon0 k) (ELam te x)
        mergePat ((PCon k [], Just te), e) x = Alt (PCon k te) x
        mergePat ((k, Nothing), e) x = Alt k x
split' (ERec n eqns) = do
  xs <- newNames "intermediate_name" (length eqns)
  t <- newTvar Star
  let (ns, exps) = unzip eqns
  return (ERec n (zip ns (map EVar xs)), zip xs exps, [scheme t])
split' l@(ELit {}) = return (l, nullSubst, [])
split' (EAct e1 e2) = do
  x1 <- newName "intermediate_name"
  x2 <- newName "intermediate_name"
  ts <- newTvars Star 2
  return (EAct (EVar x1) (EVar x2), [(x1, e1), (x2, e2)], map scheme ts)
split' (EReq  e1 e2) = do
  x1 <- newName "intermediate_name"
  x2 <- newName "intermediate_name"
  ts <- newTvars Star 2
  return (EReq (EVar x1) (EVar x2), [(x1, e1), (x2, e2)], map scheme ts)
split' t@(ETempl {}) = return (t, nullSubst, [])
split' d@(EDo {}) = return (d, nullSubst, [])


constructTypes env xs = mapM refresh ts
  where ts = map (lookup' (inSet env)) xs

maybeTypes env n
  | Just ts <- lookup n (locTEnv env) = Just ts
  | Just ts <- lookup n (globTEnv env) = Just ts
  | otherwise = Nothing

maybeInline env n 
  | Just e <- lookup n (locEqns env) = Just e
  | Just e <- lookup n (globEqns env) = Just e
  | otherwise = Nothing -- tr' ("Not found: " ++ show n) $ Nothing

driveCmd env (CGen n t e c) = do
  -- tr ("CGen: " ++ show n ++ " = " ++ show' e)
  (e', used') <- drive env e emptyContext
  (c', used'') <- driveCmd env c
  return (CGen n t e' c', used' ++ used'')
driveCmd env (CAss n e c) = do
  -- tr ("CAss: " ++ show n ++ " = " ++ show' e)
  (e', used') <- drive env e emptyContext
  -- tr ("CAss: drivad: " ++ show' e')
  (c', used'') <- driveCmd env c
  return (CAss n e' c', used' ++ used'')
-- Non-recursive monadic let
driveCmd env (CLet (Binds False te eqs) c) = do
  let (names, eqs') = unzip eqs
      env' = env {inSet = te ++ inSet env}
--      env' = env {ctxt = emptyContext, inSet = te ++ inSet env}
  -- tr ("CLet: " ++ show names)
  (exps, used') <- mapAndUnzipM (\e -> drive env' e emptyContext) eqs'
  let eqs' = zip names exps
  -- XXXpj: GLOM INTE EMPTYCONTEXT
  (c', used) <- driveCmd env' c
  return (CLet (Binds False te eqs') c', used ++ concat used')
driveCmd env (CLet bs c) = do
  -- tr ("Dynga")
  -- XXXpj: Fixa f�r letrec
  -- bs' <- drive env bs
  (c', used'') <- driveCmd env c
  return (CLet bs c', used'')
driveCmd env (CRet e) = do
  -- tr ("CRet: " ++ show' e)
  (e', used') <- drive env e emptyContext
  return (CRet e', used')
driveCmd env (CExp e) = do
  -- tr ("CExp: " ++ show' e)
  (e', used') <- drive env e emptyContext
  return (CExp e', used')
driveCmd env (CCase e alts) = error "driveCmd: cCase"

--homemb :: Eqns -> Eqns -> Eqns -> Exp -> Eqns
homemb gf ls ml exp = hembsM ++ hembsL
    where -- hembsM = [(n, e) | (Just e, n, body) <- ml, isHomemb gf e exp]
          hembsM = []
          hembsL = [(n, e) | (n, e) <- ls, isHomemb gf e exp]

isHomemb gf e1 e2 = peel gf e1 e2 || any (isHomemb gf e1) (dive e2)

dive (ESel e _) = [e]
dive (ELam _ e) = [e]
dive (EAp e1 args) = e1:args
dive (ELet (Binds _ _ eqs) body) = rng eqs ++ [body]
dive (ECase cond legs) = cond:altRhss legs
dive (ERec _ eqs) = rng eqs
dive (EAct e1 e2) = [e1, e2]
dive (EReq e1 e2) = [e1, e2]
dive (ETempl _ _ _ c) = diveCmd c
dive (EDo _ _ c) = diveCmd c
dive _ = []

diveCmd (CGen _ _ e c) = dive e ++ diveCmd c
diveCmd (CAss _ e c) = dive e ++ diveCmd c
diveCmd (CLet (Binds _ _ eqs) c) = rng eqs ++ diveCmd c
diveCmd (CRet e) = dive e
diveCmd (CExp e) = dive e

peel gf (EVar v1) (EVar v2) 
  | v1 == v2 = True
  | isJust (lookup v1 gf) || isJust (lookup v2 gf) = False
  | otherwise = True
peel gf (ELit l1) (ELit l2) = True
peel gf (ECon n1) (ECon n2) = n1 == n2
peel gf (ESel e1 n1) (ESel e2 n2) = n1 == n2 && isHomemb gf e1 e2 
peel gf (ELam te1 e1) (ELam te2 e2) 
  | length te1 == length te2 = isHomemb gf e1 e2
  | otherwise = False
peel gf (EAp e1 args1) (EAp e2 args2) 
  | length args1 == length args2 = 
      isHomemb gf e1 e2 && and (zipWith (isHomemb gf) args1 args2)
  | otherwise = False
peel gf (ELet (Binds r1 te1 eq1) e1) (ELet (Binds r2 te2 eq2) e2) 
  | r1 == r2 && length eq1 == length eq2 && dom te1 == dom te2 =
      isHomemb gf e1 e2 && and (zipWith (isHomemb gf) (rng eq1) (rng eq2))
  | otherwise = False
peel gf (ECase c1 legs1) (ECase c2 legs2) 
  | length legs1 == length legs2 = isHomemb gf c1 c2 && 
      and (zipWith (isHomemb gf) (altRhss legs1) (altRhss legs2))
  | otherwise = False
-- XXXpj eqs kan vara permuterade
peel gf (ERec n1 eqs1) (ERec n2 eqs2) = n1 == n2
peel gf (EAct e1 e2) (EAct e1' e2') = isHomemb gf e1 e1' && isHomemb gf e2 e2'
peel gf (EReq e1 e2) (EReq e1' e2') = isHomemb gf e1 e1' && isHomemb gf e2 e2'
peel gf (ETempl n1 t1 te1 c1)  (ETempl n2 t2 te2 c2) = n1 == n2 
peel gf (EDo n1 t1 c1) (EDo n2 t2 c2) = n1 == n2 
peel _ _ _ = False

msg env p@(e1, _, _) = {- tr' ("msg infvs: " ++ show (fv env e1) ++ show' e1) $ -} msg' env (fv env e1) p

-- The substitution returned here has a few special properties that 
-- are cruicial for it to work. All the names are fresh - hence we can
-- just concatenate substitutions together. 
msg' env infvs (ECon n1, ECon n2, s)
  | n1 == n2 = return (ECon n1, [], [])
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, ECon n1)], [s])
msg' env infvs (ESel e1 n1, ESel e2 n2, s) 
  | n1 == n2 = return (ESel e1 n1, [], [])
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, ESel e1 n1)], [s])
msg' env infvs (EVar n1, EVar n2, s)
  | n1 == n2 = return (EVar n1, [], [])
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, EVar n1)], [s])
msg' env infvs (ELam te1 e1, ELam te2 e2, s) 
  | length te1 == length te2 && dom te1 == dom te2 = do
       t <- newTvar Star
       let env' = env {inSet = te1 ++ inSet env }
       (e', s1, t1) <- msg' env' infvs (e1, e2, scheme t)
       return (ELam te1 e', s1, t1)
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, ELam te1 e1)], [s])
msg' env infvs (EAp (EVar n1) args1, EAp (EVar n2) args2, s)
  | n1 == n2 && length args1 == length args2
  , Just (Scheme (F schemes (R t)) _ _) <- maybeTypes env n1 = do
--  , Just e <- maybeInline env n1 = do
--    let Just (Scheme (F schemes (R t)) _ _) = maybeTypes env n1
    tmp <- mapM (msg' env infvs) (zip3 args1 args2 schemes)
    let (args, s2, t2) = unzip3 tmp
    return (EAp (EVar n1) args, concat s2, concat t2)
  | n1 == n2 && length args1 == length args2 = do
    ts <- newTvars Star (length args1)
    tmp <- mapM (msg' env infvs) (zip3 args1 args2 (map scheme ts))
    let (args, s2, t2) = unzip3 tmp
    return (EAp (EVar n1) args, concat s2, concat t2)
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, EAp (EVar n1) args1)], [s])
-- msg' env infvs (EAp e1 args1, EAp e2 args2, s)
--   | length args1 == length args2 = do
--     tr ("msg, eap: " ++ show' (EAp e1 args1) ++ " : " ++ show' (EAp e2 args2))
--     -- XXXpj: msg, kolla upp typ i locEqns for e1/e2
--     t <- newTvar Star
--     ts <- newTvars Star (length args1)
--     (e1', s1, t1) <- msg' env infvs (e1, e2, scheme t)
--     tmp <- mapM (msg' env infvs) (zip3 args1 args2 (map scheme ts))
--     let (args, s2, t2) = unzip3 tmp
--     return (EAp e1' args, s1 ++ concat s2, t1 ++ concat t2)
--   | otherwise = do
--     n <- newName "tmp" 
--     return (EVar n, [(n, EAp e1 args1)], [s]) 
msg' env infvs (ELet (Binds r1 te1 eq1) e1, ELet (Binds r2 te2 eq2) e2, s) = error "msg, ELet"
-- XXXpj FIXME Bleh. [(p, e)]
msg' env infvs (ECase c1 legs1, ECase c2 legs2, s) 
  | altPats legs1 == altPats legs2 = do
    t <- newTvar Star
    (c', s1, t1) <- msg' env infvs (c1, c2, scheme t)
    ts <- newTvars Star (length legs1)
    let par = zip3 legs1 legs2 (map scheme ts)
    tmp <- mapM (msgAlt env infvs) par
--     let (legs1', legs2') = unzip (zipWith separatePats legs1 legs2)
--     tmp <- mapM (msg' env infvs) (zip3 legs1' legs2' (map scheme ts))
    let (legs', s2, t2) = unzip3 tmp
--         tmp' = (map collectLambdas legs1)
        s2' = concat s2
--    return (ECase c' (zipWith (mergePats s2') tmp' legs'), s1 ++ s2', t1 ++ concat t2)
    return (ECase c' legs', s1 ++ s2', t1 ++ concat t2)
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, ECase c1 legs1)], [s])
--     where --separatePats (Alt (PCon k) (ELam te1 e1)) (Alt (PCon _) (ELam te2 e2)) = (e1, e2)
--           separatePats (Alt (PCon k te1) e1) (Alt (PCon _ te2) e2) = (e1, e2)
--           separatePats (Alt k e1) (Alt _ e2) = (e1, e2)
--           mergePats s (p, Just (ELam te1 e1)) e = tr' ("mPats: " ++ show te1') $ (Alt p (ELam te1' e))
--               where te1' = updateTe s te1 
--                     updateTe [] te = te
--                     updateTe ((n, EVar x):t) te = tr' ("Substar: " ++ show n ++ " for " ++ show x) $ updateTe t (updateTe' x n te)
--                     updateTe (h:t) te = updateTe t te
--                     updateTe' _ _ [] = []
--                     updateTe' x x' ((n, v):t) | x == n = (x', v):updateTe' x x' t
--                                               | otherwise = (n, v):updateTe' x x' t
--           mergePats s (p, Nothing) e = Alt p e
--           --collectLambdas (PCon k, ELam te1 e1) = (pCon0 k, Just (ELam te1 e1))
--           collectLambdas (Alt (PCon k te1) e1) = (pCon0 k, Just (ELam te1 e1))
--           collectLambdas (Alt k e) = (k, Nothing)
msg' env infvs (ERec n1 eq1, ERec n2 eq2, s)
  | n1 == n2 = return (ERec n1 eq1, [], [])
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, ERec n1 eq1)], [s])
msg' env infvs (ELit l1, ELit l2, s) 
  | l1 == l2 = return (ELit l1, [], [])
--   | otherwise = do
--     n <- newName "tmp"
--     return (EVar n, [(n, ELit l1)], [s])
msg' env infvs (EAct e1 e2, EAct e1' e2', s) = do
  t1 <- newTvar Star
  t2 <- newTvar Star
  (e', s1, t1') <- msg' env infvs (e1, e1', scheme t1)
  (e'', s2, t2') <- msg' env infvs (e2, e2', scheme t2)
  return (EAct e' e'', s1 ++ s2, t1' ++ t2')
msg' env infvs (EReq e1 e2, EReq e1' e2', s) = do
  t1 <- newTvar Star
  t2 <- newTvar Star
  (e', s1, t1') <- msg' env infvs (e1, e1', scheme t1)
  (e'', s2, t2') <- msg' env infvs (e2, e2', scheme t2)
  return (EReq e' e'', s1 ++ s2, t1' ++ t2')
msg' env infvs (ETempl n1 t1 te1 c1, ETempl n2 t2 te2 c2, s) = error "msg, Etempl"
msg' env infvs (EDo n1 t1 c1, EDo n2 t2 c2, s) 
 | n1 == n2 = do
   t1' <- newTvar Star
   (c1', s1', t1'') <- msgCmd env infvs (c1, c2, scheme t1')
   return (EDo n1 t1 c1', s1', t1'')
 | otherwise = error "msg, edo"
msg' env infvs (e1, e2, s) = do
  n <- newName "tmp"
  let fvs' = delete infvs (fv env e1)
  te <- findTe env fvs'
  let lterm = mkLams te e1
      newterm = mkApps (EVar n) (map EVar fvs')
  -- tr ("Msg: " ++ show' e1 ++ " and " ++ show' e2)
  -- tr ("MsgGLONK: " ++ show fvs' ++ show infvs ++ show' lterm ++ " and " ++ show' newterm)
  return (newterm, [(n, lterm)], [s])

findTe env [] = return []
findTe env (h:t) 
  | Just s <- lookup h (inSet env) = do
   ret <- findTe env t
   return ((h, s):ret)
  | otherwise = error "findTe: otheriwse" -- trace ("Did not find " ++ show h) $ findTe env t

msgAlt env infvs (Alt (PCon k te1) e1, Alt (PCon _ te2) e2, s) = do
  let env' = env {inSet = te1 ++ inSet env }
  (e1', s', t) <- msg' env' infvs (e1, e2, s)
  return (Alt (PCon k te1) e1', s', t)
msgAlt env infvs (Alt PWild e1, Alt PWild e2, s) = do
  (e1', s', t) <- msg' env infvs (e1, e2, s)
  return (Alt PWild e1', s', t)
msgAlt env infvs (Alt (PLit l) e1, Alt (PLit l') e2, s) = do
  (e1', s', t) <- msg' env infvs (e1, e2, s)
  return (Alt (PLit l) e1', s', t)

msgCmd env infvs (CGen n1 t1 e1 c1, CGen n2 t2 e2 c2, s) 
 | n1 == n2 = do
   t1' <- newTvar Star
   t2' <- newTvar Star
   (e1', s1', t1'') <- msg' env infvs (e1, e2, scheme t1')
   (c1', s2', t2'') <- msgCmd env infvs (c1, c2, scheme t2')
   return (CGen n1 t1 e1' c1', s1' ++ s2', t1'' ++ t2'')
 | otherwise = error "msgCmd, Cgen"
msgCmd env infvs (CAss n1 e1 c1, CAss n2 e2 c2, s) 
 | n1 == n2 = do
   t1' <- newTvar Star
   t2' <- newTvar Star
   (e1', s1', t1'') <- msg' env infvs (e1, e2, scheme t1')
   (c1', s2', t2'') <- msgCmd env infvs (c1, c2, scheme t2')
   return (CAss n1 e1' c1', s1' ++ s2', t1'' ++ t2'')
 | otherwise = error "msgCmd, Cgen"
msgCmd env infvs (CLet bs1 c1, CLet bs2 c2, s) = error "msgCmd: CLet"
msgCmd env infvs (CRet e1, CRet e2, s) = do
  t <- newTvar Star
  (e', s', t') <- msg' env infvs (e1, e2, scheme t)
  return (CRet e', s', t')
msgCmd env infvs (CExp e1, CExp e2, s) = do
  t <- newTvar Star
  (e', s', t') <- msg' env infvs (e1, e2, scheme t)
  return (CExp e', s', t')
msgCmd env infvs (e1, e2, s) = error "msgCmd, e1 e2"
-- msgCmd env infvs (e1, e2, s) = do
--   n <- newName "tmp"
--   return (EVar n, [(n, e1)], [s])

renaming gf e1 e2 
  | isJust s = subst s' e2 == e2
  | otherwise = False
    where s = isCommonExp gf [(e1, e2)]
          s' :: Map Name Exp
          s' = fromJust s

isCommonExp gf [] = Just nullSubst
isCommonExp gf ((ECon n1, ECon n2):t) | n1 == n2 = isCommonExp gf t
                                      | otherwise = Nothing
isCommonExp gf ((ESel e1 n1, ESel e2 n2):t) 
  | n1 == n2 = isCommonExp gf ((e1, e2):t)
  | otherwise = Nothing
isCommonExp gf ((EVar n1, EVar n2):t) 
  | n1 == n2 = isCommonExp gf t
  | isJust (lookup n1 gf) || isJust (lookup n2 gf) = Nothing
  | isJust s' = Just (s @@ fromJust s')
  | otherwise = Nothing
  where --s ::  Map Name (Maybe Rho -> Exp)
        s = n1 +-> EVar n2
        s' = isCommonExp gf (map (subst s) t)
isCommonExp gf ((ELam te1 e1, ELam te2 e2):t) = isCommonExp gf ((e1, e2'):t)
    where e2' = substList tep te2p e2
          tep = map fst te1
          te2p = map fst te2
isCommonExp gf ((EAp e1 args1, EAp e2 args2):t)  
  | length args1 == length args2 = isCommonExp gf ((e1, e2):zip args1 args2 ++ t)
  | otherwise = Nothing
isCommonExp gf ((ELet (Binds r1 te1 eq1) e1, ELet (Binds r2 te2 eq2) e2):t) 
  | r1 == r2 && 
     length eq1 == length eq2 = isCommonExp gf (neweqs ++ ((e1, e2):t))
  | otherwise = Nothing
      where neweqs = zip (map snd eq1) (map snd eq2)
isCommonExp gf ((ECase cond1 legs1, ECase cond2 legs2):t) 
  | p1 == p2 = isCommonExp gf ((cond1, cond2):newlegs ++ t)
  | otherwise = Nothing
    where newlegs = zip (altRhss legs1) (altRhss legs2)
          p1 = altPats legs1
          p2 = altPats legs2
isCommonExp gf ((ERec n1 eq1, ERec n2 eq2):t) 
  | n1 == n2 && 
     length eq2 == length eq2 = isCommonExp gf (neweqs ++ t)
  | otherwise = Nothing
         where neweqs = zip (rng eq1) (rng eq2)
isCommonExp gf ((ELit l1, ELit l2):t) 
  | l1 == l2 = isCommonExp gf t
  | otherwise = Nothing
isCommonExp gf ((EAct e1 e2, EAct e3 e4):t) = 
    isCommonExp gf ([(e1, e3), (e2, e4)] ++ t)
isCommonExp gf ((EReq e1 e2, EReq e3 e4):t) = 
    isCommonExp gf ([(e1, e3), (e2, e4)] ++ t)
-- XXXpj Equality between templates?
isCommonExp gf ((ETempl n1 _ _ c1, ETempl n2 _ _ c2):t) 
  | n1 == n2 && isJust s1 && isJust s2  = Just (fromJust s1 @@ fromJust s2)
  | otherwise = Nothing
     where s1 = isCommonCmd gf (c1, c2)
           s2 = isCommonExp gf t
isCommonExp gf ((EDo n1 te1 c1, EDo n2 te2 c2):t) 
  | n2 == n2 && isJust s1 && isJust s2  = Just (fromJust s1 @@ fromJust s2)
  | otherwise = Nothing
     where s1 = isCommonCmd gf (c1, c2)
           s2 = isCommonExp gf t
isCommonExp gf ((e1, e2):t) = Nothing

isCommonCmd gf (CGen n1 t1 e1 c1, CGen n2 t2 e2 c2) 
  | n1 == n2 && isJust s1 && isJust s2 = Just (fromJust s1 @@ fromJust s2)
  | otherwise = Nothing
     where s1 = isCommonExp gf [(e1,e2)] 
           s2 = isCommonCmd gf (c1,c2)
isCommonCmd gf (CAss n1 e1 c1, CAss n2 e2 c2) 
  | n1 == n2 && isJust s1 && isJust s2 = Just (fromJust s1 @@ fromJust s2)
  | otherwise = Nothing
     where s1 = isCommonExp gf [(e1,e2)] 
           s2 = isCommonCmd gf (c1,c2)
isCommonCmd gf (CLet b1 c1, CLet b2 c2)
  | isJust s1 && isJust s2 = Just (fromJust s1 @@ fromJust s2)
  | otherwise = Nothing
     where s1 = isCommonExp gf [(ELet b1 (eBool True), ELet b2 (eBool True))]
           s2 = isCommonCmd gf (c1,c2)
isCommonCmd gf (CRet e1, CRet e2) = isCommonExp gf [(e1, e2)]
isCommonCmd gf (CExp e1, CExp e2) = isCommonExp gf [(e1, e2)]
isCommonCmd gf _ = Nothing

strict x (ECon _) = False
strict x (ESel e _) = strict x e
strict x (EVar n) = x == n
strict x (ELam _ _) = False
strict x (EAp (EVar (Prim Fatbar m)) [e1, e2]) = strict x e1
strict x (EAp (EVar (Prim LazyOr m)) [e1, e2]) = strict x e1
strict x (EAp (EVar (Prim LazyAnd m)) [e1, e2]) = strict x e1
-- XXXpj: lazyOr, possibly others?
strict x (EAp e args) = strict x e || any (strict x) args
strict x (ELet (Binds True _ eq) body) = strict x body
strict x (ELet (Binds False _ eq) body) = res
    where res = strict x body || any (strict x) (rng eq)
strict x (ECase cond legs) = strict x cond || strictLegs
   where strictLegs = all (strict x . altRhs) legs
-- XXXpj Is this right?
strict x (ERec _ eqns) = any (strict x) (rng eqns)
strict x (ELit _) = False
strict x (EAct e1 e2) = strict x e1 || strict x e2
strict x (EReq e1 e2) = strict x e1 || strict x e2
strict x (ETempl _ _ _ c) = strictCmd x c
strict x (EDo _ _ c) = strictCmd x c

strictCmd x (CGen _ _ e c) = strict x e || strictCmd x c
strictCmd x (CAss _ e c) = strict x e || strictCmd x c
strictCmd x (CLet (Binds True _ _) c) = strictCmd x c
strictCmd x (CLet (Binds False _ eq) c) = strictCmd x c || res
  where res = any (strict x) (rng eq)
strictCmd x (CRet e) = strict x e
strictCmd x (CExp e) = strict x e

terminates env (ECon _) = True
terminates env (ESel e _) = False -- terminates enve
terminates env (EVar n) 
 | Just body <- maybeInline env n = value body
 | n `elem` (splitVars env) = False
 | otherwise = True 
terminates env (ERec _ eqns) = all (terminates env) (rng eqns)
terminates env (ELam {}) = True
terminates env (EAp (EVar (Prim IntPlus a)) [e1, e2]) = 
  terminates env e1 && terminates env e2
terminates env (EAp (EVar (Prim IntMinus a)) [e1, e2]) =
  terminates env e1 && terminates env e2
terminates env (EAp (EVar (Prim IntTimes a)) [e1, e2]) =
  terminates env e1 && terminates env e2
terminates env (EAp (EVar (Prim IntDiv a)) [e1, e2]) = False
terminates env (EAp (EVar (Prim FloatDiv a)) [e1, e2]) = False
terminates env (ELit _) = True
terminates env _ = False

var (EVar _) = True
var _ = False

lit (ELit _) = True
lit _ = False



annoying env (EVar n@(Name _ _ _ _))
  | Just _ <- maybeInline env n = False
  | otherwise = True
annoying env (ESel v args) = annoying env v
-- XXXpj Does lit look like this?
annoying env (EAp (EVar (Prim op a)) [arg]) = annoying env arg
annoying env (EAp (EVar (Prim IntDiv a)) [ELit l, ELit l2]) | zero l2 = True
annoying env (EAp (EVar (Prim FloatDiv a)) [ELit l, ELit l2]) | zero l2 = True
annoying env (EAp (EVar (Prim op a)) [ELit l, arg2]) = annoying env arg2
annoying env (EAp (EVar (Prim op a)) [arg1, ELit l]) = annoying env arg1
annoying env (EAp (EVar (Prim op a)) [arg1, arg2]) =
  annoying env arg1 && annoying env arg2
annoying env (EAp (EVar (Prim op a)) [ELit l, ELit l2, arg3]) = 
  annoying env arg3
annoying env (EAp (EVar (Prim op a)) [ELit l, arg2, ELit l3]) = 
  annoying env arg2
annoying env (EAp (EVar (Prim op a)) [arg1, ELit l2, ELit l3]) = 
  annoying env arg1
annoying env (EAp (EVar (Prim op a)) [arg1, arg2, arg3]) =
  annoying env arg1 && annoying env arg2 && annoying env arg3
annoying env (EAp e args) = annoying env e
annoying _ _ = False

value (EVar _) = False
value (ECon _) = True
value (ELit _) = True
value (ESel e _) = value e
value (EAp (EVar (Prim IntDiv _)) [e1,e2]) = value e1 && nonzero e2
value (EAp (EVar (Prim FloatDiv _)) [e1,e2]) = value e1 && nonzero e2
value (EAp (EVar (Prim _ _)) es) = all value es
value (EAp (EVar (Tuple _ _)) es) = all value es
value (EAp (ECon c) es) = all value es
value (ELam _ _) = True
value (ERec _ eqs) = all (value . snd) eqs
value (EAct e1 e2) = True
value (EReq e1 e2) = True
value (ETempl _ _ _ _) = True
value (EDo n t c) = True
value e = False


isValue (EVar _) = False
isValue (ECon _) = True
isValue (ELit _) = True
isValue (ESel e _) = value e
isValue (EAp (EVar (Tuple _ _)) es) = all isValue es
isValue (EAp (ECon c) es) = all isValue es
isValue (ELam _ _) = True
isValue (ERec _ eqs) = all (isValue . snd) eqs
isValue (EAct e1 e2) = True
isValue (EReq e1 e2) = True
isValue (ETempl _ _ _ _) = True
isValue (EDo n t c) = True
isValue e = False

isNum (ELit _) = True
isNum _ = False

zero (LInt _ 0) = True
zero (LRat _ 0) = True
zero _ = False

nonzero (ELit l) = not (zero l)
nonzero _                         = False

substList [] [] e = e
substList (o:old) (n:new) e = substList old new (subst s e)
    where --s :: Map Name (Maybe Rho -> Exp)
          s = o +-> EVar n

substList' [] [] e = e
substList' (o:old) (n:new) e = substList' old new (subst s e)
    where s = o +-> n

renamings gf ls ml exp = rensM ++ rensL
    where rensM = [n | (Just e, t, n, body) <- ml, renaming gf e exp]
          rensL = [n | (n, e) <- ls, renaming gf e exp]

-- XXXpj: We need to determine statically if we can eval this, fv doesn't
-- account for functions in other modules.
closed _ _ = False
--closed env exp = null $ fv env exp

fv env exp = res -- trace ("fv: " ++ show res ++ " exp: " ++ show' exp ++ " invars : " ++ show invars) tmp
  where res = [x | x <- fv', elem x invars ] 
        fv' = nub $ evars exp
        invars = dom (inSet env)

eBool True = ECon (prim TRUE)
eBool False = ECon (prim FALSE)


findCon k (Alt PWild e:_)  = e
findCon k (Alt (PCon k' te) e:_)
  | k == k'                     = if null te then e else ELam te e
findCon k (_:alts)       = findCon k alts


findLit l (Alt PWild e:_)     = e
findLit l (Alt (PLit l') e:_)
  | l == l'                     = e
findLit l (_:alts)          = findLit l alts
findLit _ p = error (show p)

              
driveCaseStrLit env l c (Alt PWild e:_) = drive env e c
driveCaseStrLit env l c (Alt (PLit l') e:_) | l == l' = drive env e c
driveCaseStrLit env (LStr _ "") c (Alt (PCon (Prim NIL _) []) e:alts) = do
  drive env e c
driveCaseStrLit env (LStr _ str) c alts@(Alt (PCon (Prim CONS _) _) e:_) = do
  let nil = ECon (prim NIL)
      cons = ECon (prim CONS)
      chr x = ELit (LChr Nothing x)
      e' = foldr (\x y -> EAp cons [chr x, y]) nil str
  drive env (ECase e' alts) c
driveCaseStrLit env l c (_:alts) = driveCaseStrLit env l c alts


normLit (LInt p i)
  | i >= 0x80000000             = normLit (LInt p (i - 0x100000000))
  | i < -0x80000000             = normLit (LInt p (i + 0x100000000))
  | otherwise                   = LInt p i
normLit l                       = l

reducible IntNeg                 = True
reducible CharToInt              = True
reducible IntToFloat             = True
reducible IntToChar              = True
reducible FloatNeg               = True
reducible FloatTimes             = True
reducible FloatMinus             = True
reducible FloatPlus              = True
reducible FloatDiv               = True
reducible FloatLE                = True
reducible FloatToInt             = True
reducible IntPlus                = True
reducible IntMinus               = True
reducible IntTimes               = True
reducible IntDiv                 = True
reducible Sqrt                   = False
reducible op                     = {- tr' ("Not reducible: " ++ show op) -} False

redPrim ((EVar (Prim op _)):t)
 | not (reducible op) = Nothing
 | length t == 2
 , (ELit (LInt _ l1)) <- (head t)
 , (ELit (LInt _ l2)) <- (head (tail t))
  = Just (redInt2 op l1 l2)
 | length t == 2
 , (ELit (LRat _ l1)) <- (head t)
 , (ELit (LRat _ l2)) <- (head (tail t))
  = Just (redRat2 op l1 l2)
 | length t == 1
 , (ELit (LRat _ l)) <- (head t)
  = Just (redRat1 op l)
 | length t == 1
 , (ELit (LInt _ l)) <- (head t)
  = Just (redInt1 op l)
 | otherwise = error ("redPrim not done:" ++ show op ++ show t)

redChar1 CharToInt x             = ELit (lInt (ord x))
redChar1 p _                     = internalError0 ("Scp.redChar1: unknown primitive " ++ show p)

redInt1 IntNeg x                 = ELit (lInt (-x))
redInt1 IntToFloat x             = ELit (lRat (fromInteger x))
redInt1 IntToChar x              = ELit (lChr (chr (fromInteger x)))
redInt1 p _                      = internalError0 ("Scp.redInt1: unknown primitive " ++ show p)

redInt2 IntPlus a b              = ELit (normLit (lInt (a + b)))
redInt2 IntMinus a b             = ELit (normLit (lInt (a - b)))
redInt2 IntTimes a b             = ELit (normLit (lInt (a * b)))
redInt2 IntDiv a b               = ELit (lInt (a `div` b))
redInt2 IntMod a b               = ELit (lInt (a `mod` b))
redInt2 IntEQ a b                = eBool (a == b)
redInt2 IntNE a b                = eBool (a /= b)
redInt2 IntLT a b                = eBool (a < b)
redInt2 IntLE a b                = eBool (a <= b)
redInt2 IntGE a b                = eBool (a >= b)
redInt2 IntGT a b                = eBool (a > b)
redInt2 p _ _                    = internalError0 ("Scp.redInt2: unknown primitive " ++ show p)

redRat1 FloatNeg x               = ELit (lRat (-x))
redRat1 FloatToInt x             = ELit (lInt (truncate x))
redRat1 p _                      = internalError0 ("Scp.redRat1: unknonw primitive " ++ show p)

redRat2 FloatPlus a b            = ELit (lRat (a + b))
redRat2 FloatMinus a b           = ELit (lRat (a - b))
redRat2 FloatTimes a b           = ELit (lRat (a * b))
redRat2 FloatDiv a b             = ELit (lRat (a / b))
redRat2 FloatEQ a b              = eBool (a == b)
redRat2 FloatNE a b              = eBool (a /= b)
redRat2 FloatLT a b              = eBool (a < b)
redRat2 FloatLE a b              = eBool (a <= b)
redRat2 FloatGE a b              = eBool (a >= b)
redRat2 FloatGT a b              = eBool (a > b)
redRat2 p _ _                    = internalError0 ("Scp.redRat2 " ++ show p)

isCommit (EVar (Prim Commit _)) = True
isCommit _ = False

mkLams [] e = e
mkLams te e = ELam te e

mkApps e [] = e
mkApps e te = EAp e te

delete :: Eq a => [a] -> [a] -> [a]
delete _ [] = []
delete es (h:t) | h `elem` es = delete es t
                | otherwise = h:delete es t

linear n e = go e <= 1
    where go (ECon n) = 0
          go (ESel e _) = go e
          go (EVar v) | v == n = 1
                      | otherwise = 0
          go (ELam te e) | elem n (dom te) = 0
                         | otherwise = 2 * go e
          go (EAp e es) = go e + sum (map go es)
          go (ELet (Binds _ te bs) body) | elem n (dom te) = 0
                                         | otherwise = go body + sum (map go (rng bs))
          go (ECase e alts) = go e + maximum (go_alts alts)
          go (ERec _ eqns) = sum (map go (rng eqns))
          go (ELit {}) = 0
          go (EAct e1 e2) = go e1 + go e2
          go (EReq e1 e2) = go e1 + go e2
          go (ETempl _ _ te c) = goC c
          go (EDo _ _ c) = goC c

          go_alts [] = [0]
          go_alts ((Alt p e):t) | inPat n p = go_alts t
                                | otherwise = go e:go_alts t

          goC (CGen _ _ e c) = go e + goC c
          goC (CAss _ e c) = go e + goC c
          goC (CLet (Binds _ te bs) c) | elem n (dom te) = 0
                                       | otherwise = goC c + sum (map go (rng bs))
          goC (CRet e) = go e
          goC (CExp e) = go e
          goC (CCase e alts) = error "linear, goC, CCase: undefined"


inPat n (PCon _ te) = elem n (dom te)
inPat _ _ = False
