module Reduce where

import PP
import Common
import Core
import Env
import Kind
import Termred




type TSubst                             = Map TVar Type

type TEqs                               = [(Type,Type)]


noreduce env eqs pe                     = do s0 <- unify env eqs
                                             return (s0, pe, id)

fullreduce                              :: Env -> TEqs -> PEnv -> M (TSubst, PEnv, Exp->Exp)
fullreduce env eqs pe                   = do -- tr ("FULLREDUCE " ++ show pe)
                                             s0          <- unify env eqs
                                             let env0     = subst s0 env
                                             (s1,pe1,f1) <- normalize env0 (subst s0 pe)
                                             let env1     = subst s1 env0
                                             (s2,pe2,f2) <- resolve env1 pe1
                                             -- tr ("END FULLREDUCE " ++ show pe2)
                                             return (s2@@s1@@s0, pe2, f2 . f1)

normalize env pe                        = do -- tr ("NORMALIZE " ++ show pe)
                                             (s1, pe1, f1) <- reduce env pe
                                             (s2, pe2, f2) <- simplify (subst s1 env) pe1
                                             -- tr ("END NORMALIZE " ++ show pe2)
                                             return (s2@@s1, pe2, f2 . f1)


-- Conservative reduction ----------------------------------------------------------------------

reduce env pe                           = do -- tr ("###reduce " ++ show (tvars (typeEnv env)) ++ 
                                             --     "\n    tevars = " ++ show (tevars env) ++ "\n      " ++ show pe)
                                             (s,q,[],es) <- red [] (map mkGoal pe)
                                             -- tr ("###result: " ++ show s ++ ", preds: " ++ show q)
                                             return (s, q, \e -> eAp (eLam pe e) es)
  where mkGoal (v,p)                    = (tick env (isCoercion v), p)


-- Simplification ------------------------------------------------------------------------------

simplify env pe                         = do -- tr ("SIMPLIFY " ++ show pe)
                                             cs <- newNames skolemSym (length tvs)
                                             r <- expose (closeEnv env [] (subst (tvs`zip`map TId cs) pe) (cs`zip`ks))
                                             case r of
                                               Right (_,eqcs) -> return (nullSubst, pe2, \e -> foldr (f pe1) e eqs)
                                                 where eqs       = g eqcs
                                                       vs        = dom eqs
                                                       (pe1,pe2) = partition ((`elem`vs) . fst) pe
                                               Left s -> case decodeCircular s of
                                                 Nothing  -> fail s
--                                               Just cs' -> return (nullSubst, pe, id)
                                                 Just cs' -> do (t:ts) <- mapM sat tvs'
                                                                -- tr ("Circular: " ++ show pe)
                                                                s <- unify env (repeat t `zip` ts)
                                                                -- tr ("New: " ++ show (subst s pe))
                                                                (s',pe',f) <- normalize (subst s env) (subst s pe)
                                                                return (s'@@s, pe', f)
                                                   where tvs' = [ tv | (tv,c) <- tvs `zip` cs, c `elem` cs' ]
                                                         sat tv = do ts <- mapM newTVar (kArgs (tvKind tv))
                                                                     return (tAp (TVar tv) ts)
  where tvs                             = tvars pe
        ks                              = map tvKind tvs

        g []                            = []
        g ((e1,e2):eqcs)                = case (redTerm e1, redTerm e2) of
                                            (EVar v,e2) -> (v,e2) : g eqcs
                                            (e1,EVar v) -> (v,e1) : g eqcs
                                            _           -> g eqcs

        f pe (v,e) e0                   = EAp (eLam [(v,lookup' pe v)] e0) [e]


{-

B x < C Int \\ x
A x < B x \\ x, A x < C x \\ x

A x < B x \\ x, A x < C x \\ x
B x < C Int \\ x

Show [a] \\ a
Show [Char] \\

E a \\ a
E a \\ a, F a

b x < c Int \\ x, a x < b x \\ x, a x < c x \\ x

a x < b x \\ x, a x < c x \\ x, b x < c Int \\ x

-}

-- Forced reduction ------------------------------------------------------------------------

--resolve env pe                                = return (nullSubst, pe, id)

resolve env pe                          = do --tr ("###############\nBefore resolve: " ++ show pe)
                                             (s,q,[],es) <- red [] (map mkGoal pe)
                                             --tr ("###############\nAfter resolve: " ++ show q)
                                             let env1 = subst s env
                                                 q' = filter (isTemp . fst) q
                                             assert (null q') ("Cannot resolve predicates: " ++ vshow (rng q'))
                                             q'' <- mapM (closeCoercion env1) q
                                             return (s, q'', \e -> eAp (eLam pe e) es)
  where env_tvs                         = tevars env
        reachable_tvs                   = vclose (map tvars pe) (ps ++ ns ++ env_tvs)
          where (ps,ns)                 = pols env
        mkGoal (v,p)                    = (force env' (local  && (coercion || ambig)), p)
          where tvs                     = tvars p
                coercion                = isCoercion v
                local                   = null (tvs `intersect` env_tvs)
                ambig                   = null (tvs `intersect` reachable_tvs)
                env'                    = tick env coercion
        closeCoercion env (v,p)
          | isCoercion v                = do (_,p') <- gen env p
                                             return (v, p')
          | otherwise                   = return (v, p)
        


resolve' env pe                         = do (s,q,[],es) <- red [] (map mkGoal pe)
                                             let q' = filter (isTemp . fst) q
                                             assert (null q') ("Cannot resolve predicates: " ++ vshow (rng q'))
                                             return (s, q, \e -> eAp (eLam pe e) es)
  where mkGoal (v,p)                    = (force env True, p)



{-

    C1 x < C x
    C2 x < C Y
    C3 x < C y \\ x < y


    |- m aa < C b     |- Int < Int                              (All a . Exists b . m a < C b)
    ------------------------------                              (All a . m a < C b)
    |- C b -> Int  <  m aa -> Int
    ---------------------------------------                     (All a . (All b . m a < C b) =>
    |- (All b . C b -> Int)  <  m aa -> Int
    ----------------------------------------------
    |- All a . (All b . C b -> Int)  <  m a -> Int



    |- C1 aa < C aa     |- Int < Int                            C1 / m, aa / b
    --------------------------------
    |- C aa -> Int  <  C1 aa -> Int
    ----------------------------------------
    |- (All b . C b -> Int)  <  C1 aa -> Int
    -----------------------------------------------
    |- All a . (All b . C b -> Int)  <  C1 a -> Int



    |- C2 aa < C Y     |- Int < Int                             C2 / m, Y / b
    -------------------------------
    |- C Y -> Int  <  C2 aa -> Int
    ----------------------------------------
    |- (All b . C b -> Int)  <  C2 aa -> Int
    ------------------------------------------------
    |- All a . (All b . C b -> Int)  <  C2 ab -> Int


    All a . a < b |- aa < b                                     C3 / m
    -----------------------
    All a . a < b |- C3 aa < C b     |- Int < Int
    ---------------------------------------------
    All a . a < b |- C b -> Int  <  C3 aa -> Int
    -----------------------------------------------------
    All a . a < b |- (All b . C b -> Int)  <  m aa -> Int
    ------------------------------------------------------------
    All a . a < b |- All a . (All b . C b -> Int)  <  m a -> Int




-}

-- Scheme reduction -----------------------------------------------------------------------------
--
-- If   red cs ps == (s,q,es,es')    then   q |- es :: subst s cs   and   q |- es' :: subst s ps
--
-------------------------------------------------------------------------------------------------


red [] []                               = return (nullSubst, [], [], [])
red gs []                               = do (s,q,e:es) <- solve r g (gs1++gs2)
                                             let (es1,es2) = splitAt i es
                                             return (s, q, es1++[e]++es2, [])
  where rs                              = map (rank info) gs
        r                               = minimum rs
        i                               = length (takeWhile (/=r) rs)
        (gs1, g:gs2)                    = splitAt i gs
        info                            = varInfo gs
red gs ((env, p@(Scheme (F [sc1] t2) ps2 ke2)):ps)
                                        = do (t1,ps1) <- inst sc1
                                             pe <- newEnv assumptionSym ps2
                                             v  <- newName assumptionSym
                                             (env',_) <- closeEnv env (tvars sc1 ++ tvars t2 ++ tvars ps2) pe ke2
                                             let ps' = repeat (tick env' False) `zip` ps1
                                             (s,q,es,e,es') <- redf gs env' t1 t2 (ps'++ps)
                                             let (es1,es2) = splitAt (length ps') es'
                                                 g = eLam pe (ELam [(v,sc1)] (EAp e [eAp (EVar v) es1]))
--                                               ptvs = tvars (subst s (p : map snd ps))
-- Captured anyway...                        assert (null (pquant sc1) || null (tvars q \\ ptvs)) "Ambiguous coercion"
                                             return (s, q, es, g : es2)
red gs ((env, Scheme (R t) ps' ke):ps)  = do pe <- newEnv assumptionSym ps'
                                             (env',_) <- closeEnv env (tvars t ++ tvars ps') pe ke
                                             (s,q,e:es,es') <- red ((env',t) : gs) ps
                                             return (s, q, es, eLam pe e : es')



redf gs env (F ts t) (F ts' t') ps      = do te1' <- newEnv assumptionSym ts1'
                                             te2' <- newEnv assumptionSym ts2'
                                             te2  <- newEnv assumptionSym ts2
                                             v    <- newName assumptionSym
                                             (s,q,es,e,es1,es2) <- redf1 gs env t (tFun ts2' t') ts1' ts1 ps
                                             let e0  = ELam [(v,scheme' (F ts t))] (ELam (te1'++te2') e1)
                                                 e1  = eAp (EAp e [eLam te2 e2]) (map EVar (dom te2'))
                                                 e2  = EAp (EVar v) (es3 ++ map EVar (dom te2))
                                                 es3 = zipWith eAp1 es1 (map EVar (dom te1'))
                                             return (s, q, es, e0, es2)
  where (ts1 ,ts2 )                     = splitAt (length ts') ts
        (ts1',ts2')                     = splitAt (length ts ) ts'
redf gs env (R (TFun ts t)) b ps        = redf gs env (F (map scheme ts) (R t)) b ps
redf gs env a (R (TFun ts t)) ps        = redf gs env a (F (map scheme ts) (R t)) ps
redf gs env (R a) b@(F ts _) ps         = do (t:ts') <- mapM newTVar (replicate (length ts + 1) Star)
                                             s <- unify env [(a, TFun ts' t)]
                                             redf2 s gs env (F (map scheme ts') (R t)) b ps 
redf gs env a@(F ts _) (R b) ps         = do (t:ts') <- mapM newTVar (replicate (length ts + 1) Star)
                                             s <- unify env [(TFun ts' t, b)]
                                             redf2 s gs env a (F (map scheme ts') (R t)) ps 
redf gs env (R a) (R b) ps              = do (s,q,e:es,es') <- red ((tick env True, a `sub` b) : gs) ps
                                             return (s,q,es,e,es')



redf1 gs env a b (sc1:ts1) (sc2:ts2) ps = do (s,q,es,e,es1,e2:es2) <- redf1 gs env a b ts1 ts2 ((env,sc):ps)
                                             return (s, q, es, e, flip e2 : es1, es2)
  where Scheme t2 ps2 ke2               = sc2
        sc                              = Scheme (F [sc1] t2) ps2 ke2
        flip e  | null ps2              = e
        flip (ELam te2 (ELam te1 t))    = ELam te1 (ELam te2 t)
redf1 gs env a b [] [] ps               = do (s,q,es,e,es2) <- redf gs env a b ps
                                             return (s,q,es,e,[],es2)



redf2 s gs env a b ps                   = do (s',q,es,e,es') <- redf (subst s gs) (subst s env) 
                                                                   (subst s a) (subst s b) (subst s ps)
                                             return (s'@@s,q,es,e,es')



-- Predicate reduction ----------------------------------------------------------------------

solve RUnif (env,p) gs                  = do s <- unify env [(a,b)]
                                             (s',q,es,[]) <- red (subst s gs) []
                                             return (s'@@s, q, EVar (prim Refl) : es)
  where (a,b)                           = subs p
solve r g gs
  | mayLoop g                           = do assert (conservative g) "Recursive constraint"
                                             -- tr ("Avoiding loop: " ++ show g)
                                             (s,q,es,_) <- red gs []
                                             (q',e) <- newHyp g
                                             return (s, q'++q, e:es)
  | otherwise                           = do --tr ("Solving " ++ show r ++ " : " ++ show (snd g))
                                             try r (Left msg) (findWG r g) (logHistory g) gs
  where msg                             = "Cannot solve typing constraint " ++ show' (snd g)


try r accum wg g gs
  | isNullWG wg || isNull accum         = unexpose accum
  | otherwise                           = do --tr ("###Witness graph: " ++ show wg)
                                             res <- expose (hyp wit g gs)
                                             accum <- plus (g : gs) accum res
                                             try r accum (wg2 res) g gs
  where (l,wit,wg1)                     = takeWG wg
        wg2 res                         = if mayPrune res r g then pruneWG l wg1 else wg1


mayPrune (Left _)  _          _         = False
mayPrune (Right r) (RClass _) (env,c)   = forced env || subst (fst3 r) c == c
mayPrune _         _          _         = True


hyp wit (env,c) gs                      = do (R c',ps) <- inst (predOf wit)
                                             --tr ("hyp: " ++ show c ++ ", witness: " ++ show wit)
                                             s <- unify env [(c,c')]
                                             --tr "**OK"
                                             let ps' = repeat (subst s env) `zip` subst s ps
                                             (s',q,es,es') <- red (subst s gs) ps'
                                             return (s'@@s, q, eAp (expOf wit) es' : es)


plus gs (Left a) b                      = return b
plus gs a (Left b)                      = return a
plus gs (Right (s1,_,es1)) (Right (s2,_,es2))   
                                        = do s <- auSubst env s1 s2
                                             (q,es) <- auTerms (subst s gs) es1 es2
                                             return (Right (s, q, es))
  where env                             = fst (head gs)


-- Anti-unification --------------------------------------------------------------------------

auSubst env [] s2                       = return []
auSubst env ((v1,t1):s1) s2             = case lookup v1 s2 of
                                            Just t2 | h1 == h2 -> 
                                              do ts <- auTypes ts1 ts2
                                                 s <- auSubst env s1 s2
                                                 return ((v1, tAp h1 ts):s)
                                              where (h1,ts1) = tFlat t1
                                                    (h2,ts2) = tFlat t2
                                            _ -> auSubst env s1 s2
  where auType t1 t2
          | h1 == h2                    = do ts <- auTypes ts1 ts2
                                             return (tAp h1 ts)
          | otherwise                   = newTVar (kindOfType env t1)
          where (h1,ts1)                = tFlat t1
                (h2,ts2)                = tFlat t2
        auTypes ts1 ts2                 = sequence (zipWith auType ts1 ts2)



auTerms gs es1 es2                      = auZip auTerm gs es1 es2
  where
    auZip f [] [] []                    = return ([],[])
    auZip f (g:gs) (e1:es1) (e2:es2)    = do (q,e) <- f g e1 e2
                                             (q',es) <- auZip f gs es1 es2
                                             return (q++q',e:es)
    auZip f gs es1 es2                  = error ("Internal: auZip " ++ show gs ++ "\n" ++ show es1 ++ "\n" ++ show es2)
    auTerm g@(env,c) e1 e2              = auTerm' g (eFlat e1) (eFlat e2)


    auTerm' g@(env,c) (EVar v1, es1) (EVar v2, es2)
      | v1 == v2                        = do (R c',ps) <- inst (findPred env v1)
                                             let ps' = repeat env `zip` subst (matchTs [(c,c')]) ps
                                             (q,es) <- auZip auSc ps' es1 es2
                                             return (q, eAp (EVar v1) es)
    auTerm' g@(env,c) (ELam pe1 e1, es1) (ELam pe2 e2, es2)
      | ps1 == ps2                      = do (q,e) <- auTerm g e1 (subst s e2)
                                             (q',es) <- auZip auSc (repeat env `zip` ps1) es1 es2
                                             return (q, eAp (ELam pe1 e) es)
      where (vs1,ps1)                   = unzip pe1
            (vs2,ps2)                   = unzip pe2
            s                           = vs2 `zip` map EVar vs1
    auTerm' g e1 e2                     = newHyp g

    auSc (env,Scheme (R c) [] ke) e1 e2 = auTerm (addKEnv ke env,c) e1 e2
    auSc (env,Scheme (R c) ps ke) (ELam pe1 e1) (ELam pe2 e2)     
                                        = do (q,e) <- auTerm (env',c) e1 (subst s e2)
                                             return (q, ELam pe1 e)
      where s                           = dom pe2 `zip` map EVar (dom pe1)
            env'                        = addPEnv pe1 (addKEnv ke env)
    auSc _ _ _                          = error "Internal: auTerms"


newHyp (env,c)                          = do v <- newName (sym env)
                                             return ([(v,p)], eAp (wit v) (map EVar vs))
  where p                               = Scheme (R c) ps ke
        tvs                             = tyvars c
        tvs'                            = tvars c
        useful p                        = all (`elem` tvs) (tyvars p) && all (`elem` tvs') (tvars p)
        (vs,ps)                         = unzip (filter useful (predEnv env))
        ke                              = filter ((`elem` tvs) . fst) (kindEnv env)
        wit v                           = if isCoercion v then ESig (EVar v) (scheme c) else EVar v
        sym env
          | forced env                  = tempSym
          | ticked env                  = coercionSym 
          | otherwise                   = assumptionSym


-- Unification ----------------------------------------------------------

{-
unify env []                            = return nullSubst
unify env ((t,TVar n []):eqs)           = tvarBind env n t eqs
unify env ((TVar n [],t):eqs)           = tvarBind env n t eqs
unify env ((TFun ts1 t1,TFun ts2 t2):eqs)
  | length ts1 == length ts2            = unify env ((t1,t2) : ts1 `zip` ts2 ++ eqs)
unify env ((TId c1 ts1,TId c2 ts2):eqs)
  | c1 == c2                            = unify env (ts1 `zip` ts2 ++ eqs)
unify env ((TVar n ts1,TId c ts2):eqs)
  | length ts2 >= length ts1            = tvarBind env n (TId c post2) (ts1 `zip` pre2 ++ eqs)
  where (pre2,post2)                    = splitAt (length ts1) ts2
unify env ((TId c ts1,TVar n ts2):eqs)
  | length ts1 >= length ts2            = tvarBind env n (TId c post1) (pre1 `zip` ts2 ++ eqs)
  where (pre1,post1)                    = splitAt (length ts2) ts1
unify env ((TVar n1 ts1,TVar n2 ts2):eqs)
  | length ts2 >= length ts1            = tvarBind env n1 (TVar n2 post2) (ts1 `zip` pre2 ++ eqs)
  | otherwise                           = tvarBind env n2 (TVar n1 post2) (pre1 `zip` ts2 ++ eqs)
  where (pre1,post1)                    = splitAt (length ts2) ts1
        (pre2,post2)                    = splitAt (length ts1) ts2
unify env (eq:eqs)                      = fail ("Unification error: " ++ show eq)
-}


unify env []                            = return nullSubst
unify env ((TVar n,t):eqs)
  | mayBind env n                       = tvarBind env n t eqs
unify env ((t,TVar n):eqs)
  | mayBind env n                       = tvarBind env n t eqs
unify env ((TAp t u,TAp t' u'):eqs)     = unify env ((t,t'):(u,u'):eqs)
unify env ((TId c,TId c'):eqs)
  | c == c'                             = unify env eqs
unify env ((TFun ts t, TFun ts' t'):eqs)
  | length ts == length ts'             = unify env ((t,t') : ts `zip` ts' ++ eqs)
unify env eqs                           = fail ("Unification error: " ++ show (head eqs))


tvarBind env n t eqs
  | t == TVar n                         = unify env eqs
  | tvKind n /= kindOfType env t        = fail "Kind mismatch in unify"
  | n `elem` tvars t                    = fail "Occurs check failed in unify"
  | n `elem` skolEnvs env (tyvars t)    = fail "Skolem escape in unify"
  | otherwise                           = do s' <- unify env (subst s eqs)
                                             return (s' @@ s)
  where s                               = n +-> t

mayBind env n                           = not (frozen env && n `elem` pevars env)


-- Misc ----------------------------------------------------------------

mayLoop (env,c)                         = any (\c' -> equalTs [(c,c')]) (history env)


isNull (Right ([],q,es))
  | all varTerm es                      = True
  where varTerm (EAp e es)              = varTerm e
        varTerm (ELam pe e)             = varTerm e
        varTerm (EVar v)                = v `elem` vs
        varTerm _                       = False
        vs                              = dom q
isNull _                                = False



-- Environment closure reduction -------------------------------------------------

closeEnv env tvs pe ke                  = do env1 <- initRefl ke env0
                                             closeWits env1 [] wits
  where se                              = mapSnd (const (tvs ++ pevars env)) ke
        wits                            = mapFst EVar pe
        env0                            = freeze (addSkolEnv se (addPEnv pe (addKEnv ke env)))


initRefl ke env                         = do refls <- mapM mkRefl ke
                                             return (initSubs (dom ke) refls env)
  where mkRefl (i,k)                    = do vs <- newNames paramSym (length ks)
                                             let t = tAp (TId i) (map TId vs)
                                             return (EVar (prim Refl), Scheme (R (t `sub` t)) [] (vs `zip` ks))
          where ks                      = kArgs k



-- Note: For top-level instances, the eqcs are equality constraints that must be met by the instance terms.
-- For local instance assumptions, they are facts that can be utilized for simplification.

closeWits env eqs []                    = return (thaw env, eqs)
closeWits env eqs (wit : wits)
  | isSub' p                            = do wits' <- mapSuccess (mkTrans (noClasses env) wit) wpairs
                                             let cycles = filter (uncurry (==)) (map (subsyms . predOf) wits')
                                             assert (null cycles) (encodeCircular (nub (a:b:map fst cycles)))
                                             (env1,eqs1) <- subGraphs env eqs wits'
                                             closeWits env1 eqs1 wits
  | otherwise                           = do wits' <- mapSuccess (mkSup (noClasses env) wit) wsups
                                             (env1,eqs1) <- classGraph env eqs wits'
                                             closeWits env1 eqs1 wits
  where c                               = headsym p
        (a,b)                           = subsyms p
        p                               = predOf wit
        wbelow_a                        = rng (nodes (findBelow env a))
        wabove_b                        = rng (nodes (findAbove env b))
        wpairs                          = [ (w0,w2) | w0 <- wbelow_a, w2 <- wabove_b ]
        wabove_c                        = rng (nodes (findAbove env c))
        wsups                           = [ w | w <- wabove_c, uppersym (predOf w) `elem` dom (classEnv env) ]



imply env wit' wit                      = do res <- expose (red [] [(env0, p)])
                                             case res of
                                               Right (_,_,_,es) -> return (Just (expOf wit, head es))
                                               Left _           -> return Nothing
  where env0                            = singleWitness env wit'
        p                               = predOf wit


mapSuccess f xs                         = do xs' <- mapM (expose . f) xs
                                             return [ x | Right x <- xs' ]



mkTrans env (e1,p1) ((e0,p0),(e2,p2))   = do (pe0, R c0, e0) <- instantiate p0 e0
                                             (pe1, R c1, e1) <- instantiate p1 e1
                                             (pe2, R c2, e2) <- instantiate p2 e2
                                             let (t0,t0') = subs c0
                                                 (t1,t1') = subs c1
                                                 (t2,t2') = subs c2
                                             s <- unify env [(t0',t1),(t1',t2)]
                                             let t        = subst s t0
                                                 p        = scheme (t `sub` subst s t2')
                                             (s',qe,f) <- normalize (protect p env) (subst s (pe0++pe1++pe2))
                                             x  <- newName paramSym
                                             let e3       = EAp e2 [EAp e1 [EAp e0 [EVar x]]]
                                                 e        = ELam [(x,scheme (subst s' t))] (f e3)
                                                 (e',p')  = qual qe e (subst s' p)
                                             (s'',sc) <- gen env p'
                                             return (redTerm (subst s'' e'), sc)



subGraphs env eqs []                    = return (env,eqs)
subGraphs env eqs (wit:wits)            = case findCoercion env a b of
                                            Just (l,wit') -> do 
                                               r1 <- imply env wit' wit
                                               r2 <- imply env wit wit'
                                               case (r1,r2) of
                                                 (Just eq, _) -> subGraphs env (eq:eqs) wits
                                                 (_, Just eq) -> subGraphs (replaceWit l wit' env) (eq:eqs) wits
                                                 _            -> fail "Ambiguous subtyping"
                                            Nothing -> do 
                                               l <- newNum
                                               subGraphs (buildBelow l wit (buildAbove l wit env)) eqs wits
  where (a,b)                           = subsyms (predOf wit)
        replaceWit l wit' env           = env { aboveEnv = update a f (aboveEnv env),
                                                belowEnv = update b f (belowEnv env) }
          where f wg                    = wg { nodes = insert l wit' (nodes wg) }



mkSup env (e1,p1) (e2,p2)               = do (pe1, R c1, e1) <- instantiate p1 e1
                                             (pe2, R c2, e2) <- instantiate p2 e2
                                             let (t2,t2') = subs c2
                                             s <- unify env [(c1,t2)]
                                             let p = scheme (subst s t2')
                                             (s',qe,f) <- normalize (protect p env) (subst s (pe1++pe2))
                                             w <- newName witnessSym
                                             let e = f (EAp e2 [e1])
                                                 (e',p') = qual qe e (subst s' p)
                                             (s'',sc) <- gen env p'
                                             return (subst s'' e', sc)


classGraph env eqs []                   = return (env,eqs)
classGraph env eqs (wit:wits)           = do (eqs',pre,post) <- cmpW env [] [] [] wit (nodes wg)
                                             l <- newNum
                                             let ls = repeat l
                                                 wg' = WG { nodes = insertBefore (l,wit) post (nodes wg),
                                                            arcs  = pre `zip` ls ++ ls `zip` post ++ arcs wg }
                                                 env' = insertClass c wg' env
                                             classGraph env' (eqs' ++ eqs) wits
  where c                               = headsym (predOf wit)
        wg                              = findClass env c


cmpW env eqs pre post wit []            = return (eqs,pre,post)
cmpW env eqs pre post wit ((l,wit'):ns) = do r1 <- imply env wit wit'
                                             r2 <- imply env wit' wit
                                             case (r1,r2) of
                                               (Just _ , Just eq) -> cmpW env (eq:eqs) pre post wit ns
                                               (Just _ , Nothing) -> cmpW env eqs (l:pre) post wit ns
                                               (Nothing, Just _ ) -> cmpW env eqs pre (l:post) wit ns
                                               (Nothing, Nothing) -> cmpW env eqs pre post wit ns
        


{-

    Ord a |- Ord a

    Ord a \\ a |- Ord Int

    C a \\ a |- D a => C a \\ a

    D a => C a \\ a |- D a => E a => C a \\ a

    a < b |- a < b

    m a < m a \\ a |- m Int < m Int

    m a < n a \\ a, C a |- m a < n a \\ a


    x : Eq a
    y : Eq a
    ==>
    eqid x : Eq a
    eqid y : Eq a
    ==>
    eqid x : Eq a |- (eqid x)/y : Eq a

-}



{-
          A<A:0  : A :   0:A<A          []
          B<B:0  : B :   0:B<B       []

   buildAbove 7:A<B,   (a,b)=(A,B), syms = {A}, c = A, adding A<B after A<A to aboveEnv A (0,7)

          0:A<A  : A :   0:A<A,7:A<B


   buildBelow 7:A<B,   (a,b)=(A,B), syms = {B}, c = B, adding A<B after B<B to belowEnv B (0,7)

    7:A<B,0:B<B  : B :   0:B<B

   

-}

{-

    Ord a => Ord [a] \\ a

    (b < a) => Ord a < Ord b \\ a,b
    (b < a) => Ord a < Eq b \\ a,b

    (a < b) => [a] < [b] \\ a,b

    ...

    Ord a, [b] < [a] => Eq [b]

    Ord a, b < a => Eq [b]

    Ord a => Eq [a]


    Eq a => Eq [a]


    Ord aa, Eq aa, Eq a => Eq [a] \\ a  |-  Eq aa
    ---------------------------------------------
    Ord aa, Eq a => Eq [a] \\ a  |-  Eq aa
    ----------------------------------------
    Ord aa, Eq a => Eq [a] \\ a  |-  Eq [aa]
    ---------------------------------------------
    Eq a => Eq [a] \\ a  |-  Ord a => Eq [a] \\ a

-}
        

{-

   1: A < A                            x < A   [ A < A, B < A, C < A ]     A < x   [ A < A ]

   2: B < A                            x < B   [ B < B, C < B ]            B < x   [ B < B, B < A ]

3: B < B  4: C < A                     x < C   [ C < C ]                   C < x   [ C < C, C < B, C < A ]

   5: C < B

   6: C < C

(1,2) (1,4) (2,3) (2,4) (3,5) (4,5) (4,6) (5,6)



Show [a] \\ Show a

Show [C]

w : Eq A

u : Eq B

Eq C

x -> T \\ x < A, Show [x]

x -> T \\ x < A, Eq x

x -> T \\ x < y, y < a



f : A->T
eq : Eq a => a->a->Bool


 |- w : Eq A         |- id : A->A->Bool < A->A->Bool
-----------------------------------------------------
 |- \v.id (v w) : Eq A => A->A->Bool < A->A->Bool
--------------------------------------------------
 |- \v.v w : Eq a => a->a->Bool < A->A->Bool
x:A |- eq : Eq a => a->a->Bool
--------------------------------
x:A |- (\v.v w) eq : A->A->Bool              x:A |- x : A
--------------------------------             ---------------
x:A |- eq w x x : Bool                       x:A |- f x : T
-----------------------------------------------------------
x:A |- if eq w x x then f x else f x : T
-----------------------------------------------------------
    |- \x -> if eq w x x then f x else f x : A -> T


 |- u : Eq B         |- id : B->B->Bool < B->B->Bool
----------------------------------------------------
 |- \v.id (v u) : Eq B => B->B->Bool < B->B->Bool
--------------------------------------------------
 |- \v.v u : Eq a => a->a->Bool < B->B->Bool
x:B |- eq : Eq a => a->a->Bool               x.B |- x : B    |- b : B < A
------------------------------------         ----------------------------
x:B |- (\v.v u) eq : B->B->Bool              x:B |- b x : A
-------------------------------              ---------------
x:B |- eq u x x : Bool                       x:B |- f (b x) : T
---------------------------------------------------------------
x:B |- if eq u x x then f (b x) else f (b x) : T
---------------------------------------------------------------
    |- \x -> if eq u x x then f (b x) else f (b x) : B -> T


P = k:x<A, j:Eq x


P |- j : Eq x       P |- id : x->x->Bool < x->x->Bool
----------------------------------------------------
P |- \v.id (v j) : Eq B => x->x->Bool < x->x->Bool
--------------------------------------------------
P |- \v.v j : Eq a => a->a->Bool < x->x->Bool
P | x:x |- eq : Eq a => a->a->Bool              P | x.x |- x : x   P |- k : x < A
------------------------------------           -----------------------------------
P | x:x |- (\v.v j) eq : x->x->Bool             P | x:x |- k x : A
-----------------------------------            ----------------------
P | x:x |- eq j x x : Bool                      P | x:x |- f (k x) : T
----------------------------------------------------------------------
P | x:x |- if eq j x x then f (k x) else f (k x) : T
---------------------------------------------------------------
P |  |- \x -> if eq j x x then f (k x) else f (k x) : x -> T
------------------------------------------------------------------------------------
  |  |- \k j x -> if eq j x x then f (k x) else f (k x) : (x < A, Eq x) => x -> T



x < A |- x < A        x < A |- T < T
------------------------------------
x < A |- A -> T < x -> T
--------------------------------
 |- x < A => (A -> T < x -> T)
--------------------------------
 |- A -> T < (x < A => x < T)


x < A, Eq x |- x < A        x < A, Eq x |- T < T
------------------------------------------------
x < A, Eq x |- A -> T < x -> T
-------------------------------------
 |- (x < T, Eq x) => (A -> T < x -> T)
--------------------------------------
 |- A -> T < ((x < A, Eq x) => x < T)


w : Eq A
u : Eq B
c : B < A
eq : Eq a => a -> a -> Bool

eq w (f b1) (f b2)  ==  eq u b1 b2 ???


h . g . f      ==        


i2f : Int < Float

neq : Num a => a -> a

ni : Num Int
nf : Num Float

i2f (neg ni x)     ==       neg nf (i2f x)     ???


bind : Monad m => m a -> (a->m b) -> m b
f : Request Int
g : Int -> Action


Monad m, Request Int < m Int, Action < m b


Monad Cmd, Request Int < Cmd Int, Action < Cmd ()

Monad (O s), Request Int < O s Int, Action < O s ()


 |- c : Monad m => m a -> (a -> m b) -> m b < Cmd Int -> (Int -> Cmd ()) -> Cmd ()
 |- bind : Monad m => m a -> (a -> m b) -> m b     
---------------------------------------------------------------------------------------------------------
 |- c bind : Cmd Int -> (Int -> Cmd ()) -> Cmd ()
----------------------------------------------
 |-> c bind f g : Cmd ()

-}

