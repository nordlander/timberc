module Reduce where

import PP
import Common
import Core
import Env
import Kind
import Depend
import Termred




type TSubst                             = Map TVar Type

type TEqs                               = [(Type,Type)]


noreduce env eqs pe                     = do s0 <- unify env eqs
                                             return (s0, pe, id)

fullreduce                              :: Env -> TEqs -> PEnv -> M s (TSubst, PEnv, Exp->Exp)
fullreduce env eqs pe                   = do -- tr ("FULLREDUCE\n" ++ render (vpr pe))
                                             (s1,pe1,f1) <- normalize env eqs pe
                                             -- tr ("Subst: " ++ show s1)
                                             let env1     = subst s1 env
                                             (s2,pe2,f2) <- resolve env1 pe1
                                             -- tr ("END FULLREDUCE " ++ show pe2)
                                             return (s2@@s1, pe2, f2 . f1)

normalize env eqs pe                    = do s0 <- unify env eqs
                                             let env0 = subst s0 env
                                             (s1,pe1,f1) <- norm env0 (subst s0 pe)
                                             return (s1@@s0, pe1, f1)
                                             

norm env pe                             = do -- tr ("NORMALIZE\n" ++ render (vpr pe))
                                             (s1, pe1, f1) <- reduce env pe
                                             (s2, pe2, f2) <- simplify (subst s1 env) pe1
                                             -- tr ("END NORMALIZE " ++ show pe2)
                                             return (s2@@s1, pe2, f2 . f1)


-- Conservative reduction ----------------------------------------------------------------------

reduce env pe                           = do -- tr ("###reduce\n" ++ render (vpr pe) ) -- ++ "\n\n" ++ show (tvars (typeEnv env)))
                                             (s,q,[],es) <- red [] (map mkGoal pe)
                                             -- tr ("###result: " ++ show (dom pe `zip` es))
                                             return (s, q, eLet pe (dom pe `zip` es))
  where mkGoal (v,p)                    = (tick env{errPos = pos v} (isCoercion v || isDummy v), p)

-- Simplification ------------------------------------------------------------------------------

simplify env pe                         = do -- tr ("SIMPLIFY " ++ show pe)
                                             cs <- newNames skolemSym (length tvs)
                                             r <- expose (closePreds env [] (subst (tvs`zip`map TId cs) pe) (cs`zip`ks))
                                             case r of
                                               Right (env',qe,eq) -> return (nullSubst, pe', eLet' (subst s bss))
                                                 where (pe',bss) = preferLocals env' pe qe eq
                                                       s = cs `zip` map TVar tvs
                                               Left s -> case decodeCircular s of
                                                 Nothing  -> fail s
--                                               Just cs' -> return (nullSubst, pe, id)
                                                 Just cs' -> do (t:ts) <- mapM sat tvs'
                                                                -- tr ("Circular: " ++ show pe)
                                                                s <- unify env (repeat t `zip` ts)
                                                                -- tr ("New: " ++ show (subst s pe))
                                                                (s',pe',f) <- norm (subst s env) (subst s pe)
                                                                return (s'@@s, pe', f)
                                                   where tvs' = [ tv | (tv,c) <- tvs `zip` cs, c `elem` cs' ]
                                                         sat tv = do ts <- mapM newTVar (kArgs (tvKind tv))
                                                                     return (tAp (TVar tv) ts)
  where tvs                             = tvars pe
        ks                              = map tvKind tvs


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

resolve env pe                          = do -- tr "RESOLVING"
                                             -- tr ("############### Before resolve: " ++ show pe)
                                             -- tr ("tevars: " ++ show env_tvs ++ ",   reachable: " ++ show reachable_tvs)
                                             (s,q,[],es) <- red [] (map mkGoal pe)
                                             -- tr ("############### After resolve: " ++ show q)
                                             let q' = filter badDummy q
                                             assert (null q') "Cannot resolve predicates with heads" (map (headsym . snd) q')
                                             -- tr "DONE RESOLVING"
                                             return (s, q, eLet pe (dom pe `zip` es))
  where env_tvs                         = tevars env
        reachable_tvs                   = vclose (map tvars pe) (ps ++ ns ++ env_tvs)
          where (ps,ns)                 = pols env
        mkGoal (v,p)                    = (force env' (coercion || ambig), p)
          where tvs                     = tvars p
                coercion                = isCoercion v
                ambig                   = null (tvs `intersect` reachable_tvs)
                env'                    = tick env coercion
        badDummy (v,p)                  = isDummy v && null (tvars p `intersect` env_tvs)


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
red gs []                               = do -- tr ("%%%%%%%% Simple goals:")
                                             -- tr (render (vpr (map snd gs)))
                                             let str = if forced (fst g) then "%%%%%%%% Force " else "%%%%%%%% Solve "
                                             -- tr (str ++ show r ++ " : " ++ render (pr (snd g)) ++ ",  info: " ++ show info)
                                             (s,q,e:es) <- solve r g (gs1++gs2)
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
                                             v  <- newName coercionSym
                                             (env',qe,eq) <- closePreds env (tvars sc1 ++ tvars t2 ++ tvars ps2) pe ke2
                                             let ps' = repeat (tick env' False) `zip` ps1
                                             (s,q,es,e,es') <- redf gs env' t1 t2 (ps'++ps)
                                             let (es1,es2) = splitAt (length ps') es'
                                                 bss = preferParams env' pe qe eq
                                                 e' = eLet' bss (EAp e [eAp (EVar v) es1])
                                             return (s, q, es, eLam pe (ELam [(v,sc1)] e') : es2)
red gs ((env, Scheme (R t) ps' ke):ps)  = do pe <- newEnv assumptionSym ps'
                                             (env',qe,eq) <- closePreds env (tvars t ++ tvars ps') pe ke
                                             (s,q,e:es,es') <- red ((env',t) : gs) ps
                                             let bss = preferParams env' pe qe eq
                                             return (s, q, es, eLam pe (eLet' bss e) : es')



redf gs env (F ts t) (F ts' t') ps      = do te1' <- newEnv assumptionSym ts1'
                                             te2' <- newEnv assumptionSym ts2'
                                             te2  <- newEnv assumptionSym ts2
                                             v    <- newName coercionSym
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

solve RFun (env,p) gs                   = do (s,q,es,[e]) <- red gs [(env, scheme' (F [scheme a] (R b)))]
                                             return (s, q, e:es)
  where (a,b)                           = subs p
solve RUnif (env,p) gs                  = do -- tr ("unif: " ++ show p)
                                             s <- unify env [(a,b)]
                                             (s',q,es,[]) <- red (subst s gs) []
                                             return (s'@@s, q, EVar (prim Refl) : es)
  where (a,b)                           = subs p
solve r g gs
  | mayLoop g                           = do assert0 (conservative g) "Recursive constraint"
                                             -- tr ("Avoiding loop: " ++ show g)
                                             (s,q,es,_) <- red gs []
                                             (q',e) <- newHyp g
                                             return (s, q'++q, e:es)
  | otherwise                           = do -- tr ("Solving " ++ show r ++ " : " ++ render (pr (snd g)))
                                             try r (Left msg) (findWG r g) (logHistory g) gs
  where msg                             = "Cannot solve typing constraint "++render(prPred (snd g))

try r accum wg g gs
  | isNullWG wg || isNull accum         = unexpose accum
  | otherwise                           = do -- tr ("###Witness graph: " ++ show wg)
                                             res <- expose (hyp wit g gs)
                                             accum <- plus (g : gs) accum res
                                             -- tr ("New accum: " ++ show accum)
                                             try r accum (wg2 res) g gs
  where (wit,wg1)                       = takeWG wg
        wg2 res                         = if mayPrune res r g then pruneWG (nameOf wit) wg1 else wg1


mayPrune (Left _)  _          _         = False
mayPrune (Right r) (RClass _) (env,c)   = forced env || subst (fst3 r) c == c
mayPrune _         _          _         = True


hyp (w,p) (env,c) gs                    = do (R c',ps) <- inst p
                                             -- tr ("hyp: " ++ show c ) -- ++ ", witness: " ++ show (w,p))
                                             s <- unify env [(c,c')]
                                             -- tr ("**OK: " ++ show s)
                                             let ps' = repeat (subst s env) `zip` subst s ps
                                             (s',q,es,es') <- red (subst s gs) ps'
                                             return (s'@@s, q, eAp (EVar w) es' : es)


plus gs (Left a) (Left b)
  |take 5 (drop 7 a)=="solve" &&
   take 5 (drop 7 b)=="unify"           = return (Left a)
  |otherwise                            = return (Left b)
plus gs (Left a) b                      = return b
plus gs a (Left b)                      = return a
plus gs (Right (s1,_,es1)) (Right (s2,_,es2))   
                                        = do s <- auSubst env s1 s2
                                             (s',q,es) <- auTerms (subst s gs) es1 es2
                                             return (Right (s'@@s, q, es))
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
    auZip f [] [] []                    = return ([],[],[])
    auZip f (g:gs) (e1:es1) (e2:es2)    = do (s,q,e) <- f g e1 e2
                                             (s',q',es) <- auZip f (subst s gs) es1 es2
                                             return (s'@@s, subst s' q ++ q', e:es)
    auZip f gs es1 es2                  = internalError0 ("auZip " ++ show gs ++"\n" ++ show es1 ++ "\n" ++ show es2) 
    auTerm g@(env,c) e1 e2              = auTerm' g (eFlat e1) (eFlat e2)


    auTerm' g@(env,c) (EVar v1, es1) (EVar v2, es2)
      | v1 == v2                        = do (R c',ps) <- inst (findPred env v1)
                                             let s = matchTs [(c,c')]
                                             (s',q,es) <- auZip auSc (repeat (subst s env) `zip` subst s ps) es1 es2
                                             return (s'@@s, q, eAp (EVar v1) es)
    auTerm' g@(env,c) (ELam pe1 e1, es1) (ELam pe2 e2, es2)
      | ps1 == ps2                      = do (s,q,e) <- auTerm g e1 (subst s0 e2)
                                             (s',q',es) <- auZip auSc (repeat (subst s env) `zip` subst s ps1) es1 es2
                                             return (s'@@s, subst s' q ++ q', eAp (ELam pe1 e) es)
      where (vs1,ps1)                   = unzip pe1
            (vs2,ps2)                   = unzip pe2
            s0                          = vs2 `zip` map EVar vs1
    auTerm' g e1 e2                     = do (q,e) <- newHyp g
                                             return ([], q, e)

    auSc (env,Scheme (R c) [] ke) e1 e2 = auTerm (addKEnv ke env,c) e1 e2
    auSc (env,Scheme (R c) ps ke) (ELam pe1 e1) (ELam pe2 e2)     
                                        = do (s,q,e) <- auTerm (env',c) e1 (subst s0 e2)
                                             return (s, q, ELam pe1 e)
      where s0                          = dom pe2 `zip` map EVar (dom pe1)
            env'                        = addPEnv pe1 (addKEnv ke env)
    auSc _ _ _                          = internalError0 "auTerms"


newHyp (env,c)                          = do -- tr ("newHyp " ++ render (prPScheme 0 p))
                                             v <- newName (sym env)
                                             return ([(v,p)], eAp (EVar (annotExplicit v)) (map EVar vs))
  where p                               = Scheme (R c) ps ke
        (vs,ps)                         = unzip (predEnv env)
        ke                              = kindEnv env
        sym env
          | forced env                  = dummySym          -- unwanted garbage predicate, trap in resolve
          | ticked env                  = coercionSym       -- originates from a coercion predicate
          | otherwise                   = assumptionSym     -- ordinary predicate


-- Unification ----------------------------------------------------------

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
unify env ((t1,t2):_)                   = fail ("Cannot unify " ++ render(pr t1) ++ " with " ++ render(pr t2))

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



-- Adding predicates to the environment -------------------------------------------------

closePreds0 env pe                      = do (env1,pe1,eq1) <- closeTransitive env0 pe
                                             (env2,pe2,eq2) <- closeSuperclass env1 pe
                                             return (env2, pe1++pe2, eq1++eq2)
  where env0                            = addPEnv0 pe env

closePreds env tvs pe ke                = do (env1,pe1,eq1) <- closeTransitive env0 pe
                                             (env2,pe2,eq2) <- closeSuperclass env1 pe
                                             return (thaw env2, pe1++pe2, eq1++eq2)
  where se                              = mapSnd (const (tvs ++ pevars env)) ke
        env0                            = freeze (addPEnv pe (addSkolEnv se (addKEnv ke env)))


preferLocals env pe qe eq               = walk [] (equalities env)
  where walk bs []                      = let (pe1,pe2) = partition ((`elem` dom bs) . fst) pe
                                          in  (pe2, groupBinds (Binds False (pe1++qe) (prune eq (dom bs) ++ mapSnd EVar bs)))
        walk bs ((x,y):eqs)
          | x `notElem` vs1             = walk bs eqs
          | otherwise                   = case (x `elem` vs0, y `elem` vs0) of
                                            (True,  True)  -> walk ((x,y):bs) eqs
                                            (True,  False) -> walk ((x,y):bs) eqs
                                            (False, True)  -> walk ((y,x):bs) eqs
                                            (False, False) -> walk ((y,x):bs) eqs
        vs0                             = dom pe
        vs1                             = vs0 ++ dom qe

preferParams env pe qe eq               = walk [] [] (equalities env)
  where walk ws bs []                   = groupBinds (Binds False (prune qe ws) (prune eq (ws ++ dom bs) ++ mapSnd EVar bs))
        walk ws bs ((x,y):eqs)
          | x `notElem` vs1             = walk ws bs eqs
          | otherwise                   = case (x `elem` vs0, y `elem` vs0) of
                                            (True,  True)  -> walk ws bs eqs
                                            (True,  False) -> walk ws ((y,x):bs) eqs
                                            (False, True)  -> walk (x:ws) bs eqs
                                            (False, False) -> walk (x:ws) bs eqs
        vs0                             = dom pe
        vs1                             = vs0 ++ dom qe

{-
                                                        Top-level & local reduction:        Action during simplify:
In (equalities env):  Meaning:                          (prefer parameters/constants)       (prefer local defs)

    (v,v')      Two witness parameters equal            Ignore equality info                Remove parameter v
                                                        [only v' is in use]                 Add def "let v = v' in ..."

    (v,w')      Witness parameter is equal to a         Remove local def of w' [in use]     Remove parameter v
                local def                               Add "let w' = v in ..."             Add def "let v = w' in ..."

    (w,v')      Witness parameter is equal to a         Remove local def of w               Remove parameter v'
                local def                               [only parameter v' is in use]       Add def "let v' = w in ..."

    (w,w')      Two local witness definitions           Remove local def of w               Remove local def of w'
                are equal                               [only w' is in use]                 Add def "let w' = w in ..."

    v and v' are witness parameters (elements of (dom pe))
    w and w' are locally generated witnesses
-}


mapSuccess f xs                         = do xs' <- mapM (expose . f) xs
                                             return (unzip [ x | Right x <- xs' ])


-- Handle subtype predicates
closeTransitive env []                  = return (env, [], [])
closeTransitive env ((w,p):pe)
  | isSub' p                            = do assert (a /= b) "Illegal subtype predicate with head" [headsym p]
                                             (pe1,eq1) <- mapSuccess (mkTrans env) [ (n1,n2) | n1 <- below_a, n2 <- [(w,p)] ]
                                             (pe2,eq2) <- mapSuccess (mkTrans env) [ (n1,n2) | n1 <- (w,p):pe1, n2 <- above_b ]
                                             let cycles = filter (uncurry (==)) (map (subsyms . predOf) (pe1++pe2))
                                             assert0 (null cycles) (encodeCircular (nub (a:b:map fst cycles)))
                                             env2 <- addPreds env ((w,p):pe1++pe2)
                                             (env3,pe3,eq3) <- closeTransitive env2 pe
                                             return (env3, pe1++pe2++pe3, eq1++eq2++eq3)
  where (a,b)                           = subsyms p
        below_a                         = nodes (findBelow env a)
        above_b                         = nodes (findAbove env b)
closeTransitive env (_:pe)              = closeTransitive env pe


mkTrans env ((w1,p1), (w2,p2))          = do (pe1, R c1, e1) <- instantiate p1 (EVar w1)
                                             (pe2, R c2, e2) <- instantiate p2 (EVar w2)
                                             let (t1,t1') = subs c1
                                                 (t2',t2) = subs c2
                                             s <- unify env [(t1',t2')]
                                             let t = subst s t1
                                                 p = scheme (t `sub` subst s t2)
                                             (s',qe,f) <- norm (protect p env) (subst s (pe1++pe2))
                                             x  <- newName paramSym
                                             let e = ELam [(x,scheme (subst s' t))] (f (EAp e2 [EAp e1 [EVar x]]))
                                                 (e',p') = qual qe e (subst s' p)
                                             sc <- gen (tevars env) p'
                                             w <- newNameMod (modName env) coercionSym
                                             e' <- redTerm (coercions env) e'
                                             return ((w,sc), (w, e'))

-- Handle class predicates
closeSuperclass env []                  = return (env, [], [])
closeSuperclass env ((w,p):pe)
  | isClass' p                          = do (pe1,eq1) <- mapSuccess (mkSuper env (w,p)) [ n | n <- above_c ]
                                             env1 <- addPreds env ((w,p):pe1)
                                             (env2,pe2,eq2) <- closeSuperclass env1 pe
                                             return (env2, pe1++pe2, eq1++eq2)
  where c                               = headsym p
        above_c                         = filter ((`elem` dom (classEnv env)) . uppersym . predOf) (nodes (findAbove env c))
closeSuperclass env (_:pe)              = closeSuperclass env pe

mkSuper env (w1,p1) (w2,p2)             = do (pe1, R c1, e1) <- instantiate p1 (EVar w1)
                                             (pe2, R c2, e2) <- instantiate p2 (EVar w2)
                                             let (t2',t2) = subs c2
                                             s <- unify env [(c1,t2')]
                                             let p = scheme (subst s t2)
                                             (s',qe,f) <- norm (protect p env) (subst s (pe1++pe2))
                                             let e = f (EAp e2 [e1])
                                                 (e',p') = qual qe e (subst s' p)
                                             sc <- gen (tevars env) p'
                                             w <- newNameMod (modName env) witnessSym
                                             return ((w,sc), (w,e'))
                                             

-- Add predicates to the environment and build overlap graph
addPreds env []                         = return env
addPreds env (n@(w,p):pe)
  | isSub' p                            = case findCoercion env a b of
                                             Just n' -> do 
                                                r <- implications env (predOf n') p
                                                case r of
                                                   Equal -> addPreds (addEqs [(w,nameOf n')] env) pe
                                                   _     -> fail "Ambiguous subtyping"
                                             Nothing -> do 
                                                addPreds (insertSubPred n env) pe
  | isClass' p                          = do r <- cmpNode [] [] (nodes (findClass env c))
                                             case r of
                                                Right (pre,post) -> addPreds (insertClassPred pre n post env) pe
                                                Left w'          -> addPreds (addEqs [(w,w')] env) pe
  where (a,b)                           = subsyms p
        c                               = headsym p

        cmpNode pre post []             = return (Right (pre,post))
        cmpNode pre post (n':pe')       = do r <- implications env (predOf n') p
                                             case r of
                                                Equal      -> return (Left (nameOf n'))
                                                Similar    -> fail "Ambiguous instances"
                                                ImplyRight -> cmpNode pre (nameOf n':post) pe'
                                                ImplyLeft  -> cmpNode (nameOf n':pre) post pe'
                                                Unrelated  -> cmpNode pre post pe'


data Implications                       = Equal | Similar | ImplyRight | ImplyLeft | Unrelated

implications env p1 p2                  = do (R c1,ps1) <- inst p1
                                             (R c2,ps2) <- inst p2
                                             r1 <- expose (unify env [(c1,body p2)])
                                             r2 <- expose (unify env [(body p1,c2)])
                                             case (r1,r2) of
                                                (Right s, Right _) 
                                                  | subst s ps1 == ctxt p2  -> return Equal
                                                  | otherwise               -> return Similar
                                                (Right _, Left _)           -> return ImplyRight
                                                (Left _, Right _)           -> return ImplyLeft
                                                (Left _, Left _)            -> return Unrelated




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

