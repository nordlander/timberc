module Env where

import PP
import Common
import Core
import Depend



data WGraph                             = WG  { nodes :: PEnv,
                                                arcs  :: [(Name,Name)] 
                                              }
                                        deriving Show

nameOf                                  = fst
predOf                                  = snd

--labelOf                                 = fst
--witOf                                   = snd

data Env = Env { kindEnv0      :: KEnv,              -- Kind for each global tycon
                 kindEnv       :: KEnv,              -- Kind for each local skolemized tyvar
                 typeEnv0      :: TEnv,              -- Type scheme for each top-level def (no free tyvars)
                 typeEnv       :: TEnv,              -- Type scheme for each additional def
                 predEnv0      :: PEnv,              -- Predicate scheme for each global witness def (no free tyvars)
                 predEnv       :: PEnv,              -- Predicate scheme for each local witness abstraction

                 tevars        :: [TVar],            -- The tvars free in typeEnv (cached)
                 pevars        :: [TVar],            -- The tvars free in predEnv (cached)
                 stateT        :: Maybe Type,        -- Type of current state scope (invariant: included in typeEnv as well)
                 
                 coercions     :: Eqns,              -- Defs of top-level subtype coercions

                 aboveEnv      :: Map Name WGraph,   -- Overlap graph of all S > T for each T (closed under reflexivity & transitivity)
                 belowEnv      :: Map Name WGraph,   -- Overlap graph of all S < T for each T (closed under reflexivity & transitivity)
                 classEnv      :: Map Name WGraph,   -- Overlap graph of all instances for each type class (closed under subclassing)

                 modName       :: Maybe String, 
                 -- The following fields are only used during constraint reduction:
                 history       :: [Pred],            -- Stack of predicates currently being reduced
                 skolEnv       :: Map Name [TVar],   -- For each skolemized tyvar T: a list of free tvars not unifiable with T
                 pols          :: ([TVar],[TVar]),   -- Pair of tvars occurring in (positive,negative) position in reduction target
                 equalities    :: [(Name,Name)],     -- List of witness names that must be equivalent

                 ticked        :: Bool,              -- Root constraint is an automatically generated coercion (must be removed!)
                 forced        :: Bool,              -- Non-conservative reduction turned on
                 frozen        :: Bool               -- Treat tvars as constants (during env closure)       -- Not yet meaningful
               }

instance Show Env where
    show env = "<env>"


nullEnv                                 = Env { kindEnv0   = [],
                                                kindEnv    = [],
                                                typeEnv0   = [],
                                                typeEnv    = [],
                                                predEnv0   = [],
                                                predEnv    = [],
                                                tevars     = [],
                                                pevars     = [],
                                                stateT     = Nothing,
                                                coercions  = [],
                                                aboveEnv   = [],
                                                belowEnv   = [],
                                                classEnv   = [],
                                                modName    = Nothing,
                                                history    = [],
                                                skolEnv    = [],
                                                pols       = ([],[]),
                                                equalities = [],
                                                ticked     = False,
                                                forced     = False,
                                                frozen     = False
                                          }


addTEnv0 te env                         = env { typeEnv0   = te ++ typeEnv0 env }

addTEnv te env                          = env { typeEnv    = te ++ typeEnv env,
                                                tevars     = tvs `union` tevars env }
  where tvs                             = tvars te

addKEnv0 ke env                         = env { kindEnv0   = ke ++ kindEnv0 env }

addKEnv ke env                          = env { kindEnv    = ke ++ kindEnv env }


addPEnv0 pe env                         = env { predEnv0    = pe ++ predEnv0 env }

addPEnv pe env
  | null (tvars pe)                     = env { predEnv    = pe ++ predEnv env }
  | otherwise                           = error "Internal: Positive predicate with free variables (not yet implemented)"
--                                        = env { predEnv    = pe ++ predEnv env,
--                                                pevars     = nub (tvars pe `union` pevars env) }

addClasses cs env                       = env { classEnv   = ce ++ classEnv env }
  where ce                              = cs `zip` repeat nullWG

noClasses env                           = env { classEnv = [] }

setSelf x t env                         = env' { stateT = Just t }
  where env'                            = addTEnv [(x,scheme (tRef t))] env

addCoercions eqs env                    = env { coercions = filter (isCoercion . fst) eqs ++ coercions env }

addSkolEnv se env                       = env { skolEnv  = se ++ skolEnv env }

skolEnvs env cs                         = concat [ tvs | (c,tvs) <- skolEnv env, c `elem` cs ]

tick env x                              = env { ticked = x }

force env x                             = env { forced = x }

freeze env                              = env { frozen = True }     -- Not yet meaningful

thaw env                                = env { frozen = False }    -- Not yet meaningful

target t env                            = env { pols = polvars env t `pcat` pols env }

protect t env                           = env { pols = pdupl (tvars t) `pcat` pols env }

addEqs eqs env                          = env { equalities = eqs ++ equalities env }

insertClassPred pre n@(w,p) post env    = env { classEnv = insert c wg' (classEnv env) }
  where c                               = headsym p
        ws                              = repeat w
        wg                              = findClass env c
        wg'                             = WG { nodes = insertBefore n post (nodes wg),
                                               arcs  = pre `zip` ws ++ ws `zip` post ++ arcs wg }

insertDefault (_,i1,i2) env                 
  |c1 /= c2                             = error ("Illegal defaulting; heads "++str c1++" and "++str c2++".")
  |otherwise                            = env { classEnv = insert c1 wg' (classEnv env) }
  where c1                              = headsym (snd i1)
        c2                              = headsym (snd i2)
        wg                              = findClass env c1
        n1                              = findInst wg i1
        n2                              = findInst wg i2
        wg'                             = wg { arcs = (n1,n2) : arcs wg }
        findInst wg (i,p)               = case lookup p [(p,n) | (n,p) <- nodes wg] of
                                            Nothing -> error ("No instance " ++ render(pr p))
                                            Just n ->  case i of
                                                         Just n'
                                                          |n /= n' -> error ("Wrong instance name "++show n')
                                                         _         -> n
insertDefaults env ds                   = env2
  where env1                            = foldr insertDefault env ds
        env2                            = env1 {classEnv = reorderWG (classEnv env1)}
        reorderWG []                    = []
        reorderWG ((n,wg) : ps)         = (n,tsort wg) : reorderWG ps
        tsort (WG ns as)                = case dropWhile (null . tail) is of
                                            [] -> WG (order ns is1) (closeTrans g')
                                            xs : _ -> error ("Cyclic default decls: " ++ render(hpr ',' (map (fromJust . flip lookup ns) xs)))
                                          where g = [(n,[m | (a,m) <- as, a==n]) | n <- dom ns]
                                                is = scc g
                                                is1 = reverse (concat is)
                                                g' = order g is1
                                          
        closeTrans []                   = []
        closeTrans ((a,bs):as)          = map (\b -> (a,b)) bs ++ [(a,d) | (c,d) <- cs, c `elem` bs] ++ cs
                                          where cs = closeTrans as
insertSubPred n@(w,p) env               = env { aboveEnv = insert a wg_a (aboveEnv env),
                                                belowEnv = insert b wg_b (belowEnv env) }
  where (a,b)                           = subsyms p
        ws                              = repeat w
        wg_a                            = buildAbove (findAbove env a)
        wg_b                            = buildBelow (findBelow env b)
        
        buildAbove wg                   = WG { nodes = insertBefore n post (nodes wg),
                                               arcs  = pre `zip` ws ++ ws `zip` post ++ arcs wg }
          where syms                    = mapSnd uppersym (nodes wg)
                pre                     = [ w | (w,c) <- syms, hasCoercion env c b ]
                post                    = [ w | (w,c) <- syms, hasCoercion env b c ]

        buildBelow wg                   = WG { nodes = insertBefore n post (nodes wg),
                                               arcs  = pre `zip` ws ++ ws `zip` post ++ arcs wg }
          where syms                    = mapSnd lowersym (nodes wg)
                pre                     = [ w | (w,c) <- syms, hasCoercion env a c ]
                post                    = [ w | (w,c) <- syms, hasCoercion env c a ]


singleWitness env n@(w,p)
  | isSub' p                            = env0 { aboveEnv = [(a,wg)], belowEnv = [(b,wg)] }
  | otherwise                           = env0 { classEnv = [(c,wg)] }
  where c                               = headsym p
        (a,b)                           = subsyms p
        wg                              = unitWG n
        env0                            = env { aboveEnv = [], belowEnv = [], classEnv = [] }



instance Subst WGraph TVar Type where
    subst s (WG ns as)                  = WG (subst s ns) as


instance Subst a TVar Type => Subst (Env,a) TVar Type where
    subst s (env,p)                     = (subst s env, subst s p)


instance Subst Env TVar Type where
    subst [] env                        = env
    subst s env
      | null (pevars env)               = env'
      | otherwise                       = env' { aboveEnv = subst s (aboveEnv env),
                                                 belowEnv = subst s (belowEnv env),
                                                 classEnv = subst s (classEnv env),
                                                 predEnv  = subst s (predEnv env),
                                                 pevars   = substT s (pevars env) }
      where env'                        = env { {- typeEnv  = subst s (typeEnv env), -- redundant -}
                                                 stateT = subst s (stateT env),
                                                 tevars  = substT s (tevars env),
                                                 pols = substP env s (pols env),
                                                 skolEnv = mapSnd (substT s) (skolEnv env) }


sapp s tvs                              = subst s (map TVar tvs)

substT s tvs                            = tvars (sapp s tvs)

substP env s (pvs,nvs)                  = polvars env (sapp s pvs) `pcat` pswap (polvars env (sapp s nvs))


logHistory (env,c)                      = (env { history = c : history env }, c)

conservative (env,c)                    = not (forced env)

findKind0 ke (Tuple n _)                = tupleKind n
findKind0 ke c                          = case lookup c ke of
                                            Just k  -> k
                                            Nothing -> Star  -- Hack!  This alternative is intended for the fresh type
                                                             -- constants introduced when type-checking templates with 
                                                             -- no explicit state type annotations.  The proper handling 
                                                             -- of these names would be to thread an accumulating list 
                                                             -- of generated type declarations through the type-checker
                                                             -- instead, but (1) that would further complicate an already
                                                             -- complex piece of software, and (2) these declarations 
                                                             -- still cannot be made sufficiently polymorphic until the
                                                             -- final substitution has been computed (i.e., after type-
                                                             -- checking).  A more advanced alternative would be to in-
                                                             -- troduce local type declarations to the language, although
                                                             -- issue (2) above would still need to be handled separately.
                                                             -- Considering that the Core2Kindle pass can generate correct
                                                             -- Kindle types given only the generated type name and the 
                                                             -- state variable annotations already present in template
                                                             -- expressions, there is much merit to the shortcut implemented
                                                             -- here.  Note also that unknown type constructor names have
                                                             -- already been trapped and reported during renaming.
                                                    -- error ("Internal: Unknown type constructor: " ++ show c)
                                            
findKind env c                          = findKind0 (kindEnv env ++ kindEnv0 env) c


findType0 te (Tuple n _)                = tupleType n
findType0 te v                          = case lookup v te of
                                            Just sc -> sc
                                            Nothing -> error ("Internal: Unknown identifier: " ++ show v)

findType env v                         = findType0 (typeEnv env ++ typeEnv0 env) v


findExplType env x
  | explicit (annot x)                  = let Scheme t ps ke = findType env x in Scheme (tFun ps t) [] ke
  | otherwise                           = findType env x
  

findPred env w                          = case lookup w (predEnv env ++ predEnv0 env) of
                                            Just p  -> p
                                            Nothing -> error ("Internal: Unknown witness identifier: " ++ show w)


findAbove env c                         = case lookup c (aboveEnv env) of
                                            Just wg -> wg
                                            Nothing -> nullWG


findBelow env c                         = case lookup c (belowEnv env) of
                                            Just wg -> wg
                                            Nothing -> nullWG


findClass env c                         = case lookup c (classEnv env) of
                                            Just wg -> wg
                                            Nothing -> error ("Internal: unknown class identifier: " ++ show c)



findCoercion env a b
  | a == b                              = Just reflAll
  | otherwise                           = search (upperIs b) (nodes (findAbove env a))

findCoercion' env a b                   = case findCoercion env a b of
                                            Just n -> unitWG n
                                            Nothing -> nullWG


upperIs t (w,p)                         = uppersym p == t


hasCoercion env a b                     = findCoercion env a b /= Nothing




{-

Embedding order:
- T[a] < T[b],  a[x] < T[b], b[x] < T[x], a[x] < b[x]

Principles:
- Reduce subtype constraints in embedding order, for maximum accuracy in polarity calculations
- For non-embedded variable bounds, defer invariant variables, to reduce back-tracking
- 


   X     Y
    \   /
     \ /
 Z    Pt   C
 |   / \   |
 |  /   \  |
 | /     \ |
 ZPt     CPt
   \     /
    \   /
    CZPt

a->b  \\  a < Pt, a < b, CPt < b

Pt->Pt,  CPt3->CPt,  CPt->CPt3,  CPt->CPt,  CPt->C

Pt   b
|  / |
| /  |
a   CPt

a->b  \\  a < CPt, a < b, Pt < b

CPt  b
|  / |
| /  |
a    Pt

T < Eq T, S < T, Eq a < Eq b \\ b < a  |-  x < Eq x


-}




findWG (RConCon i j)  (env,_)           = findCoercion' env i j
findWG (ROrd _ Pos i) (env,_)           = addReflWG (findAbove env i)
findWG (ROrd _ Neg i) (env,_)           = addReflWG (findBelow env i)
findWG (RUnif)        (env,_)           = reflWG
findWG (RInv _ Pos i) (env,_)           = concatWG reflWG (findAbove env i)
findWG (RInv _ Neg i) (env,_)           = concatWG reflWG (findBelow env i)
findWG (RClass i)     (env,_)           = lookup' (classEnv env) i
findWG (RVarVar)      (env,_)           = foldl concatWG reflWG (rng (aboveEnv env))


addReflWG wg                            = WG { nodes = reflAll : nodes wg, 
                                               arcs = (repeat (prim Refl) `zip` dom (nodes wg)) ++ arcs wg }
                                               
shrinkWG wg t                           = WG { nodes = filter (upperIs t) (nodes wg), arcs = [] }

flattenWG wg                            = wg { arcs = [] }

reflWG                                  = unitWG (reflAll)

isNullWG wg                             = null (nodes wg)

takeWG (WG (n:nodes) a)                 = (n, WG nodes a)
takeWG _                                = error "Internal: takeWG"

concatWG wg1 wg2                        = WG { nodes = nodes wg1 ++ nodes wg2, arcs = [] }

pruneWG w wg                            = wg { nodes = filter ((`notElem` ws) . fst) (nodes wg) }
  where ws                              = [ w2 | (w1,w2) <- arcs wg, w1 == w ]

unitWG n                                = WG { nodes = [n], arcs = [] }

nullWG                                  = WG { nodes = [], arcs = [] }




data Dir                                = Pos | Neg
                                        deriving (Ord,Eq,Show)

data Rank                               = RFun                  -- function type in either position, handle as a predicate scheme
                                        | RConCon Name Name     -- con-con, only one solution possible (or failure!)
                                        | ROrd    Int Dir Name  -- con-var, with gravity governed by var's position in target type
                                        | RUnif                 -- var-var, unifiable (either safe or forced)
                                        | RInv    Int Dir Name  -- con-var, requires unordered search 
                                        | RClass  Name          -- class constraint, rank below all subpreds involving a tycon
                                        | RVarVar               -- var-var, requires unordered search (empty common solution is likely)
                                        deriving (Ord,Eq,Show)


rank info (env,TFun [l] u)              = subrank (tFlat l) (tFlat u)
  where 
    (emb,vs,lb,ub,lb',ub',pvs)          = info
    approx                              = forced env                 -- solve subtype predicates at all costs
    subrank (TFun _ _, _) _             = RFun                       -- resubmit to predicate scheme reducer
    subrank _ (TFun _ _, _)             = RFun                       -- resubmit to predicate scheme reducer
    subrank (TId i,_) (TId j,_)         = RConCon i j                -- only one choice, highest rank
    subrank (TId i,_) (TVar n,_)
      | l==0 && b==0 && not (isNeg v)   = ROrd 0 Pos i               -- no embeddings, only constant bounds, right polarity
      | approx && n `notElem` vs        = ROrd l Pos i               -- approximate in right direction if n not in environment
      | otherwise                       = RInv l Pos i               -- otherwise rank below v-v unification (dir only for env lookup)
      where l                           = length (filter (==n) emb)  -- # of embeddings of n
            b                           = length (filter (==n) lb)   -- # of lower var bounds for n
            v                           = polarity pvs n             -- polarity of n in target type
    subrank (TVar n,_) (TId i,_)
      | l==0 && b==0 && not (isPos v)   = ROrd 0 Neg i               -- no embeddings, only constant bounds, right polarity
      | approx && n `notElem` vs        = ROrd l Neg i               -- approximate in right direction if n not in environment
      | otherwise                       = RInv l Neg i               -- otherwise rank below v-v unification (dir only for env lookup)
      where l                           = length (filter (==n) emb)  -- # of embeddings of n
            b                           = length (filter (==n) ub)   -- # of upper var bounds for n
            v                           = polarity pvs n             -- polarity of n in target type
    subrank (TVar n,ts) (TVar n',ts')
      | n == n' && ts == ts'            = RUnif     -- identical types, just eliminate the predicate
      | l==0 && b==1 && null ts && not (isPos v)
                                        = RUnif     -- no n embeddings, only one bound (here!), no variance problems, set n = upper bound
      | l'==0 && b'==1 && null ts' && not (isNeg v')
                                        = RUnif     -- no n' embeddings, only one bound (here!), no variance problems, set n' = lower bound
      | approx                          = RUnif     -- eliminate var-to-var predicates early when approximating
      | otherwise                       = RVarVar   -- leave them until last when in conservative mode
      where l                           = length (filter (==n) emb)  -- # of embeddings of n
            b                           = length (filter (==n) ub')  -- # of upper var OR con bounds for n
            v                           = polarity pvs n             -- polarity of n in target type
            l'                          = length (filter (==n') emb) -- # of embeddings of n'
            b'                          = length (filter (==n') lb') -- # of lower var OR con bounds for n'
            v'                          = polarity pvs n'            -- polarity of n' in target type
rank info (env,t)                       = RClass (tId (tHead t))     -- class predicate, rank just above conservative var-to-var


-- m x < n a b
-- m in fv(env), T i < S i j, S i j < S i j, T i < T i
-- s = [n a/m, b/x]
-- n a c < T t
-- m x < n a b, m x < T t
{-
instance Show ([TVar],[TVar],[TVar],[TVar],[TVar],[TVar],([TVar],[TVar])) where
    show (emb,vs,lb,ub,lb',ub',pols)    = "emb = "  ++ show emb ++ "\n" ++
                                          "vs = "   ++ show vs   ++ "\n" ++
                                          "lb = "   ++ show lb   ++ "\n" ++
                                          "ub = "   ++ show ub   ++ "\n" ++
                                          "lb' = "  ++ show lb'  ++ "\n" ++
                                          "ub' = "  ++ show ub'  ++ "\n" ++
                                          "pols = (" ++ show (fst pols)   ++ "," ++ show (snd pols) ++ ")\n"
-}
varInfo gs                              = (emb, vs, lb, ub, lb', ub', polvs)
  where (sPreds,cPreds)                 = partition isSub (map snd gs)
        tts                             = map ((\(t1,t2) -> (tFlat t1, tFlat t2)) . subs) sPreds
        lb                              = [ n | ((TVar _, _), (TVar n, _)) <- tts ]       -- all n with a lower var bound
        ub                              = [ n | ((TVar n, _), (TVar _, _)) <- tts ]       -- all n with an upper var bound
        lb'                             = [ n | (_, (TVar n, _)) <- tts ]                 -- all n with any lower bound
        ub'                             = [ n | ((TVar n, _), _) <- tts ]                 -- all n with any upper bound
        emb                             = concat (map (\((_,ts1),(_,ts2)) -> tvars (ts1++ts2)) tts) -- the vars inside type exps
        vs                              = tevars env                                      -- the vars free in the environment
        polvs                           = pnub (pols env `pcat` pdupl (vs++tvars cPreds)) -- target vars (positive & negative)
        env                             = fst (head gs) -- arbitrary choice, but we only use info that must be equal in all gs



-- Instantiation & generalization ----------------------------------------------------

inst (Scheme t ps ke)           = do ts <- mapM newTVar ks
                                     let s = vs `zip` ts
                                     return (subst s t, subst s ps)
  where (vs,ks)                 = unzip ke


saturate (t,ps) e               = do pe <- newEnv assumptionSym ps
                                     return (pe, t, eAp e (map EVar (dom pe)))


instantiate sc e                = do r <- inst sc
                                     saturate r e


qual [] e sc                    = (e, sc)
qual qe e (Scheme t [] ke)      = (ELam qe e, Scheme t (rng qe) ke)
qual qe (ELam te e) (Scheme t ps ke)
                                = (ELam (qe++te) e, Scheme t (rng qe ++ ps) ke)
qual qe e (Scheme t ps ke)      = (ELam (qe++te) (EAp e (map EVar (dom te))), Scheme t (rng qe ++ ps) ke)
  where te                      = abcSupply `zip` ps


gen tvs0 sc@(Scheme t ps ke)    = do ids <- newNames tyvarSym (length tvs)
                                     let s = tvs `zip` map TId ids
                                         ke' = ids `zip` map tvKind tvs 
                                     return (Scheme (subst s t) (subst s ps) (ke' ++ ke))
  where tvs                     = nub (filter (`notElem` tvs0) (tvars t ++ tvars ps))



-- Variance ------------------------------------------------------------------

polarity (pvs,nvs) tv                   = (tv `elem` pvs, tv `elem` nvs)

isPos                                   = fst
isNeg                                   = snd

covariant                               = (True,False)
contravariant                           = (False,True)
invariant                               = (True,True)
nonvariant                              = (False,False)

pcat (p,n) (p',n')                      = (p++p', n++n')

pswap (p,n)                             = (n, p)

pdupl tvs                               = (tvs,tvs)

pnub (p,n)                              = (nub p, nub n)


class Polvars a where
    polvars :: Env -> a -> ([TVar],[TVar])

instance Polvars a => Polvars [a] where
    polvars env []                      = ([],[])
    polvars env (x:xs)                  = pcat (polvars env x) (polvars env xs)

instance Polvars a => Polvars (Name,a) where
    polvars env (_,t)                   = polvars env t

instance Polvars Type where
    polvars env t                       = polvars env (tFlat t)


instance Polvars (Type,[Type]) where
    polvars env (TFun ts t, [])         = polvars env t  `pcat` pswap (polvars env ts)
    polvars env (TId c, ts)             = pdupl (tvars ts)
      where k                           = findKind env c    -- Future work: let the result be determined by variances encoded in k
    polvars env (TVar n, ts)            = ([n],[]) `pcat` pdupl (tvars ts)
        
instance Polvars Scheme where
    polvars env (Scheme t ps ke)        = polvars env t `pcat` pdupl (tvars ps)

instance Polvars Rho where
    polvars env (R t)                   = polvars env t
    polvars env (F sc rh)               = polvars env rh `pcat` pswap (polvars env sc)

{-
    T a a1 a2 a3 a4 < T a b1 b2 b3 b4 \\ a1<b1, b1<a1, a2<b2, b3<a3
    
    If  T t t1 t2 t3 t4 < T s s1 s2 s3 s4, where s,s1,s2,s3,s4 can have lower as well as upper bounds, then
       t==s may have upper as well as lower bounds
       t1 has an upper and a lower bound: [t1/a1,s1/b1](a1<b1, b1<a1) = t1<s1, s1<t1
       t2 has an upper bound:             [t2/a2,s2/b2](a1<b2)        = t2<s2
       t3 has a lower bound:              [t3/a3,s3/b3](b3<a3)        = s3<t3
       t4 has no bounds:                  [t4/a4,s4/b4]()             = ()
    
    If  T s s1 s2 s3 s4 < T t t1 t2 t3 t4, where s,s1,s2,s3,s4 can have lower as well as upper bounds, then
       t==s may have lower as well as upper bounds
       t1 has a lower and an upper bound: [s1/a1,t1/t1](a1<b1, b1<a1) = s1<t1, t1<s1
       t2 has a lower bound:              [s2/a2,t2/b2](a1<b2)        = s2<t2
       t3 has an uooer bound:             [s3/a3,t3/b3](b3<a3)        = t3<s3
       t4 has no bounds:                  [s4/a4,t4/b4]()             = ()
    
    +: a, a1, a2
    -: a, a1, a3
    (T t t1 t2 t3 t4) in + => (t,t1,t2) in +, (t,t1,t3) in -
    (T t t1 t2 t3 t4( in - => (t,t1,t2) in -, (t,t1,t3) in +
    
-}

-- Printing -------------------------------------------------------------------

instance Pr Env where
    pr env                      = vpr (kindEnv env) $$
                                  vpr (aboveEnv env) $$
                                  vpr (belowEnv env) $$
                                  vpr (classEnv env) $$
                                  vpr (typeEnv env) 

instance Pr (Name,WGraph) where
    pr (_, WG ns as)            = vpr ns $$ nest 4 (vpr as)

instance Pr (Name,Name) where
    pr (n,n')                   = prId n <+> text "<" <+> prId n'


-- initEnv --------------------------------------------------------------------

initEnv                 = nullEnv { kindEnv0 = primKindEnv,
                                    typeEnv0 = primTypeEnv,
                                    aboveEnv = primAboveEnv,
                                    belowEnv = primBelowEnv,
                                    classEnv = primClassEnv,
                                    predEnv0 = primPredEnv  }



primKindEnv             = [ (prim Action,       Star),
                            (prim Request,      KFun Star Star),
                            (prim Template,     KFun Star Star),
                            (prim Cmd,          KFun Star (KFun Star Star)),
                                    
                            (prim Msg,          Star),
                            (prim Ref,          KFun Star Star),
                            (prim PID,          Star),
                            (prim PMC,          KFun Star Star),
                            (prim Time,         Star),
                                    
                            (prim Int,          Star),
                            (prim Float,        Star),
                            (prim Char,         Star),
                            (prim Bool,         Star),

                            (prim Array,        KFun Star Star),

                            (prim LIST,         KFun Star Star),
                            (prim UNITTYPE,     Star) ]


primTypeEnv             = [ (prim UNITTERM,     scheme0 [] tUnit),
                            (prim NIL,          scheme1 [] (tList a)),
                            (prim CONS,         scheme1 [a,tList a] (tList a)),

                            (prim FALSE,        scheme0 [] tBool),
                            (prim TRUE,         scheme0 [] tBool),

                            (prim Refl,         scheme1 [a] a),

                            (prim ActToCmd,     scheme1 [tAction] (tCmd a tMsg)),
                            (prim ReqToCmd,     scheme2 [tRequest a] (tCmd b a)),
                            (prim TemplToCmd,   scheme2 [tTemplate a] (tCmd b a)),
                            (prim RefToPID,     scheme1 [tRef a] tPID),

                            (prim IntPlus,      scheme0 [tInt,tInt] tInt),
                            (prim IntMinus,     scheme0 [tInt,tInt] tInt),
                            (prim IntTimes,     scheme0 [tInt,tInt] tInt),
                            (prim IntDiv,       scheme0 [tInt,tInt] tInt),
                            (prim IntMod,       scheme0 [tInt,tInt] tInt),
                            (prim IntNeg,       scheme0 [tInt] tInt),

                            (prim IntEQ,        scheme0 [tInt,tInt] tBool),
                            (prim IntNE,        scheme0 [tInt,tInt] tBool),
                            (prim IntLT,        scheme0 [tInt,tInt] tBool),
                            (prim IntLE,        scheme0 [tInt,tInt] tBool),
                            (prim IntGE,        scheme0 [tInt,tInt] tBool),
                            (prim IntGT,        scheme0 [tInt,tInt] tBool),

                            (prim FloatPlus,    scheme0 [tFloat,tFloat] tFloat),
                            (prim FloatMinus,   scheme0 [tFloat,tFloat] tFloat),
                            (prim FloatTimes,   scheme0 [tFloat,tFloat] tFloat),
                            (prim FloatDiv,     scheme0 [tFloat,tFloat] tFloat),
                            (prim FloatNeg,     scheme0 [tFloat] tFloat),

                            (prim FloatEQ,      scheme0 [tFloat,tFloat] tBool),
                            (prim FloatNE,      scheme0 [tFloat,tFloat] tBool),
                            (prim FloatLT,      scheme0 [tFloat,tFloat] tBool),
                            (prim FloatLE,      scheme0 [tFloat,tFloat] tBool),
                            (prim FloatGE,      scheme0 [tFloat,tFloat] tBool),
                            (prim FloatGT,      scheme0 [tFloat,tFloat] tBool),

                            (prim IntToFloat,   scheme0 [tInt] tFloat),
                            (prim FloatToInt,   scheme0 [tFloat] tInt),

                            (prim CharToInt,    scheme0 [tChar] tInt),
                            (prim IntToChar,    scheme0 [tInt] tChar),

                            (prim LazyOr,       scheme0 [tBool,tBool] tBool),
                            (prim LazyAnd,      scheme0 [tBool,tBool] tBool),

                            (prim MsgEQ,        scheme0 [tMsg,tMsg] tBool),
                            (prim MsgNE,        scheme0 [tMsg,tMsg] tBool),

                            (prim PidEQ,        scheme0 [tPID,tPID] tBool),
                            (prim PidNE,        scheme0 [tPID,tPID] tBool),

                            (prim Sec,          scheme0 [tInt] tTime),
                            (prim MilliSec,     scheme0 [tInt] tTime),
                            (prim MicroSec,     scheme0 [tInt] tTime),
                            (prim NanoSec,      scheme0 [tInt] tTime),
                            (prim Infinity,     scheme0 [] tTime),
                                
                            (prim TimePlus,     scheme0 [tTime,tTime] tTime),
                            (prim TimeMinus,    scheme0 [tTime,tTime] tTime),
                            (prim TimeMin,      scheme0 [tTime,tTime] tTime),
                                
                            (prim TimeEQ,       scheme0 [tTime,tTime] tBool),
                            (prim TimeNE,       scheme0 [tTime,tTime] tBool),
                            (prim TimeLT,       scheme0 [tTime,tTime] tBool),
                            (prim TimeLE,       scheme0 [tTime,tTime] tBool),
                            (prim TimeGE,       scheme0 [tTime,tTime] tBool),
                            (prim TimeGT,       scheme0 [tTime,tTime] tBool),

                            (prim Raise,        scheme1 [tInt] a),          -- temporary
                            (prim Catch,        scheme0 [] tUnit),          -- temporary
                            
                            (prim ListArray,    scheme1 [tList a] (tArray a)),
                            (prim ConstArray,   scheme1 [tInt, a] (tArray a)),
                            (prim SizeArray,    scheme1 [tArray a] tInt),
                            (prim IndexArray,   scheme1 [tArray a, tInt] a),
                            (prim UpdateArray,  scheme2 [tArray a, tInt, a] (tCmd b tUnit)),
                            (prim CloneArray,   scheme1 [tArray a, tInt] (tArray a)),

                            (prim Fail,         scheme1 [] (tPMC a)),
                            (prim Commit,       scheme1 [a] (tPMC a)),
                            (prim Match,        scheme1 [tPMC a] a),
                            (prim Fatbar,       scheme1 [tPMC a, tPMC a] (tPMC a)),

                            (prim After,        scheme0 [tTime,tAction] tAction),
                            (prim Before,       scheme0 [tTime,tAction] tAction),

                            (prim ASYNC,        scheme0 [tMsg, tTime, tTime] tUnit),
                            (prim LOCK,         scheme0 [tPID] tUnit),
                            (prim UNLOCK,       scheme0 [tPID] tUnit),
                            (prim Inherit,      scheme0 [] tTime) 

                          ]


tAction                 = TId (prim Action)
tRequest a              = TAp (TId (prim Request)) a
tTemplate a             = TAp (TId (prim Template)) a
tCmd a b                = TAp (TAp (TId (prim Cmd)) a) b
tTime                   = TId (prim Time)
tMsg                    = TId (prim Msg)
tRef a                  = TAp (TId (prim Ref)) a
tPID                    = TId (prim PID)
tPMC a                  = TAp (TId (prim PMC)) a
tInt                    = TId (prim Int)
tFloat                  = TId (prim Float)
tChar                   = TId (prim Char)
tBool                   = TId (prim Bool)
tArray a                = TAp (TId (prim Array)) a
tList a                 = TAp (TId (prim LIST)) a
tUnit                   = TId (prim UNITTYPE)
        
a                       = TId (name0 "a")
b                       = TId (name0 "b")
        
scheme0 ts t            = Scheme (mkRho ts t) [] []
scheme1 ts t            = Scheme (mkRho ts t) [] [(name0 "a",Star)]
scheme2 ts t            = Scheme (mkRho ts t) [] [(name0 "a",Star),(name0 "b",Star)]

mkRho [] t              = R t
mkRho ts t              = F (map scheme ts) (R t)

primAboveEnv    = [ (prim Action,   unitWG subActCmd),
                    (prim Request,  unitWG subReqCmd),
                    (prim Template, unitWG subTemplCmd),
                    (prim Cmd,      nullWG),
                    (prim Ref,      unitWG subRefPID),
                    (prim PID,      nullWG)
                  ]

primBelowEnv    = [ (prim Action,   nullWG),
                    (prim Request,  nullWG),
                    (prim Template, nullWG),
                    (prim Cmd,      WG [subActCmd,subReqCmd,subTemplCmd] []),
                    (prim Ref,      nullWG),
                    (prim PID,      unitWG subRefPID)
                  ]


reflAll         = (prim Refl,       scheme1 [] (a `sub` a))

subActCmd       = (prim ActToCmd,   scheme1 [] (tAction `sub` tCmd a tMsg))

subReqCmd       = (prim ReqToCmd,   scheme2 [] (tRequest a `sub` tCmd b a))

subTemplCmd     = (prim TemplToCmd, scheme2 [] (tTemplate a `sub` tCmd b a))

subRefPID       = (prim RefToPID,   scheme1 [] (tRef a `sub` tPID))


primPredEnv     = [reflAll, subActCmd, subReqCmd, subTemplCmd, subRefPID]


primClassEnv    = []
