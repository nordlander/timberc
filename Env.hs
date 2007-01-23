module Env where

import PP
import Common
import Core
import Depend



data WGraph                             = WG  { nodes :: Map Int Wit,
                                                arcs  :: [(Int,Int)] 
                                              }
                                        deriving Show

type Wit                                = (Exp,Scheme)

expOf                                   = fst
predOf                                  = snd

labelOf                                 = fst
witOf                                   = snd

data Env = Env { kindEnv       :: KEnv,            -- Kind for each global tycon AND each local skolemized tyvar
                 typeEnv0      :: TEnv,            -- Type scheme for each top-level def (no free tyvars)
                 typeEnv       :: TEnv,            -- Type scheme for each additional def
                 predEnv       :: PEnv,            -- Predicate scheme for each local witness abstraction

                 tevars        :: [TVar],          -- The tvars free in typeEnv (cached)
                 pevars        :: [TVar],          -- The tvars free in predEnv (cached)
                 stateT        :: Maybe Type,      -- Type of current state scope (invariant: included in typeEnv as well)

                 aboveEnv      :: Map Name WGraph, -- Overlap graph of all S > T for each T (closed under reflexivity & transitivity)
                 belowEnv      :: Map Name WGraph, -- Overlap graph of all S < T for each T (closed under reflexivity & transitivity)
                 classEnv      :: Map Name WGraph, -- Overlap graph of all instances for each type class (closed under subclassing)

                 -- The following fields are only used during constraint reduction:
                 history       :: [Pred],          -- Stack of predicates currently being reduced
                 skolEnv       :: Map Name [TVar], -- For each skolemized tyvar T: a list of free tvars not unifiable with T
                 pols          :: ([TVar],[TVar]), -- Pair of tvars occurring in (positive,negative) position in reduction target

                 ticked        :: Bool,            -- Root constraint is an automatically generated coercion (must be removed!)
                 forced        :: Bool,            -- Non-conservative reduction turned on
                 frozen        :: Bool             -- Treat tvars as constants (during env closure)
               }

instance Show Env where
    show env = "<env>"


nullEnv                                 = Env { kindEnv  = [],
                                                typeEnv0 = [],
                                                typeEnv  = [],
                                                predEnv  = [],
                                                tevars   = [],
                                                pevars   = [],
                                                stateT   = Nothing,
                                                aboveEnv = [],
                                                belowEnv = [],
                                                classEnv = [],
                                                history  = [],
                                                skolEnv  = [],
                                                pols     = ([],[]),
                                                ticked   = False,
                                                forced   = False,
                                                frozen   = False
                                          }


addTEnv0 te env                         = env { typeEnv0   = te ++ typeEnv0 env }

addTEnv te env                          = env { typeEnv    = te ++ typeEnv env,
                                                tevars     = tvs `union` tevars env }
  where tvs                             = tvars te

addKEnv ke env                          = env { kindEnv    = ke ++ kindEnv env }

addPEnv pe env                          = env { predEnv    = pe ++ predEnv env,
                                                pevars     = nub (tvars pe `union` pevars env) }

addAEnv ae env                          = env { aboveEnv   = ae ++ aboveEnv env }

addBEnv be env                          = env { belowEnv   = be ++ belowEnv env }

addCEnv ce env                          = env { classEnv   = ce ++ classEnv env }

insertAbove c wg env                    = env { aboveEnv   = insert c wg (aboveEnv env) }

insertBelow c wg env                    = env { belowEnv   = insert c wg (belowEnv env) }

insertClass c wg env                    = env { classEnv   = insert c wg (classEnv env) }

noClasses env                           = env { classEnv = [] }

setSelf x t env                         = env' { stateT = Just t }
  where env'                            = addTEnv [(x,scheme (tRef t))] env

addSkolEnv se env                       = env { skolEnv  = se ++ skolEnv env }

skolEnvs env cs                         = concat [ tvs | (c,tvs) <- skolEnv env, c `elem` cs ]

tick env x                              = env { ticked = x }

force env x                             = env { forced = x }

freeze env                              = env { frozen = True }

thaw env                                = env { frozen = False }

target t env                            = env { pols = polvars env t `pcat` pols env }

protect t env                           = env { pols = pdupl (tvars t) `pcat` pols env }

unitWG wit                              = WG { nodes = [(0,wit)], arcs = [] }

nullWG                                  = WG { nodes = [], arcs = [] }


initSubs cs wits env                    = addAEnv pgs (addBEnv pgs env)
  where pgs                             = cs `zip` map unitWG wits


initClasses cs env                      = addCEnv pgs env
  where pgs                             = cs `zip` repeat nullWG


buildAbove l wit env                    = insertAbove a wg' env
  where (a,b)                           = subsyms (predOf wit)
        wg                              = findAbove env a
        ls                              = repeat l
        syms                            = mapSnd (uppersym . predOf) (nodes wg)
        pre                             = [ l | (l,c) <- syms, c==a || hasCoercion env c b ]
        post                            = [ l | (l,c) <- syms, hasCoercion env b c ]
        wg'                             = WG { nodes = insertBefore (l,wit) post (nodes wg),
                                               arcs  = pre `zip` ls ++ ls `zip` post ++ arcs wg }

buildBelow l wit env                    = insertBelow b wg' env
  where (a,b)                           = subsyms (predOf wit)
        wg                              = findBelow env b
        ls                              = repeat l
        syms                            = mapSnd (lowersym . predOf) (nodes wg)
        pre                             = [ w | (w,c) <- syms, c==b || hasCoercion env a c ]
        post                            = [ w | (w,c) <- syms, hasCoercion env c a ]
        wg'                             = WG { nodes = insertBefore (l,wit) post (nodes wg),
                                               arcs  = pre `zip` ls ++ ls `zip` post ++ arcs wg }



singleWitness env wit
  | isSub' p                            = insertAbove a wg' (insertBelow b wg' env0)
  | otherwise                           = insertClass c wg' env0
  where p                               = predOf wit
        c                               = headsym p
        (a,b)                           = subsyms p
        wg'                             = unitWG wit
        env0                            = env { aboveEnv = [], 
                                                belowEnv = [], 
                                                classEnv = [] }



instance Subst WGraph TVar Type where
    subst s (WG ns as)                  = WG (subst s ns) as

instance Subst (Int,(Exp,Scheme)) TVar Type where
    subst s (l,(e,p))                   = (l, (e,subst s p))

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


findKind env c                          = case lookup c (kindEnv env) of
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


findType env v                          = case lookup v (typeEnv env ++ typeEnv0 env) of
                                            Just t  -> t
                                            Nothing -> error ("Internal: Unknown identifier: " ++ show v)
 

findPred env w                          = case lookup w (predEnv env ++ typeEnv0 env) of
                                            Just p  -> p
                                            Nothing -> error ("UInternal: nknown witness identifier: " ++ show w)


findAbove env c                         = lookup' (aboveEnv env) c


findBelow env c                         = lookup' (belowEnv env) c


findClass env c                         = lookup' (classEnv env) c



findCoercion env a b                    = do wg <- lookup a (aboveEnv env)
                                             search (upperIs b) (nodes wg)

findCoercion' env a b                   = case findCoercion env a b of
                                            Just (l,wit) -> wit
                                            Nothing      -> error ("Internal: findCoercion " ++ show a ++ " " ++ show b)


upperIs t (l,wit)                       = uppersym (predOf wit) == t     -- undefined???


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




findWG (RConCon i j)  (env,_)           = shrinkWG (lookup' (aboveEnv env) i) j
findWG (ROrd _ Pos i) (env,_)           = lookup' (aboveEnv env) i
findWG (ROrd _ Neg i) (env,_)           = lookup' (belowEnv env) i
findWG (RUnif)        (env,_)           = reflWG
findWG (RInv _ Pos i) (env,_)           = flattenWG (lookup' (aboveEnv env) i)
findWG (RInv _ Neg i) (env,_)           = flattenWG (lookup' (belowEnv env) i)
findWG (RClass i)     (env,_)           = lookup' (classEnv env) i
findWG (RVarVar)      (env,_)           = foldr concatWG nullWG (rng (aboveEnv env))


shrinkWG wg t                           = wg { nodes = filter (upperIs t) (nodes wg), arcs = [] }

flattenWG wg                            = wg { arcs = [] }

reflWG                                  = unitWG (reflAll)

isNullWG wg                             = null (nodes wg)

takeWG (WG ((l,wit):n) a)               = (l, wit, WG n a)
takeWG _                                = error "Internal: takeWG"

concatWG wg1 wg2                        = WG { nodes = nodes wg1 ++ nodes wg2, arcs = [] }

pruneWG l wg                            = wg { nodes = filter ((`notElem` ls) . fst) (nodes wg) }
  where ls                              = [ l2 | (l1,l2) <- arcs wg, l1 == l ]



data Dir                                = Pos | Neg
                                        deriving (Ord,Eq,Show)

data Rank                               = RConCon Name Name     -- con-con, only one solution
                                        | ROrd    Int Dir Name  -- con-var, with gravity
                                        | RUnif                 -- var-var, unifiable
                                        | RInv    Int Dir Name  -- con-var, unordered
                                        | RClass  Name          -- class constraint
                                        | RVarVar               -- var-var, preserve
                                        deriving (Ord,Eq,Show)


rank info (env,TFun [l] u)              = subrank (tFlat l) (tFlat u)
  where 
    (emb,vs,lb,ub,lb',ub',pols)         = info
    approx                              = forced env
    subrank (TId i,_) (TId j,_)         = RConCon i j
    subrank (TId i,_) (TVar n,_)
      | l==0 && b==0 && not (isNeg v)   = ROrd 0 Pos i
      | approx && n `notElem` vs        = ROrd l Pos i
      | otherwise                       = RInv l Pos i
      where l                           = length (filter (==n) emb)
            b                           = length (filter (==n) lb)   -- var bounds
            v                           = polarity pols n
    subrank (TVar n,_) (TId i,_)
      | l==0 && b==0 && not (isPos v)   = ROrd 0 Neg i
      | approx && n `notElem` vs        = ROrd l Neg i
      | otherwise                       = RInv l Neg i
      where l                           = length (filter (==n) emb)
            b                           = length (filter (==n) ub)   -- var bounds
            v                           = polarity pols n
    subrank (TVar n,ts) (TVar n',ts')
      | n == n' && ts == ts'            = RUnif
      | l==0 && b==1 && null ts && not (isPos v)
                                        = RUnif
      | l'==0 && b'==1 && null ts' && not (isNeg v')
                                        = RUnif
      | approx                          = RUnif
      | otherwise                       = RVarVar
      where l                           = length (filter (==n) emb)
            b                           = length (filter (==n) ub')  -- var OR con bounds
            v                           = polarity pols n
            l'                          = length (filter (==n') emb)
            b'                          = length (filter (==n') lb') -- var OR con bounds
            v'                          = polarity pols n'
rank info (env,t)                       = RClass (tId (tHead t))


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
varInfo gs                              = (emb,vs,lb,ub,lb',ub',polvs)
  where (subs,cls)                      = partition isSub (map snd gs)
        tts                             = map ((\[t1,t2] -> (tFlat t1, tFlat t2)) . snd . tFlat) subs
        lb                              = [ n | ((TVar _, _), (TVar n, _)) <- tts ]
        ub                              = [ n | ((TVar n, _), (TVar _, _)) <- tts ]
        lb'                             = [ n | (_, (TVar n, _)) <- tts ]
        ub'                             = [ n | ((TVar n, _), _) <- tts ]
        emb                             = concat (map (\((_,ts1),(_,ts2)) -> tvars ts1 ++ tvars ts2) tts)
        env                             = fst (head gs)
        vs                              = tevars env
        vs'                             = vs ++ tvars cls
        (pvs,nvs)                       = pols env
        polvs                           = (nub (vs'++pvs), nub (vs'++nvs))
                                        



-- Instantiation & generalization ----------------------------------------------------

inst (Scheme t ps ke)           = do ts <- mapM newTVar ks
                                     let s = vs `zip` ts
                                     return (subst s t, subst s ps)
  where (vs,ks)                 = unzip ke


instantiate sc e                = do (t,ps) <- inst sc
                                     pe <- newEnv assumptionSym ps
                                     return (pe, t, eAp (esig e t ps) (map EVar (dom pe)))
  where esig e t ps             = if null (pquant sc) then e else ESig e (Scheme t ps [])


qual qe e (Scheme t ps ke)      = (eLam qe e, Scheme t (rng qe ++ ps) ke)


gen env sc@(Scheme t ps ke)     = do ids <- newNames paramSym (length tvs)
                                     let s = tvs `zip` map TId ids
                                         s' = tvs `zip` map (TId . mkLocal) ids
                                         ke' = ids `zip` map tvKind tvs 
                                     return (s', Scheme (subst s t) (subst s ps) (ke' ++ ke))
  where tvs                     = nub (filter (`notElem` tevars env) (tvars t ++ tvars ps))



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
    polvars env (t@(TId c), ts)         = polvars env ps `pcat` pswap (polvars env ns) `pcat` pdupl (tvars zs)
      where wit                         = findCoercion' env c c
            p                           = predOf wit
            (ps0,ns0)                   = unzip (map (subs . pbody) (pctxt p))
            (tlo,tup)                   = subs (pbody p)
            vs                          = tyvars tlo
            s                           = vs `zip` ts
            ps                          = [ t | (v,t) <- s, v `elem` tyvars ps0 ]
            ns                          = [ t | (v,t) <- s, v `elem` tyvars ns0 ]
            zs                          = [ t | (v,t) <- s, v `elem` tyvars tup ]
    polvars env (TVar n, ts)            = ([n],[]) `pcat` pdupl (tvars ts)
        
instance Polvars Scheme where
    polvars env (Scheme r ps ke)        = (vs,vs) `pcat` polvars env r
      where vs                          = tvars ps

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

instance Pr (Int,(Exp,Scheme)) where
    pr (l,(e,p))                = text (show l) <+> text ":" <+> pr e <+> text "::" <+> pr p

instance Pr (Name,WGraph) where
    pr (_, WG ns as)            = vpr ns $$ nest 4 (vpr as)

instance Pr (Int,Int) where
    pr (l,l')                   = text (show l ++ " < " ++ show l')


-- initEnv --------------------------------------------------------------------

initEnv                 = nullEnv { kindEnv  = primKindEnv,
                                    typeEnv0 = primTypeEnv,
                                    aboveEnv = commonWGs ++ aboveWGs,
                                    belowEnv = commonWGs ++ belowWGs,
                                    classEnv = []  }



primKindEnv             = [ (prim Action,       Star),
                            (prim Request,      KFun Star Star),
                            (prim Template,     KFun Star Star),
                            (prim Cmd,          KFun Star (KFun Star Star)),
                                    
                            (prim Msg,          Star),
                            (prim Ref,          KFun Star Star),
                            (prim PID,          KFun Star Star),
                            (prim PMC,          Star),
                            (prim Time,         Star),
                                    
                            (prim Int,          Star),
                            (prim Float,        Star),
                            (prim Char,         Star),
                            (prim Bool,         Star),

                            (prim LIST,         KFun Star Star),
                            (prim UNIT,         Star) ]


primTypeEnv             = [ (prim UNIT,         scheme0 tUnit),
                            (prim NIL,          scheme1 (tList a)),
                            (prim CONS,         scheme1 (TFun [a,tList a] (tList a))),

                            (prim FALSE,        scheme0 tBool),
                            (prim TRUE,         scheme0 tBool),

                            (prim Refl,         scheme1 (TFun [a] a)),

                            (prim ActToCmd,     scheme1 (TFun [tAction] (tCmd a tMsg))),
                            (prim ReqToCmd,     scheme2 (TFun [tRequest a] (tCmd b a))),
                            (prim TemplToCmd,   scheme2 (TFun [tTemplate a] (tCmd b a))),
                            (prim RefToPID,     scheme1 (TFun [tRef a] tPID)),

                            (prim IntPlus,      scheme0 (TFun [tInt,tInt] tInt)),
                            (prim IntMinus,     scheme0 (TFun [tInt,tInt] tInt)),
                            (prim IntTimes,     scheme0 (TFun [tInt,tInt] tInt)),
                            (prim IntDiv,       scheme0 (TFun [tInt,tInt] tInt)),
                            (prim IntMod,       scheme0 (TFun [tInt,tInt] tInt)),
                            (prim IntNeg,       scheme0 (TFun [tInt] tInt)),

                            (prim IntEQ,        scheme0 (TFun [tInt,tInt] tBool)),
                            (prim IntNE,        scheme0 (TFun [tInt,tInt] tBool)),
                            (prim IntLT,        scheme0 (TFun [tInt,tInt] tBool)),
                            (prim IntLE,        scheme0 (TFun [tInt,tInt] tBool)),
                            (prim IntGE,        scheme0 (TFun [tInt,tInt] tBool)),
                            (prim IntGT,        scheme0 (TFun [tInt,tInt] tBool)),

                            (prim FloatPlus,    scheme0 (TFun [tFloat,tFloat] tFloat)),
                            (prim FloatMinus,   scheme0 (TFun [tFloat,tFloat] tFloat)),
                            (prim FloatTimes,   scheme0 (TFun [tFloat,tFloat] tFloat)),
                            (prim FloatDiv,     scheme0 (TFun [tFloat,tFloat] tFloat)),
                            (prim FloatNeg,     scheme0 (TFun [tFloat] tFloat)),

                            (prim FloatEQ,      scheme0 (TFun [tFloat,tFloat] tBool)),
                            (prim FloatNE,      scheme0 (TFun [tFloat,tFloat] tBool)),
                            (prim FloatLT,      scheme0 (TFun [tFloat,tFloat] tBool)),
                            (prim FloatLE,      scheme0 (TFun [tFloat,tFloat] tBool)),
                            (prim FloatGE,      scheme0 (TFun [tFloat,tFloat] tBool)),
                            (prim FloatGT,      scheme0 (TFun [tFloat,tFloat] tBool)),

                            (prim CharToInt,    scheme0 (TFun [tChar] tInt)),
                            (prim IntToChar,    scheme0 (TFun [tInt] tChar)),

                            (prim MsgEQ,        scheme0 (TFun [tMsg,tMsg] tBool)),
                            (prim MsgNE,        scheme0 (TFun [tMsg,tMsg] tBool)),

                            (prim PidEQ,        scheme0 (TFun [tPID,tPID] tBool)),
                            (prim PidNE,        scheme0 (TFun [tPID,tPID] tBool)),

                            (prim Fail,         scheme1 (tPMC a)),
                            (prim Commit,       scheme1 (TFun [a] (tPMC a))),
                            (prim Match,        scheme1 (TFun [tPMC a] a)),
                            (prim Fatbar,       scheme1 (TFun [tPMC a, tPMC a] (tPMC a))),

                            (prim After,        scheme0 tUnit),
                            (prim Before,       scheme0 tUnit),

                            (prim Baseline,     scheme0 (TFun [tMsg] tTime)),
                            (prim Deadline,     scheme1 (TFun [tMsg] tTime)) ]

primCurrentEnv          = [ (prim Current,      scheme0 tMsg) ]


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
tList a                 = TAp (TId (prim LIST)) a
tUnit                   = TId (prim UNIT)
        
a                       = TId (name0 "a")
b                       = TId (name0 "b")
        
scheme0 t               = Scheme (R t) [] []
scheme1 t               = Scheme (R t) [] [(name0 "a",Star)]
scheme2 t               = Scheme (R t) [] [(name0 "a",Star),(name0 "b",Star)]



commonWGs       = [ (prim Msg,      WG [(0,reflMsg)]    []),
                    (prim PMC,      WG [(0,reflPMC)]    []),

                    (prim Int,      WG [(0,reflInt)]    []),
                    (prim Float,    WG [(0,reflFloat)]  []),
                    (prim Char,     WG [(0,reflChar)]   []),
                    (prim Bool,     WG [(0,reflBool)]   []),

                    (prim LIST,     WG [(0,reflList)]   []),
                    (prim UNIT,     WG [(0,reflUnit)]   [])
                  ]

aboveWGs        = [ (prim Action,   WG [(0,reflAct), (1, subActCmd)]     [(0,1)]),
                    (prim Request,  WG [(0,reflReq), (1, subReqCmd)]     [(0,1)]),
                    (prim Template, WG [(0,reflTempl), (1, subTemplCmd)] [(0,1)]),
                    (prim Cmd,      WG [(0,reflCmd)]                     []),

                    (prim Ref,      WG [(0,reflRef), (1,subRefPID)]      [(0,1)]),
                    (prim PID,      WG [(0,reflPID)]                     [])
                  ]

belowWGs        = [ (prim Action,   WG [(0,reflAct)]                     []),
                    (prim Request,  WG [(0,reflReq)]                     []),
                    (prim Template, WG [(0,reflTempl)]                   []),
                    (prim Cmd,      WG [(0,reflCmd),(1,subActCmd),(2,subReqCmd),(3,subTemplCmd)] 
                                                                         [(0,1),(0,2),(0,3)]),
                    (prim Ref,      WG [(0,reflRef)]                     []),
                    (prim PID,      WG [(0,reflPID),(1,subRefPID)]       [(0,1)])
                  ]


reflAll         = (ePrim Refl,  scheme1 (a `sub` a))

reflAct         = (ePrim Refl,  scheme0 (tAction `sub` tAction))
reflReq         = (ePrim Refl,  scheme1 (tRequest a `sub` tRequest a))
reflTempl       = (ePrim Refl,  scheme1 (tTemplate a `sub` tTemplate a))
reflCmd         = (ePrim Refl,  scheme2 (tCmd b a `sub` tCmd b a))

reflMsg         = (ePrim Refl,  scheme0 (tMsg `sub` tMsg))
reflRef         = (ePrim Refl,  scheme1 (tRef a `sub` tRef a))
reflPID         = (ePrim Refl,  scheme0 (tPID `sub` tPID))
reflPMC         = (ePrim Refl,  scheme1 (tPMC a `sub` tPMC a))

reflInt         = (ePrim Refl,  scheme0 (tInt `sub` tInt))
reflFloat       = (ePrim Refl,  scheme0 (tFloat `sub` tFloat))
reflChar        = (ePrim Refl,  scheme0 (tChar `sub` tChar))
reflBool        = (ePrim Refl,  scheme0 (tBool `sub` tBool))

reflList        = (ePrim Refl,  scheme1 (tList a `sub` tList a))
reflUnit        = (ePrim Refl,  scheme0 (tUnit `sub` tUnit))


subActCmd       = (ePrim ActToCmd,   scheme1 (tAction `sub` tCmd a tMsg))

subReqCmd       = (ePrim ReqToCmd,   scheme2 (tRequest a `sub` tCmd b a))

subTemplCmd     = (ePrim TemplToCmd, scheme2 (tTemplate a `sub` tCmd b a))

subRefPID       = (ePrim RefToPID,   scheme1 (tRef a `sub` tPID))


ePrim p         = EVar (prim p)

