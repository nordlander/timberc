module Core where

import Common
import PP
import Data.Binary


data Module     = Module Name [Name] [Default] Types Binds Binds
                deriving  (Eq,Show)

data Types      = Types   KEnv Decls
                deriving  (Eq,Show)

data Binds      = Binds   Bool TEnv Eqns
                deriving  (Eq,Show)

type Default    = (Bool,Inst,Inst)

type Inst       = (Maybe Name,Scheme)

type PEnv       = TEnv

type TEnv       = Map Name Scheme

type CEnv       = Map Name Constr

type Decls      = Map Name Decl

type Eqns       = Map Name Exp

data Decl       = DData     [Name] [Scheme] CEnv
                | DRec Bool [Name] [Scheme] TEnv
                | DType     [Name] Type
                deriving  (Eq,Show)

type PScheme    = Scheme

type Pred       = Type

data Scheme     = Scheme  Rho [PScheme] KEnv
                deriving  (Eq,Show)

data Constr     = Constr  [Scheme] [PScheme] KEnv
                deriving (Eq,Show)

data Rho        = R       Type
                | F       [Scheme] Rho
                deriving (Eq,Show)

data Type       = TId     Name
                | TVar    TVar
                | TFun    [Type] Type
                | TAp     Type Type
                deriving  (Eq,Show)

type Alt        = (Pat, Exp)

data Pat        = PCon    Name
                | PLit    Lit
                deriving (Eq,Show)

data Exp        = ECon    Name
                | ESel    Exp Name
                | EVar    Name
                | ELam    TEnv Exp
                | EAp     Exp [Exp]
                | ELet    Binds Exp
                | ECase   Exp [Alt] Exp
                | ERec    Name Eqns
                | ELit    Lit

                | EAct    Exp Exp
                | EReq    Exp Exp
                | ETempl  Name Type TEnv Cmd
                | EDo     Name Type Cmd
                deriving (Eq,Show)

data Cmd        = CGen    Name Type Exp Cmd
                | CAss    Name Exp Cmd
                | CLet    Binds Cmd
                | CRet    Exp
                | CExp    Exp
                deriving (Eq,Show)



litType (LInt i)                = TId (prim Int)
litType (LRat r)                = TId (prim Float)
litType (LChr c)                = TId (prim Char)
litType (LStr s)                = error "Internal chaos: Core.litType LStr"


tupleKind n                     = foldr KFun Star (replicate n Star)


tupleType n                     = Scheme (tFun' (map scheme ts) t) [] (vs `zip` repeat Star)
  where vs                      = take n abcSupply
        ts                      = map TId vs
        t                       = tAp (TId (tuple n)) ts


newTVar k                       = fmap TVar (newTV k)


nullCon                         = Constr [] [] []


isLitAlt (PLit _, _)            = True
isLitAlt _                      = False

isLambda (ELam _ _)             = True
isLambda _                      = False


arity (ELam te e)               = length te
arity e                         = 0

eLet [] [] e                    = e
eLet te eq e                    = ELet (Binds False te eq) e

eLet' [] e                      = e
eLet' (bs:bss) e                = ELet bs (eLet' bss e)

cLet' [] c                      = c
cLet' (bs:bss) c                = CLet bs (cLet' bss c)

eLam [] e                       = e
eLam te e                       = ELam te e

eAbs (ELam te e)                = (te,e)
eAbs e                          = ([],e)

eAp e []                        = e
eAp e es                        = EAp e es

eAp1 e1 e2                      = EAp e1 [e2]

eAp2 (EAp e es) es'             = eAp2 e (es++es')
eAp2 e es                       = EAp e es

eFlat (EAp e es)                = (e, es)
eFlat e                         = (e,[])

eHead (EAp e e')                = eHead e
eHead e                         = e

nAp n f es                      = f es1 : es2
  where (es1,es2)               = splitAt n es

tAp t ts                        = foldl TAp t ts

tAp' i vs                       = tAp (TId i) (map TId vs)

tFun [] rh                      = rh
tFun scs rh                     = F scs rh

tFun' scs t                     = tFun scs (R t)

tFlat t                         = flat t []
  where flat (TAp t t') ts      = flat t (t':ts)
        flat t ts               = (t,ts)


tHead (TAp t t')                = tHead t
tHead t                         = t

tId (TId i)                     = i

isTVar t                        = case tHead t of
                                    TVar _ -> True
                                    _      -> False


a `sub` b                       = TFun [a] b


isSub' p                        = isSub (body p)


isSub (TFun [l] u)              = True
isSub _                         = False

isClass' p                      = isClass (body p)

isClass c                       = not (isSub c)


subs (TFun [l] u)               = (l,u)
subs t                          = error ("Internal: subs of " ++ show t)

subsyms p                       = (tId (tHead t), tId (tHead t'))
  where (t,t')                  = subs (body p)

lowersym p                      = fst (subsyms p)
uppersym p                      = snd (subsyms p)

headsym                         = tId . tHead . body


body (Scheme (R c) _ _)         = c

ctxt (Scheme _ ps _)            = ps

quant (Scheme _ _ ke)           = ke

scheme t                        = Scheme (R t) [] []
scheme' t                       = Scheme t [] []
pscheme p                       = Scheme (R p) [] []
pscheme' p ps ke                = Scheme (R p) ps ke


splitInsts (Binds r pe eq)      = (Binds False pe1 eq1, Binds r pe2 eq2)
  where (pe1,pe2)               = partition (isSub' . snd) pe
        (eq1,eq2)               = partition ((`elem` dom pe1) . fst) eq


ksigsOf (Types ke ds)           = ke

tdefsOf (Types ke ds)           = ds

tsigsOf (Binds _ te es)         = te

tsigsOf'                        = foldr (\bs -> ((tsigsOf bs) ++)) []

eqnsOf (Binds _ te es)          = es

isRec (Binds r _ _)             = r

catDecls ds1 ds2                = Types (ksigsOf ds1 ++ ksigsOf ds2) (tdefsOf ds1 ++ tdefsOf ds2)

catBinds bs1 bs2                = Binds (isRec bs1 || isRec bs2) (tsigsOf bs1 ++ tsigsOf bs2) (eqnsOf bs1 ++ eqnsOf bs2)

concatBinds                     = foldr catBinds nullBinds

nullDecls                       = Types [] []

nullBinds                       = Binds True [] []

clash (Binds r te es)           = not r && dom es `overlaps` evars es


altPats                         = map fst
altRhss                         = map snd

oplus eqs eqs'                  = filter ((`notElem` vs) . fst) eqs ++ eqs'
  where vs                      = dom eqs'


-- One-way matching -----------------------------------------------------
-- Only apply when types are known to match!! ---------------------------


matchTs []                              = nullSubst
matchTs ((t,TVar n):eqs)                = matchTs (subst s eqs) @@ s
  where s                               = n +-> t
matchTs ((TAp t u,TAp t' u'):eqs)       = matchTs ((t,t'):(u,u'):eqs)
matchTs ((TId c,TId c'):eqs)            = matchTs eqs
matchTs ((TFun ts t,TFun ts' t'):eqs)   = matchTs ((t,t') : ts `zip` ts' ++ eqs)
matchTs _                               = error "Internal: Core.match"


-- Equality up to renaming ----------------------------------------------

equalTs []                              = True
equalTs ((TVar n, t@(TVar n')):eqs)
  | n == n'                             = equalTs eqs
  | otherwise                           = equalTs (subst (n +-> t) eqs)
equalTs ((TAp t u, TAp t' u'):eqs)      = equalTs ((t,t'):(u,u'):eqs)
equalTs ((TId c,TId c'):eqs)
  | c == c'                             = equalTs eqs
equalTs ((TFun ts t,TFun ts' t'):eqs)
  | length ts == length ts'             = equalTs ((t,t') : ts `zip` ts' ++ eqs)
equalTs eqs                             = False


-- Simple instantiation check ---------------------------------------------

simpleInst (Scheme r [] _) t            = case mkT r of Just t' -> instT [(t',t)]; Nothing -> False
  where instT []                        = True
        instT ((TFun ts1 t1, TFun ts2 t2):ts)
          | length ts1 == length ts2    = instT ((t1,t2):(ts1 `zip` ts2)++ts)
        instT ((TAp t1 u1, TAp t2 u2):ts)
                                        = instT ((t1,t2):(u1,u2):ts)
        instT ((TId n1, TId n2):ts)
          | n1 == n2                    = instT ts
        instT ((TId n, t):ts)
          | isVar n                     = instT (subst (n +-> t) ts)
        instT ts                        = False
        
        mkT (R t)                       = Just t
        mkT (F ss r)                    = do ts <- mapM mkT' ss; t <- mkT r; return (TFun ts t)
        mkT' (Scheme r [] [])           = mkT r
        mkT' _                          = Nothing
simpleInst s t                          = False
                
                
                
-- Free variables (Exp) -------------------------------------------------------------

instance Ids Binds where
    idents (Binds rec te eqns)
      | rec                     = idents eqns \\ dom eqns
      | otherwise               = idents eqns

instance Ids Exp where
    idents (EVar v)             = [v]
    idents (ESel e l)           = idents e
    idents (ELam te e)          = idents e \\ dom te
    idents (EAp e e')           = idents e ++ idents e'
    idents (ELet bs e)          = idents bs ++ (idents e \\ bvars bs)
    idents (ECase e alts def)   = idents e ++ idents alts ++ idents def
    idents (ERec c eqs)         = idents eqs
    idents (EAct e e')          = idents e ++ idents e'
    idents (EReq e e')          = idents e ++ idents e'
    idents (ETempl x t te c)    = idents c \\ (x : dom te)
    idents (EDo x t c)          = filter (not . isState) (idents c \\ [x])
    idents _                    = []

instance Ids Alt where
    idents (p,e)                = idents e


instance Ids Cmd where
    idents (CLet bs c)          = idents bs ++ (idents c \\ bvars bs)
    idents (CGen x t e c)       = idents e ++ (idents c \\ [x])
    idents (CAss x e c)         = idents e ++ idents c
    idents (CRet e)             = idents e
    idents (CExp e)             = idents e


-- Substitutions (Exp) --------------------------------------------------------------

-- Note! This substitution algorithm does not alpha convert!
-- Only use when variables are known not to clash

instance Subst Binds Name Exp where
    subst s (Binds r te eqns)   = Binds r te (subst s eqns)
    
instance Subst Exp Name Exp where
    subst [] e                  = e
    subst s (EVar v)            = case lookup v s of
                                      Just e  -> e
                                      Nothing -> EVar v
    subst s (ESel e l)          = ESel (subst s e) l
    subst s (ELam te e)         = ELam te (subst s e)
    subst s (EAp e e')          = EAp (subst s e) (subst s e')
    subst s (ELet bs e)         = ELet (subst s bs) (subst s e)
    subst s (ECase e alts def)  = ECase (subst s e) (subst s alts) (subst s def)
    subst s (ERec c eqs)        = ERec c (subst s eqs)
    subst s (EAct e e')         = EAct (subst s e) (subst s e')
    subst s (EReq e e')         = EReq (subst s e) (subst s e')
    subst s (ETempl x t te c)   = ETempl x t te (subst s c)
    subst s (EDo x t c)         = EDo x t (subst s c)
    subst s e                   = e

instance Subst Cmd Name Exp where
    subst s (CLet bs c)         = CLet (subst s bs) (subst s c)
    subst s (CAss x e c)        = CAss x (subst s e) (subst s c)
    subst s (CGen x t e c)      = CGen x t (subst s e) (subst s c)
    subst s (CRet e)            = CRet (subst s e)
    subst s (CExp e)            = CExp (subst s e)

instance Subst Exp a b => Subst Alt a b where
    subst s (p,rh)              = (p, subst s rh)



instance Subst Binds TVar Type where
    subst s (Binds r te eqns)   = Binds r (subst s te) (subst s eqns)

instance Subst Exp TVar Type where
    subst [] e                  = e
    subst s (ESel e l)          = ESel (subst s e) l
    subst s (ELam te e)         = ELam (subst s te) (subst s e)
    subst s (EAp e e')          = EAp (subst s e) (subst s e')
    subst s (ELet bs e)         = ELet (subst s bs) (subst s e)
    subst s (ECase e alts def)  = ECase (subst s e) (subst s alts) (subst s def)
    subst s (ERec c eqs)        = ERec c (subst s eqs)
    subst s (EAct e e')         = EAct (subst s e) (subst s e')
    subst s (EReq e e')         = EReq (subst s e) (subst s e')
    subst s (ETempl x t te c)   = ETempl x (subst s t) (subst s te) (subst s c)
    subst s (EDo x t c)         = EDo x (subst s t) (subst s c)
    subst s e                   = e

instance Subst Cmd TVar Type where
    subst s (CLet bs c)         = CLet (subst s bs) (subst s c)
    subst s (CAss x e c)        = CAss x (subst s e) (subst s c)
    subst s (CGen x t e c)      = CGen x (subst s t) (subst s e) (subst s c)
    subst s (CRet e)            = CRet (subst s e)
    subst s (CExp e)            = CExp (subst s e)



instance Subst Binds Name Type where
    subst s (Binds r te eqns)   = Binds r (subst s te) (subst s eqns)

instance Subst Exp Name Type where
    subst [] e                  = e
    subst s (ESel e l)          = ESel (subst s e) l
    subst s (ELam te e)         = ELam (subst s te) (subst s e)
    subst s (EAp e e')          = EAp (subst s e) (subst s e')
    subst s (ELet bs e)         = ELet (subst s bs) (subst s e)
    subst s (ECase e alts def)  = ECase (subst s e) (subst s alts) (subst s def)
    subst s (ERec c eqs)        = ERec c (subst s eqs)
    subst s (EAct e e')         = EAct (subst s e) (subst s e')
    subst s (EReq e e')         = EReq (subst s e) (subst s e')
    subst s (ETempl x t te c)   = ETempl x (subst s t) (subst s te) (subst s c)
    subst s (EDo x t c)         = EDo x (subst s t) (subst s c)
    subst s e                   = e

instance Subst Cmd Name Type where
    subst s (CLet bs c)         = CLet (subst s bs) (subst s c)
    subst s (CAss x e c)        = CAss x (subst s e) (subst s c)
    subst s (CGen x t e c)      = CGen x (subst s t) (subst s e) (subst s c)
    subst s (CRet e)            = CRet (subst s e)
    subst s (CExp e)            = CExp (subst s e)


-- Type identifiers ---------------------------------------------------------

instance Ids Decl where
    idents (DData _ bs cs)      = idents bs ++ idents cs
    idents (DRec _ _ bs ss)     = idents bs ++ idents ss
    idents (DType _ t)          = idents t

instance Ids Constr where
    idents (Constr ts ps ke)    = idents ts ++ idents ps

instance Ids Type where
    idents (TId c)              = [c]
    idents (TVar _)             = []
    idents (TAp t t')           = idents t ++ idents t'
    idents (TFun ts t)          = idents ts ++ idents t

instance Ids Scheme where
    idents (Scheme rh ps ke)    = (idents rh ++ idents ps) \\ dom ke
    
instance Ids Rho where
    idents (R t)                = idents t
    idents (F scs rh)           = idents scs ++ idents rh

instance Subst Type Name Type where
    subst s (TId c)             = case lookup c s of
                                    Just t -> t
                                    Nothing -> TId c
    subst s (TVar n)            = TVar n
    subst s (TAp t t')          = TAp (subst s t) (subst s t')
    subst s (TFun ts t)         = TFun (subst s ts) (subst s t)

instance Subst Scheme Name Type where
    subst s (Scheme rh ps ke)   = Scheme (subst s rh) (subst s ps) ke

instance Subst Rho Name Type where
    subst s (R t)               = R (subst s t)
    subst s (F scs rh)          = F (subst s scs) (subst s rh)

instance Subst (Type,Type) Name Type where
    subst s (t1,t2)             = (subst s t1, subst s t2)
    
instance Subst (Scheme,Scheme) Name Type where
    subst s (s1,s2)             = (subst s s1, subst s s2)
                

-- Type variables --------------------------------------------------------------

instance TVars Type where
    tvars (TId c)               = []
    tvars (TVar n)              = [n]
    tvars (TAp t t')            = tvars t ++ tvars t'
    tvars (TFun ts t)           = tvars ts ++ tvars t

instance TVars Scheme where
    tvars (Scheme t ps ke)      = tvars t ++ tvars ps

instance TVars Rho where
    tvars (R t)                 = tvars t
    tvars (F scs rh)            = tvars scs ++ tvars rh

instance Subst Type TVar Type where
    subst [] t                  = t
    subst s (TId c)             = TId c
    subst s (TVar n)            = case lookup n s of
                                    Just t -> t
                                    Nothing -> TVar n
    subst s (TAp t t')          = TAp (subst s t) (subst s t')
    subst s (TFun ts t)         = TFun (subst s ts) (subst s t)

instance Subst Scheme TVar Type where
    subst [] sc                 = sc
    subst s sc@(Scheme t ps ke) = Scheme (subst s t) (subst s ps) ke
  

instance Subst Rho TVar Type where
    subst s (R t)               = R (subst s t)
    subst s (F scs rh)          = F (subst s scs) (subst s rh)

instance Subst (Type,Type) TVar Type where
    subst s (t,t')              = (subst s t, subst s t')


-- Kind variables ----------------------------------------------------------------

instance Subst Decl Int Kind where
    subst s (DData vs bs cs)    = DData vs (subst s bs) (subst s cs)
    subst s (DRec i vs bs ss)   = DRec i vs (subst s bs) (subst s ss)
    subst s (DType vs t)        = DType vs (subst s t)

instance Subst Constr Int Kind where
    subst s (Constr ts ps ke)   = Constr (subst s ts) (subst s ps) (subst s ke)
  
instance Subst Scheme Int Kind where
    subst s (Scheme rh ps ke)  = Scheme (subst s rh) (subst s ps) (subst s ke)

instance Subst Rho Int Kind where
    subst s (R t)               = R (subst s t)
    subst s (F scs rh)          = F (subst s scs) (subst s rh)

instance Subst Type Int Kind where
    subst s (TAp t t')          = TAp (subst s t) (subst s t')
    subst s (TFun ts t)         = TFun (subst s ts) (subst s t)
    subst s (TId c)             = TId c
    subst s (TVar (TV (n,k)))   = TVar (TV (n, subst s k))
    

-- Bound variables --------------------------------------------------------------

instance BVars Binds where
    bvars (Binds r te eqns)     = dom eqns

                 
                    
-- Printing ==================================================================

-- Modules -------------------------------------------------------------------

instance Pr Module where
    pr (Module i ns xs ds is bs)  = text "module" <+> prId i <+> text "where"
                                  $$ prImports ns $$ prDefault xs $$ pr ds $$ prInsts is $$ pr bs

prDefault []                     = empty
prDefault ((_,i1,i2) : is)       = text "default" <+> prNode i1 <+> text "<" <+> prNode i2 $$ prDefault is

prNode (Nothing,t)               = pr t
prNode (Just v,t)                = prId v <+> text "::" <+> pr t

prImports []                     = empty
prImports ns                     = text "import" <+> hpr ',' ns

instance Pr (Module,a) where
  pr (m,_)                       = pr m

-- Type declarations ---------------------------------------------------------

instance Pr Types where
    pr (Types ke ds)            = vpr ke $$ vpr ds

instance Pr (Name, Decl) where
    pr (i, DType vs t)          = text "type" <+> prId i <+> hsep (map prId vs) 
                                  <+> text "=" <+> pr t
    pr (i, DData vs ts cs)      = text "data" <+> prId i <+> hsep (map prId vs) 
                                  <+> prSubs ts <+> prConstrs cs
    pr (i, DRec isC vs ts ss)   = text kwd <+> prId i <+> hsep (map prId vs) 
                                  <+> prSups ts <+> prEq ss $$ nest 4 (vpr ss)
      where kwd                 = if isC then "class" else "record"

prEq []                         = empty
prEq _                          = text "="


-- Instances ---------------------------------------------------------------

prInsts (Binds r te eqs)        = vcat (map prInst te) $$ vpr eqs

prInst (i, p)                   = text "instance" <+> prId i <+> text "::" <+> prPScheme 0 p


-- Bindings -----------------------------------------------------------------

instance Pr Binds where
    pr (Binds _ te eqns)        = vpr te $$ vpr eqns
    
instance Pr (Name, Scheme) where
    pr (v, sc)                  = prId v <+> text "::" <+> pr sc
                                  
instance Pr (Name, Exp) where
    pr (v, e)                   = prId v <+> text "=" <+> pr e

        
-- Sub/supertypes -----------------------------------------------------------

prSups []                       = empty
prSups ts                       = char '<' <+> hpr ',' ts

prSubs []                       = empty
prSubs ts                       = char '>' <+> hpr ',' ts

    
-- Constructors ------------------------------------------------------------

prConstrs []                    = empty
prConstrs (c:cs)                = vcat (char '=' <+> pr c : map ((char '|' <+>) . pr) cs)

instance Pr (Name,Constr) where
    pr (i, Constr ts ps ke)     = prId i <+> hsep (map (prn 1) ts) <+> prContext ps ke


-- Predicates --------------------------------------------------------------

prContext [] []                 = empty
prContext ps ke                 = text "\\\\" <+> preds
  where preds                   = hsep (punctuate comma (map (prPScheme 1) ps ++ map prKind ke))

prKind (n,Star)                 = prId n
prKind (n,k)                    = prId n <+> text "::" <+> pr k

prPScheme 0 (Scheme p ps ke)    = prRPred p <+> prContext ps ke
prPScheme 1 (Scheme p [] [])    = prRPred p
prPScheme 1 sc                  = parens (prPScheme 0 sc)

prRPred (F [t1] t2)             = prn 1 t1 <+> text "<" <+> prn 1 t2
prRPred (R t)                   = prPred t

prPred (TFun [t1] t2)           = prn 1 t1 <+> text "<" <+> prn 1 t2
prPred t                        = pr t


-- Types -----------------------------------------------------------------

instance Pr Scheme where
    prn 0 (Scheme rh ps ke)     = prn 0 rh <+> prContext ps ke
    prn 1 (Scheme rh [] [])     = prn 1 rh
    prn n sc                    = parens (prn 0 sc)

instance Pr Rho where
    prn 0 (F (sc:scs) rh)       = prn 1 sc <+> text "->" <+> prn 0 (F scs rh)
    prn 0 (F [] rh)             = prn 1 rh
    prn 0 rh                    = prn 1 rh
    prn 1 (R t)                 = prn 1 t
    prn 1 rh                    = parens (prn 0 rh)
    
instance Pr Type where
    prn 0 (TFun (t:ts) t')      = prn 1 t <+> text "->" <+> prn 0 (TFun ts t')
    prn 0 (TFun [] t)           = prn 1 t
    prn 0 t                     = prn 1 t
    
    prn 1 (TAp t t')            = prn 1 t <+> prn 2 t'
    prn 1 t                     = prn 2 t

    prn 2 (TId c)               = prId c
    prn 2 (TVar _)              = text "_"
--    prn 2 (TVar n)              = text "_" <> pr n

    prn 2 t                     = parens (prn 0 t)


prMaybeScheme Nothing           = empty
prMaybeScheme (Just t)          = prn 1 t


-- Patterns ----------------------------------------------------------------

instance Pr Pat where
    pr (PCon k)                 = prId k
    pr (PLit l)                 = pr l
    

-- Expressions -------------------------------------------------------------

instance Pr Exp where
--    prn 0 (ELam te e)           = hang (char '\\' <> sep (map (parens . pr) te) <+> text "->") 4 (pr e)
    prn 0 (ELam te e)           = hang (char '\\' <> sep (map prId (dom te)) <+> text "->") 4 (pr e)
    prn 0 (ELet bs e)           = text "let" $$ nest 4 (pr bs) $$ text "in" <+> pr e
    prn 0 (ECase e alts def)    = text "case" <+> pr e <+> text "of" $$ 
                                       nest 2 (vpr alts $$ text "_ ->" <+> pr def)
    prn 0 (EAct e e')           = text "action@" <> prn 2 e $$
                                       nest 4 (pr e')
    prn 0 (EReq e e')           = text "request@" <> prn 2 e $$
                                       nest 4 (pr e')
    prn 0 (ETempl x t te c)     = text "template@" <> prId x $$
                                       nest 4 (vpr te) $$
                                       nest 4 (pr c)
    prn 0 (EDo x t c)           = text "do@" <> prId x $$
                                       nest 4 (pr c)
    prn 0 e                     = prn 1 e

    prn 1 (EAp e es)            = hang (prn 1 e) 2 (sep (map (prn 2) es))

    prn 1 e                     = prn 2 e
        
    prn 2 (ECon c)              = prId c
    prn 2 (ESel e s)            = prn 2 e <> text "." <> prId s
    prn 2 (EVar v)              = prId v
    prn 2 (ELit l)              = pr l
    prn 2 (ERec c eqs)          = prId c <+> text "{" <+> hpr ',' eqs <+> text "}"
    prn 2 e                     = parens (prn 0 e)

instance Pr Alt where
    pr (p, e)                   = pr p <+> text "->" $$ nest 4 (pr e)


instance Pr Cmd where
    pr (CLet bs c)              = pr bs $$
                                  pr c
    pr (CAss x e c)             = prId x <+> text ":=" <+> pr e $$
                                  pr c
    pr (CGen x t e c)           = prId x <+> {- text "::" <+> pr t <+> -} text "<-" <+> pr e $$
                                  pr c
    pr (CRet e)                 = text "return" <+> pr e
    pr (CExp e)                 = pr e

-- Binary --------------------------------------------------

instance Binary Module where
  put (Module a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get>>= \f -> return (Module a b c d e f)

instance Binary Types where
  put (Types a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (Types a b)

instance Binary Binds where
  put (Binds a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Binds a b c)

instance Binary Decl where
  put (DData a b c) = putWord8 0 >> put a >> put b >> put c
  put (DRec a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  put (DType a b) = putWord8 2 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (DData a b c)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (DRec a b c d)
      2 -> get >>= \a -> get >>= \b -> return (DType a b)
      _ -> fail "no parse"

instance Binary Scheme where
  put (Scheme a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Scheme a b c)

instance Binary Constr where
  put (Constr a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Constr a b c)

instance Binary Rho where
  put (R a) = putWord8 0 >> put a
  put (F a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (R a)
      1 -> get >>= \a -> get >>= \b -> return (F a b)
      _ -> fail "no parse"

instance Binary Type where
  put (TId a) = putWord8 0 >> put a
  put (TVar a) = putWord8 1 >> put a
  put (TFun a b) = putWord8 2 >> put a >> put b
  put (TAp a b) = putWord8 3 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (TId a)
      1 -> get >>= \a -> return (TVar a)
      2 -> get >>= \a -> get >>= \b -> return (TFun a b)
      3 -> get >>= \a -> get >>= \b -> return (TAp a b)
      _ -> fail "no parse"

instance Binary Pat where
  put (PCon a) = putWord8 0 >> put a
  put (PLit a) = putWord8 1 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (PCon a)
      1 -> get >>= \a -> return (PLit a)
      _ -> fail "no parse"

instance Binary Exp where
  put (ECon a) = putWord8 0 >> put a
  put (ESel a b) = putWord8 1 >> put a >> put b
  put (EVar a) = putWord8 2 >> put a
  put (ELam a b) = putWord8 3 >> put a >> put b
  put (EAp a b) = putWord8 4 >> put a >> put b
  put (ELet a b) = putWord8 5 >> put a >> put b
  put (ECase a b c) = putWord8 6 >> put a >> put b >> put c
  put (ERec a b) = putWord8 7 >> put a >> put b
  put (ELit a) = putWord8 8 >> put a
  put (EAct a b) = putWord8 9 >> put a >> put b
  put (EReq a b) = putWord8 10 >> put a >> put b
  put (ETempl a b c d) = putWord8 11 >> put a >> put b >> put c >> put d
  put (EDo a b c) = putWord8 12 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ECon a)
      1 -> get >>= \a -> get >>= \b -> return (ESel a b)
      2 -> get >>= \a -> return (EVar a)
      3 -> get >>= \a -> get >>= \b -> return (ELam a b)
      4 -> get >>= \a -> get >>= \b -> return (EAp a b)
      5 -> get >>= \a -> get >>= \b -> return (ELet a b)
      6 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ECase a b c)
      7 -> get >>= \a -> get >>= \b -> return (ERec a b)
      8 -> get >>= \a -> return (ELit a)
      9 -> get >>= \a -> get >>= \b -> return (EAct a b)
      10 -> get >>= \a -> get >>= \b -> return (EReq a b)
      11 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (ETempl a b c d)
      12 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EDo a b c)
      _ -> fail "no parse"

instance Binary Cmd where
  put (CGen a b c d) = putWord8 0 >> put a >> put b >> put c >> put d
  put (CAss a b c) = putWord8 1 >> put a >> put b >> put c
  put (CLet a b) = putWord8 2 >> put a >> put b
  put (CRet a) = putWord8 3 >> put a
  put (CExp a) = putWord8 4 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (CGen a b c d)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (CAss a b c)
      2 -> get >>= \a -> get >>= \b -> return (CLet a b)
      3 -> get >>= \a -> return (CRet a)
      4 -> get >>= \a -> return (CExp a)
      _ -> fail "no parse"
