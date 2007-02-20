module Syntax where

import Common
import Lexer
import PP


data Module = Module  Name [Decl]
            deriving  (Eq,Show)
            
data Decl   = DKSig   Name Kind
            | DData   Name [Name] [Type] [Constr]
            | DRec    Bool Name [Name] [Type] [Sig]
            | DType   Name [Name] Type
            | DInst   Type [Bind]  -- removed by desugaring
            | DPSig   Name Type
            | DBind   Bind
            deriving  (Eq,Show)

data Constr =  Constr Name [Type] [Pred]
            deriving (Eq,Show)

data Sig    = Sig [Name] Type
            deriving (Eq,Show)

data Bind   = BSig    [Name] Type
            | BEqn    Lhs (Rhs Exp)
            deriving  (Eq,Show)

type Eqn    = (Lhs, Rhs Exp)

data Type   = TQual   Type [Pred]
            | TCon    Name
            | TVar    Name
            | TAp     Type Type
            | TSub    Type Type
            | TWild
            | TList   Type
            | TTup    [Type]
            | TFun    [Type] Type
            deriving  (Eq,Show)

data Pred   = PType Type
            | PKind   Name Kind
            deriving (Eq,Show)
            
data Lhs    = LFun Name [Pat]
            | LPat Pat
            deriving (Eq,Show)

type Pat    = Exp

data Exp    = EVar    Name
            | EAp     Exp Exp
            | ECon    Name
            | ESel    Name
            | ELit    Lit
            | ETup    [Exp]
            | EList   [Exp]
            | EWild
            | ESig    Exp Type
            | ERec    (Maybe (Name,Bool)) [Field]
            -- pattern syntax ends here
            | ELam    [Pat] Exp
            | ELet    [Bind] Exp
            | ECase   Exp [Alt Exp]
-- the following and ETup, EList removed in desugaring
            | EIf     Exp Exp Exp
            | ENeg    Exp
            | ESeq    Exp (Maybe Exp) Exp
            | EComp   Exp [Qual]
            | ESectR  Exp Name
            | ESectL  Name Exp
            | ESelect Exp Name
            | EDo     (Maybe Name) (Maybe Type) [Stmt] 
            | ETempl  (Maybe Name) (Maybe Type) [Stmt]
            | EAct    (Maybe Name) [Stmt] 
            | EReq    (Maybe Name) [Stmt] 
            | EAfter  Exp Exp
            | EBefore Exp Exp
            deriving  (Eq,Show)

data Field  = Field   Name Exp
            deriving (Eq,Show)

data Rhs a  = RExp    a
            | RGrd    [GExp a]
            | RWhere  (Rhs a) [Bind]
            deriving  (Eq,Show)

data GExp a = GExp    [Qual] a
            deriving (Eq,Show)
            
data Alt a  = Alt     Pat (Rhs a)
            deriving  (Eq,Show)

data Qual   = QExp    Exp
            | QGen    Pat Exp
            | QLet    [Bind]
            deriving  (Eq,Show)
            
data Stmt   = SExp    Exp
            | SRet    Exp
            | SGen    Pat Exp
            | SBind   Bind
            | SAss    Pat Exp
            | SForall [Qual] [Stmt]
            | SWhile  Exp [Stmt]
            | SIf     Exp [Stmt]
            | SElsif  Exp [Stmt]
            | SElse   [Stmt]
            | SCase   Exp [Alt [Stmt]]
            deriving  (Eq,Show)


-- Helper functions ----------------------------------------------------------------


newEVar v                       = do i <- newName v
                                     return (EVar i)

op2exp c                        = if isCon c then ECon c else EVar c

isEVar (EVar _)                 = True
isEVar _                        = False

isEConApp (EAp e es)            = isEConApp e
isEConApp (ECon _)              = True
isEConApp _                     = False

isELit (ELit _)                 = True
isELit _                        = False

isERec (ERec _ _)               = True
isERec _                        = False

isESigVar (ESig (EVar _) _)     = True
isESigVar (EVar _)              = True
isESigVar _                     = False

isETup (ETup _)                 = True
isETup _                        = False

isSAss (SAss _ _)               = True
isSAss _                        = False

isSGen (SGen _ _)               = True
isSGen _                        = False

isSBind (SBind _)               = True
isSBind _                       = False

isDBind (DBind _)               = True
isDBind _                       = False

isBSig (BSig _ _)               = True
isBSig _                        = False

isPKind (PKind _ _)             = True
isPKind _                       = False


tupSize (ETup es)               = length es
tupSize _                       = -1


eFlat e                         = flat e []
  where flat (EAp e e') es      = flat e (e':es)
        flat e es               = (e,es)

eLam [] e                       = e
eLam ps (ELam ps' e)            = ELam (ps++ps') e
eLam ps e                       = ELam ps e

rAp (RExp e) es                 = RExp (foldl EAp e es)
rAp (RWhere rhs bs) es          = RWhere (rAp rhs es) bs
rAp (RGrd gs) es                = RGrd [ GExp qs (foldl EAp e es) | GExp qs e <- gs ]


eLet [] e                       = e
eLet bs e                       = ELet bs e

simpleEqn x e                   = BEqn (LFun x []) (RExp e)

exp2lhs e                       = case eFlat e of
                                       (EVar v, ps) -> LFun v ps
                                       _            -> LPat e

tFun [] t                       = t
tFun ts t                       = TFun ts t

tQual ps (TQual t ps')          = TQual t (ps++ps')
tQual ps t                      = TQual t ps

tFlat t                         = flat t []
  where flat (TAp t1 t2) ts     = flat t1 (t2:ts)
        flat t ts               = (t,ts)


type2cons (TQual t ps)          = type2cons t
type2cons t                     = case tFlat t of
                                    (TCon c,ts) -> Constr c ts []
                                    _           -> error "Bad constructor..."

type2head (TQual t ps)          = type2head t
type2head t                     = case tFlat t of
                                    (TCon c,ts) -> c
                                    _           -> error "Bad instance head..."

stripSigs (ESig p _)            = p
stripSigs p                     = p


-- Substitution --------------------------------------------------------------

instance Subst Bind Name Exp where
    subst s (BSig xs t)         = BSig xs t
    subst s (BEqn lhs rhs)      = BEqn lhs (subst s rhs)


instance Subst Exp Name Exp where
    subst s (EVar x)            = case lookup x s of
                                    Just e  -> e
                                    Nothing -> EVar x
    subst s (EAp e e')          = EAp (subst s e) (subst s e')
    subst s (ECon c)            = ECon c
    subst s (ELit l)            = ELit l
    subst s (ETup es)           = ETup (subst s es)
    subst s (EList es)          = EList (subst s es)
    subst s (EWild)             = EWild
    subst s (ESig e t)          = ESig (subst s e) t
    subst s (ELam ps e)         = ELam ps (subst s e)
    subst s (ELet bs e)         = ELet (subst s bs) (subst s e)
    subst s (ECase e alts)      = ECase (subst s e) (subst s alts)
    subst s (ERec m fs)         = ERec m (subst s fs)
    subst s (EIf e1 e2 e3)      = EIf (subst s e1) (subst s e2) (subst s e3)
    subst s (ENeg e)            = ENeg (subst s e)
    subst s (ESeq e1 Nothing e2)  = ESeq (subst s e1) Nothing (subst s e2)
    subst s (ESeq e1 (Just e) e2) = ESeq (subst s e1) (Just (subst s e)) (subst s e2)
    subst s (EComp e qs)        = EComp (subst s e) (subst s qs)
    subst s (ESectR e op)       = ESectR (subst s e) op
    subst s (ESectL op e)       = ESectL op (subst s e)
    subst s (ESelect e l)       = ESelect (subst s e) l
    subst s (ESel l)            = ESel l
    subst s (EDo v t st)        = EDo v t (subst s st)
    subst s (ETempl v t st)     = ETempl v t (subst s st) 
    subst s (EAct v st)         = EAct v (subst s st)
    subst s (EReq v st)         = EReq v (subst s st)
    subst s (EAfter e e')       = EAfter (subst s e) (subst s e')
    subst s (EBefore e e')      = EBefore (subst s e) (subst s e')


instance Subst Field Name Exp where
    subst s (Field l e)         = Field l (subst s e)


instance Subst a Name Exp => Subst (Rhs a) Name Exp where
    subst s (RExp e)            = RExp (subst s e)
    subst s (RGrd gs)           = RGrd (subst s gs)
    subst s (RWhere rhs bs)     = RWhere (subst s rhs) (subst s bs)


instance Subst a Name Exp => Subst (GExp a) Name Exp where
    subst s (GExp qs e)         = GExp (subst s qs) (subst s e)

            
instance Subst a Name Exp => Subst (Alt a) Name Exp where
    subst s (Alt p rhs)         = Alt p (subst s rhs)


instance Subst Qual Name Exp where
    subst s (QExp e)            = QExp (subst s e)
    subst s (QGen p e)          = QGen p (subst s e)
    subst s (QLet bs)           = QLet (subst s bs)


            
instance Subst Stmt Name Exp where
    subst s (SExp e)            = SExp (subst s e)
    subst s (SRet e)            = SRet (subst s e)
    subst s (SGen p e)          = SGen p (subst s e)
    subst s (SBind b)           = SBind (subst s b)
    subst s (SAss p e)          = SAss p (subst s e)
    subst s (SForall qs st)     = SForall (subst s qs) (subst s st)
    subst s (SWhile e st)       = SWhile (subst s e) (subst s st)
    subst s (SIf e st)          = SIf (subst s e) (subst s st)
    subst s (SElsif e st)       = SElsif (subst s e) (subst s st)
    subst s (SElse st)          = SElse (subst s st)
    subst s (SCase e alts)      = SCase (subst s e) (subst s alts)





-- Printing ==================================================================

-- Modules -------------------------------------------------------------------

instance Pr Module where
    pr (Module c ds)            = text "module" <+> prId c <+> text "where"
                                  $$ vpr ds

-- Declarations --------------------------------------------------------------

instance Pr Decl where
    pr (DKSig c k)              = prId c <+> text "::" <+> pr k
    pr (DType c vs t)           = text "type" <+> prId c <+> hsep (map prId vs) 
                                  <+> equals <+> pr t
    pr (DData c vs subs cs)     = text "data" <+> prId c <+> hsep (map prId vs) 
                                  <+> prSubs subs <+> prCons cs
    pr (DRec isC c vs sups ss)  = text kwd <+> prId c <+> hsep (map prId vs) 
                                  <+> prSups sups <+> prEq ss $$ prSigs ss
      where kwd                 = if isC then "class" else "record"
    pr (DInst t bs)             = text "instance" <+> pr t <+> text "=" $$ nest 4 (vpr bs)
    pr (DPSig v t)              = text "instance" <+> prId v <+> text "::" <+> pr t
    pr (DBind b)                = pr b

prPreds []                      = empty
prPreds ps                      = text " \\\\" <+> hpr ',' ps

prEq []                         = empty
prEq bs                         = text "="

prSigs ss                       = nest 4 (vpr ss)

-- Sub/supertypes -----------------------------------------------------------

prSups []                       = empty
prSups ts                       = char '<' <+> hpr ',' ts

prSubs []                       = empty
prSubs ts                       = char '>' <+> hpr ',' ts
    

-- Constructors and contexts -----------------------------------------------


instance Pr Constr where
  pr (Constr c ts ps)           = prId c <+> hsep (map (prn 3) ts) <> prPreds ps

prCons []                       = empty
prCons (c:cs)                   = vcat (char '=' <+> pr c : map ((char '|' <+>) . pr) cs)



-- Signatures ------------------------------------------------------------

instance Pr Sig where
    pr (Sig vs qt)              = hcat (punctuate comma (map prId vs)) <+> text "::" <+> pr qt
    
    
-- Predicates ------------------------------------------------------------

instance Pr Pred where
    pr (PType t)                = pr t
    pr (PKind v k)              = prId v <+> text "::" <+> pr k

-- Left hand sides ------------------------------------------------------

instance Pr Lhs where
   pr (LFun v ps)               = prId v <+> hsep (map (prn 13) ps) 
   pr (LPat p)                  = pr p

{-
-- Patterns --------------------------------------------------------------

instance Pr Pat where

    prn 0 (PCon c ps)           = prId c <+> hsep (map (prn 13) ps) 
    prn 0 p                     = prn 13 p

    prn 13 (PVar v)             = prId v
    prn 13 (PLit l)             = pr l
    prn 13 (PTup ps)            = parens (hpr ',' ps)
    prn 13 (PList ps)           = brackets (hpr ',' ps)
    prn 13 p                    = parens (pr p)

    prn n p                     = prn 13 p
-}

-- Types -----------------------------------------------------------------

instance Pr Type where
    prn 0 (TQual t ps)          = prn 1 t <> prPreds ps
    prn 0 t                     = prn 1 t

    prn 1 (TFun ts t)           = prArgs ts <+> prn 2 t
      where prArgs []           = empty
            prArgs (t:ts)       = prn 2 t <+> text "->" <+> prArgs ts
    prn 1 (TSub t t')           = prn 2 t <+> text "<" <+> prn 2 t'
    prn 1 t                     = prn 2 t

    prn 2 (TAp t t')            = prn 2 t <+> prn 3 t'
    prn 2 t                     = prn 3 t
        
    prn 3 (TCon c)              = prId c
    prn 3 (TVar v)              = prId v
    prn 3 (TWild)               = text "_"
    prn 3 (TList t)             = brackets (pr t)
    prn 3 (TTup ts)             = parens (hpr ',' ts)
    prn 3 t                     = parens (prn 0 t)

-- Bindings ----------------------------------------------------------------

instance Pr Bind where
    pr (BSig vs qt)             = hcat (punctuate comma (map prId vs)) <+> text "::" <+> pr qt
    pr (BEqn p (RExp e))        = pr p <+> equals <+> pr e
    pr (BEqn p rhs)             = pr p $$ nest 2 (prRhs equals rhs)

prRhs eq (RExp e)               = eq <+> pr e
prRhs eq (RGrd gs)              = vcat (map (prGuard eq) gs)
prRhs eq (RWhere rhs bs)        = prRhs eq rhs $$ text "where" $$ nest 4 (vpr bs)

prGuard eq (GExp qs e)          = char '|' <+> hpr ',' qs <+> eq <+> pr e


-- Expressions -------------------------------------------------------------

instance Pr Exp where
    prn 0 (ELam ps e)           = sep [char '\\' <> hsep (map (prn 13) ps) <+> text "->", pr e]
    prn 0 (ELet bs e)           = text "let" <+> vpr bs $$ text "in" <+> pr e
    prn 0 (EIf e e1 e2)         = sep [text "if" <+> pr e, text "then" <+> pr e1, 
                                       text "else" <+> pr e2]
    prn 0 (ECase e alts)        = text "case" <+> pr e <+> text "of" $$ nest 2 (vpr alts)
    prn 0 (EDo v t ss)          = text "do" <+> vpr ss 
    prn 0 (ETempl v t ss)       = text "template" $$ nest 4 (vpr ss)
    prn 0 (EAct v ss)           = text "action" $$ nest 4 (vpr ss) 
    prn 0 (EReq v ss)           = text "request" $$ nest 4 (vpr ss) 
    prn 0 (EAfter e e')         = text "after" <+> prn 12 e <+> pr e'
    prn 0 (EBefore e e')        = text "before" <+> prn 12 e <+> pr e'
    prn 0 e                     = prn 1 e

    prn 11 (EAp e e')           = prn 11 e <+> prn 12 e'
    prn 11 e                    = prn 12 e
        
    prn 12 (ESelect e l)        = prn 12 e <> text "." <> prId l
    prn 12 e                    = prn 13 e
        
    prn 13 (EVar v)             = prId v
    prn 13 (ECon c)             = prId c
    prn 13 (ESel l)             = parens (text "." <> prId l)
    prn 13 (EWild)              = text "_"
    prn 13 (ELit l)             = pr l
    prn 13 (ERec Nothing fs)    = text "{" <+> hpr ',' fs <+> text "}"
    prn 13 (ERec (Just(c,b)) fs)= prId c <+> text "{" <+> hpr ',' fs <+> (if b then empty else text "..") <+> text "}"
    prn 13 (ENeg e)             = text "-" <+> prn 0 e
    prn 13 (ESig e qt)          = parens (pr e <+> text "::" <+> pr qt)
    prn 13 (ETup es)            = parens (hpr ',' es)
    prn 13 (ESectR e op)        = parens (pr e <> prOp op)
    prn 13 (ESectL op e)        = parens (prOp op <> pr e)
    prn 13 (EList es)           = brackets (hpr ',' es)
    prn 13 (ESeq e Nothing to) 
                                = brackets (pr e <> text ".." <> pr to)
    prn 13 (ESeq e (Just by) to) 
                                = brackets (pr e <> comma <> pr by <> text ".." <> pr to)
    prn 13 (EComp e qs)         = brackets (empty <+> pr e <+> char '|' <+> hpr ',' qs)
    prn 13 e                    = parens (prn 0 e)

    prn n e                     = prn 11 e
    
instance Pr Field where
    pr (Field l e)              = prId l <+> equals <+> pr e
    
instance Pr a => Pr (Alt a) where
    pr (Alt p (RExp e))         = pr p <+> text "->" <+> pr e
    pr (Alt p rhs)              = pr p <+> prRhs (text "->") rhs

instance Pr Qual where
    pr (QExp e)                 = pr e
    pr (QGen p e)               = pr p <+> text "<-" <+> pr e
    pr (QLet bs)                = text "let" <+> hpr ';' bs

-- Statements --------------------------------------------------------------

instance Pr [Stmt] where
    pr ss                       = vpr ss

instance Pr Stmt where
    pr (SExp e)                 = pr e
    pr (SRet e)                 = text "return" <+> pr e
    pr (SGen p e)               = pr p <+> text "<-" <+> pr e
    pr (SBind b)                = pr b
    pr (SAss p e)               = pr p <+> text ":=" <+> pr e
    pr (SIf e ss)               = text "if" <+> pr e <+> text "then" $$ nest 4 (pr ss)
    pr (SElsif e ss)            = text "elsif" <+> pr e <+> text "then" $$ nest 4 (pr ss)
    pr (SElse ss)               = text "else" $$ nest 4 (pr ss)
    pr (SWhile e ss)            = text "while" <+> pr e <+> text "do" $$ nest 4 (pr ss)
    pr (SForall qs ss)          = text "forall" <+> hpr ',' qs <+> text "do" $$ nest 4 (pr ss)
    pr (SCase e alts)           = text "case" <+> pr e <+> text "of" $$ nest 4 (vpr alts)


-- Free pattern variables ------------------------------------------------------------

instance Ids Exp where
    idents (EVar v)             = [v]
    idents (EAp p ps)           = idents p ++ idents ps
    idents (ECon c)             = [c]  --  ???????????
    idents (ELit _)             = []
    idents (ETup ps)            = idents ps
    idents (EList ps)           = idents ps
    idents (EWild)              = []
    idents (ESig p qt)          = idents p
    idents (ERec _  fs)         = concatMap (\(Field s p) -> idents p) fs
    idents _                    = []


pvars p                         = evars p

assignedVars ss                 = concat [ pvars p | SAss p _ <- ss ]


-- Bound variables ----------------------------------------------------------

instance BVars [Bind] where
    bvars []                            = []
    bvars (BEqn (LPat p) _ : bs)        = pvars p ++ bvars bs
    bvars (BEqn (LFun v _) _ : bs)      = bvars' v bs
      where bvars' v (BEqn (LFun v' _) _ : bs)
              | v == v'                 = bvars' v bs
              | otherwise               = v : bvars' v' bs
            bvars' v bs                 = v : bvars bs
    bvars (_ : bs)                      = bvars bs

instance BVars [Field] where
    bvars bs                            = [ s | Field s e <- bs ]

instance BVars [Stmt] where
    bvars []                            = []
    bvars (SGen p e : ss)               = pvars p ++ bvars ss
    bvars (SBind (BEqn (LPat p) _):ss)  = pvars p ++ bvars ss
    bvars (SBind (BEqn (LFun v _)_):ss) = bvars' v ss
      where bvars' v (SBind (BEqn (LFun v' _) _) : ss)
              | v == v'                 = bvars' v ss
              | otherwise               = v : bvars' v' ss
            bvars' v ss                 = v : bvars ss
    bvars (_ : ss)                      = bvars ss
    

instance BVars Lhs where
    bvars (LFun v ps)           = pvars ps
    bvars (LPat p)              = []

instance BVars [Pred] where
    bvars []                    = []
    bvars (PKind v k : ps)      = v : bvars ps
    bvars (PType (TVar v) : ps) = v : bvars ps
    bvars (p : ps)              = bvars ps


-- Free type variables -------------------------------------------------------

instance Ids Type where
  idents (TQual t ps)           = (idents t ++ idents ps) \\ bvars ps
  idents (TCon c)               = [c]
  idents (TVar v)               = [v]
  idents (TAp t t')             = idents t ++ idents t'
  idents (TSub t t')            = idents t ++ idents t'
  idents (TWild)                = []
  idents (TList t)              = idents t
  idents (TTup ts)              = concatMap idents ts
  idents (TFun t t')            = idents t ++ idents t'


instance Ids Pred where
  idents (PType (TVar v))       = []
  idents (PType t)              = idents t
  idents (PKind v k)            = []
