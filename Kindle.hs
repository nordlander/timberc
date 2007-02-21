module Kindle where

import Monad
import Common
import PP
import qualified Core
import qualified Env

-- Kindle is the main back-end intermediate language.  It is a typed imerative language with dynamic 
-- memory allocation and garbage-collection, that can be described as a slightly extended version of
-- the common subset of C, Java and C++.  The purpose of Kindle is to function as a high-level back-
-- end program format that can be translated into standard imperative languages as well as assembly
-- code without too much difficulty.  None of C's pointer arithmetic features are present, neither
-- are the class hieracrhies of Java and C++.  Unsafe type-casts are supported, and there is an
-- explicit wildcard (word-sized) type written ?.  The main extension compared to Java is nested
-- recursive functions.  Heap-allocated struct objects may contain function-valued components which
-- are invoked using self-application.  This provides a form of basic OO capability that may serve
-- as a target for a closure conversion pass.  For more details on the relation between Kindle and
-- the primary intermediate language Core, see module Core2Kindle.


-- A Kindle module consists of type declarations and term bindings.  A type declaration introduces
-- either a struct type or an enumeration type.  A binding either defines either a named function or 
-- a named value of atomic type.  Both binding forms are immutable.  All type declarations are 
-- mutually recursive, whereas term-level recursion is only supported within groups of adjacent 
-- function bindings.  Otherwise bound names scope over subsequent bindings.
data Module     = Module  Name Decls Binds
                deriving (Eq,Show)

-- A type declaration either introduces a struct type that defines the layout of heap-allocated 
-- objects, or an enumeration type that defines a set of (parameterless) constructor names.  A 
-- struct may contain both value and function fields.  Each struct type introduces a private 
-- namespace for its field names.  The constructor names of an enum belong to the top-level term 
-- namespace, and must therefore be globally unique.  All type names belong to a common namespace 
-- that is disjoint from every other namespace.
type Decls      = Map Name Decl

data Decl       = Struct TEnv
                | Enum   [Name]
                deriving (Eq,Show)


-- A term binding is either a named value of atomic type or a named function.  The result and 
-- parameter types of a function must all be atomic.
type Binds      = Map Name Bind

data Bind       = Val    AType Exp
                | Fun    AType ATEnv Cmd
                deriving (Eq,Show)


-- The type of a binding is either just an atomic type in case of a value binding, or a pair of
-- parameter and result types in the function binding case.
type TEnv       = Map Name Type

data Type       = ValT   AType
                | FunT   [AType] AType
                deriving (Eq,Show)


-- An atomic type is either a name, which can be primitive or introduced in a type declaration, or 
-- the wildcard type that stands for any atomic type.
type ATEnv      = Map Name AType

data AType      = TId    Name
                | TWild
                deriving (Eq,Show)


-- A function body is a command that computes the desired result, possibly while performing imperative
-- side-effects.  The current type system does not distinguish pure functions from side-effecting ones,
-- although that separation will indeed be maintained by the translation from type-checked Core programs.
data Cmd        = CRet    Exp                 -- simply return $1
                | CRun    Exp Cmd             -- evaluate $1 for its side-effects only, then execure tail $2
                | CBind   Bool Binds Cmd      -- introduce (recursive? $1) local bindings $2, then execute tail $3
                | CAssign Exp Name Exp Cmd    -- overwrite value field $2 of struct $1 with value $3, execute tail $4
                | CSwitch Exp [Alt] Cmd       -- depending on the value of $1, choose tails from $2, default to $3
                | CSeq    Cmd Cmd             -- execute $1; if fall-through, continue with $2
                | CBreak                      -- break out of a surrounding switch
                deriving (Eq,Show)

-- Note 1: command (CRun e c) is identical to (CBind False [(x,e)] c) if x is a fresh name not used anywhere else
-- Note 2: the Cmd alternatives CSeq and CBreak are intended to implement the Fatbar and Fail primitives 
-- of the pattern-matching datatype PMC used in Core.

data Alt        = ACon    Name Cmd            -- execute tail $2 if switch value matches constructor name $1
                | ALit    Lit Cmd             -- execute tail $2 if switch value matches literal $1
                deriving (Eq,Show)

-- Simple expressions that can be RH sides of value definitions as well as function arguments.
data Exp        = EVar    Name                -- local or global value name, enum constructor or function parameter
                | EThis                       -- the implicit first parameter of a function-valued struct field
                | ELit    Lit                 -- literal
                | ESel    Exp Name            -- selection of value field $2 from struct $1
                | ENew    Name Binds          -- a new struct of type $1 (partially) initialized from $2
                | ECall   Name [Exp]          -- calling local or global function $1 with arguments $2
                | EEnter  Exp Name [Exp]      -- calling function field $2 of struct $1 with arguments ($1,$3)
                | ECast   AType Exp           -- unchecked cast of value $2 to type $1
                deriving (Eq,Show)

-- Note: Kindle allows free variables to occur inside local functions and function-valued struct fields.  A
-- Kindle implementation must either handle this correctly at run-time, or such variable occurrences must be 
-- transformed away at compile-time.  The latter can be done using lambda-lifting for local functions and
-- explicitly closing the struct functions via extra value fields accessed through "this".


litType (LInt _)                        = TId (prim Int)
litType (LRat _)                        = TId (prim Float)
litType (LChr _)                        = TId (prim Char)
litType (LStr _)                        = error "Internal chaos: Kindle.litType LStr"

primDecls                               = (prim Bool,       Enum   [prim FALSE, prim TRUE]) :
                                          (prim UNIT,       Enum   [prim UNIT]) :
                                          (prim LISTtags,   Enum   [prim NIL, prim CONS]) :
                                          (prim LIST,       Struct [(name0 "tag", ValT (TId (prim LISTtags)))]) :
                                          (prim NIL,        Struct [(name0 "tag", ValT (TId (prim LISTtags)))]) :
                                          (prim CONS,       Struct [(name0 "tag", ValT (TId (prim LISTtags))),
                                                                    (name0 "hd",  ValT TWild), 
                                                                    (name0 "tl",  ValT (TId (prim LIST)))]) :
                                          (prim Msg,        Struct [(prim Code, FunT [] TWild),
                                                                    (prim Baseline, ValT (TId (prim Time))),
                                                                    (prim Deadline, ValT (TId (prim Time)))]) :
                                          []
                                          
primCons                                = (prim UNIT,       prim UNIT) :
                                          (prim NIL,        prim LIST) :
                                          (prim CONS,       prim LIST) :
                                          (prim TRUE,       prim Bool) :
                                          (prim FALSE,      prim Bool) :
                                          []

primKindleTerms                         = map prim [ IntPlus .. TimeGT ]

primTEnv                                = map cv (Env.primTypeEnv `restrict` primKindleTerms)
  where 
    cv (x,Core.Scheme (Core.R t) [] []) = (x, cv1 t)
    cv _                                = error "Internal: Kindle.primTEnv"
    cv1 (Core.TFun ts t)                = FunT (map cv2 ts) (cv2 t)
    cv1 t                               = ValT (cv2 t)
    cv2 (Core.TId n)                    = TId n
    cv2 _                               = error "Internal: Kindle.primTEnv"


searchFields ds x                       = [ (TId n, t) | (n, Struct te) <- ds, (x',t) <- te, x==x' ]

searchCons ds x                         = [ TId n | (n, Enum xs) <- ds, x `elem` xs ]


isVal (_, Val _ _)                      = True
isVal (_, Fun _ _ _)                    = False

isFunT (_, ValT _)                      = False
isFunT (_, FunT _ _)                    = True

typeOf (Val t e)                        = ValT t
typeOf (Fun t te c)                     = FunT (rng te) t

rngType (ValT t)                        = t
rngType (FunT ts t)                     = t

typeOf' b                               = rngType (typeOf b)

cBind [] c                              = c
cBind bs c                              = CBind False bs c

cBindR r [] c                           = c
cBindR r bs c                           = CBind r bs c

protect x t c                           = liftM (CRun (ECall (prim LOCK) [e])) (protect' e t c)
  where e                               = ECast (TId (prim PID)) (EVar x)

protect' e0 t (CRet e)
  | sensitive e                         = do y <- newName tempSym
                                             return (cBind [(y,Val t e)] (CRun (ECall (prim UNLOCK) [e0]) (CRet (EVar y))))
  | otherwise                           = return (CRun (ECall (prim UNLOCK) [e0]) (CRet e))
protect' e0 t (CRun e c)                = liftM (CRun e) (protect' e0 t c)
protect' e0 t (CBind r bs c)            = liftM (CBind r bs) (protect' e0 t c)
protect' e0 t (CAssign e y e' c)        = liftM (CAssign e y e') (protect' e0 t c)
protect' e0 t (CSwitch e alts c)        = liftM2 (CSwitch e) (mapM (protect'' e0 t) alts) (protect' e0 t c)
protect' e0 t (CSeq c c')               = liftM2 CSeq (protect' e0 t c) (protect' e0 t c')
protect' e0 t (CBreak)                  = return CBreak

protect'' e0 t (ACon y c)               = liftM (ACon y) (protect' e0 t c)
protect'' e0 t (ALit l c)               = liftM (ALit l) (protect' e0 t c)


sensitive (ESel e n)                    = True
sensitive (ECall n es)                  = True
sensitive (EEnter e n es)               = True
sensitive (ENew n bs)                   = True
sensitive (ECast t e)                   = sensitive e
sensitive e                             = False

{-
cast t (CRet e)                         = CRet (ECast t e)
cast t (CRun e c)                       = CRun e (cast t c)
cast t (CBind r bs c)                   = CBind r bs (cast t c)
cast t (CAssign e x e' c)               = CAssign e x e' (cast t c)
cast t (CSwitch e alts c)               = CSwitch e (map cast' alts) (cast t c)
  where cast' (ACon x c)                = ACon x (cast t c)
        cast' (ALit l c)                = ALit l (cast t c)
cast t (CSeq c c')                      = CSeq (cast t c) (cast t c')
cast t (CBreak)                         = CBreak
-}


-- Free variables ------------------------------------------------------------------------------------

instance Ids Exp where
    idents (EVar x)                     = [x]
    idents (EThis)                      = []
    idents (ESel e l)                   = idents e
    idents (ENew x bs)                  = idents bs
    idents (ECall x es)                 = x : idents es
    idents (EEnter e x es)              = idents e ++ idents es
    idents (ECast t e)                  = idents e

instance Ids Cmd where
    idents (CRet e)                     = idents e
    idents (CRun e c)                   = idents e ++ idents c
    idents (CBind False bs c)           = idents bs ++ (idents c \\ dom bs)
    idents (CBind True bs c)            = (idents bs ++ idents c) \\ dom bs
    idents (CAssign e x e' c)           = idents e ++ idents e' ++ idents c
    idents (CSwitch e alts d)           = idents e ++ idents alts ++ idents d
    idents (CSeq c c')                  = idents c ++ idents c'
    idents (CBreak)                     = []

instance Ids Alt where
    idents (ACon x c)                   = idents c
    idents (ALit l c)                   = idents c

instance Ids Bind where
    idents (Val t e)                    = idents e
    idents (Fun t te c)                 = idents c \\ dom te


-- Tentative concrete syntax ------------------------------------------------------------------------------

instance Pr Module where
    pr (Module m ds bs)                 = text "module" <+> prId2 m <+> text "where" $$
                                          vpr ds $$ 
                                          vpr bs


instance Pr (Name, Decl) where
    pr (c, Struct te)                   = text "struct" <+> prId2 c <+> text "{" $$
                                          nest 4 (vpr te) $$
                                          text "}"
    pr (c, Enum xs)                     = text "enum" <+> prId2 c <+> text "{" <> commasep pr xs <> text "}"


instance Pr (Name, Type) where
    pr (x, ValT t)                      = pr t <+> prId2 x
    pr (x, FunT ts t)                   = pr t <+> prId2 x <> parens (commasep pr ts) <> text ";"


instance Pr AType where
    pr (TId c)                          = prId2 c
    pr (TWild)                          = text "POLY"

instance Pr (Name, AType) where
    pr (x, t)                           = pr t <+> prId2 x


instance Pr (Name, Bind) where
    pr (x, Val t e)                     = pr t <+> prId2 x <+> text "=" <+> pr e <> text ";"
    pr (x, Fun t te c)                  = pr t <+> prId2 x <+> parens (commasep pr te) <+> text "{" $$
                                          nest 4 (pr c) $$
                                          text "}"

instance Pr Cmd where
    pr (CRet e)                         = text "return" <+> pr e <> text ";"
    pr (CRun e c)                       = pr e <> text ";" $$
                                          pr c
    pr (CBind r bs c)                   = vpr bs $$
                                          pr c
    pr (CAssign e x e' c)               = pr e <> text "." <> prId2 x <+> text ":=" <+> pr e' <> text ";" $$
                                          pr c
    pr (CSwitch e alts d)               = text "switch" <+> parens (pr e) <+> text "{" $$
                                          nest 2 (vpr alts $$ prDefault d) $$
                                          text "}"
    pr (CSeq c1 c2)                     = pr c1 $$
                                          pr c2
    pr (CBreak)                         = text "break;"



prScope (CBreak)                        = pr CBreak
prScope (CRet x)                        = pr (CRet x)
prScope c                               = text "{" <+> pr c $$
                                          text "}"

instance Pr Alt where
    pr (ACon x c)                       = prId2 x <> text ":" <+> prScope c
    pr (ALit l c)                       = pr l <> text ":" <+> prScope c


prDefault c                             = text "default: " <+> prScope c


instance Pr Exp where
    pr (EVar x)                         = prId2 x
    pr (EThis)                          = text "this"
    pr (ELit l)                         = pr l
    pr (ESel e l)                       = pr e <> text "->" <> pr l
    pr (ENew x bs)
      | all isVal bs                    = text "new" <+> prId2 x <+> text "{" <> commasep prInit bs <> text "}"
    pr (ENew x bs)                      = text "new" <+> prId2 x <+> text "{" $$
                                          nest 4 (vpr bs) $$
                                          text "}"
    pr (ECall x es)                     = prId2 x <+> parens (commasep pr es)
    pr (EEnter e x es)                  = pr e <> text "." <> prId2 x <> parens (commasep pr es)
    pr (ECast t e)                      = parens (pr t) <> pr e


prInit (x, Val t e)                     = prId2 x <+> text "=" <+> pr e
prInit b                                = pr b