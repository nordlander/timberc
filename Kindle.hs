module Kindle where

import Monad
import Common
import PP
import qualified Core
import qualified Env
import Data.Binary
import Control.Monad.Identity

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
data Module     = Module  Name [Name] Decls Binds
                deriving (Eq,Show)

-- A type declaration introduces a struct type that defines the layout of heap-allocated objects.
-- A struct may contain both value and function fields, and each struct type introduces a private 
-- namespace for its field names.  As an option, a struct type can be accompanied by a list of 
-- struct type names that constitute variants of the declared type.  Such names also qualify as 
-- tag names of type Int at the term level, and might therefore be used identify a particular 
-- variant when stored as a value in a common tag field.  All type names belong to a common 
-- namespace that is disjoint from every other namespace.
type Decls      = Map Name Decl

data Decl       = Struct TEnv [Name]
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
                | TArray AType
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


litType (LInt _ _)                        = TId (prim Int)
litType (LRat _ _)                        = TId (prim Float)
litType (LChr _ _)                        = TId (prim Char)
litType (LStr _ _)                        = internalError0 "Kindle.litType LStr"

tagSig                                  = (prim Tag, ValT (TId (prim Int)))

primDecls                               = (prim Bool,       Struct [tagSig] [prim FALSE, prim TRUE]) :
                                          (prim FALSE,      Struct [tagSig] []) :
                                          (prim TRUE,       Struct [tagSig] []) :
                                          (prim UNITTYPE,   Struct [tagSig] [prim UNITTERM]) :
                                          (prim UNITTERM,   Struct [tagSig] []) :
                                          (prim LIST,       Struct [tagSig] [prim NIL, prim CONS]) :
                                          (prim NIL,        Struct [tagSig] []) :
                                          (prim CONS,       Struct (tagSig : abcSupply `zip` [ValT TWild, ValT (TId (prim LIST))]) []) :
                                          (prim Msg,        Struct [(prim Code, FunT [] tUNIT),
                                                                    (prim Baseline, ValT (TId (prim AbsTime))),
                                                                    (prim Deadline, ValT (TId (prim AbsTime))),
                                                                    (prim Next, ValT (TId (prim Msg)))] []) :
                                          []
                                          
objTEnv                                 = [(prim Obj, ValT tObject)]

objInit                                 = [(prim Obj, Val tObject (EVar (prim ObjInit)))]

primKindleTerms                         = map prim [ MIN____VAR .. MAX____KINDLEVAR ]

primTEnv                                = map cv (Env.primTypeEnv `restrict` primKindleTerms) ++ primTEnv0
  where 
    cv (x,Core.Scheme r [] _)           = (x, cv0 r)
    cv0 (Core.F ts t)                   = FunT (map cv1 ts) (cv2 t)
    cv0 (Core.R t)                      = ValT (cv3 t)
    cv1 (Core.Scheme r [] _)            = cv2 r
    cv2 (Core.R t)                      = cv3 t
    cv3 (Core.TId n)
      | isCon n                         = TId n
      | otherwise                       = TWild
    cv3 (Core.TAp (Core.TId (Prim Array _)) t) = TArray (cv3 t)
    cv3 (Core.TAp t _)                  = cv3 t

tTime                                   = TId (prim Time)
tMsg                                    = TId (prim Msg)
tPID                                    = TId (prim PID)
tUNIT                                   = TId (prim UNITTYPE)
tObject                                 = TId (prim Object)

primTEnv0                               = (prim ASYNC,   FunT [tMsg,tTime,tTime] tUNIT) :
                                          (prim LOCK,    FunT [tPID] tUNIT) :
                                          (prim UNLOCK,  FunT [tPID] tUNIT) :
                                          (prim Inherit, ValT tTime) :
                                          []


boxedPrims                              = [Int, Float, Time]

smallPrims                              = [Char, Bool, UNITTYPE]

tupleSels (Tuple n _)                   = take n abcSupply `zip` repeat TWild


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

enter es e@(ECall (Prim Raise _) _)     = e
enter es e                              = EEnter e (prim Code) es


mkSig (n,Val at _)                      = (n,ValT at)
mkSig (n,Fun at ats _)                  = (n,FunT (map snd ats) at)


simpleExp (EVar _)                      = True
simpleExp (ELit _)                      = True
simpleExp (ECast _ e)                   = simpleExp e
simpleExp _                             = False

lock t e                                = ECast t (ECall (prim LOCK) [ECast (TId (prim PID)) e])

unlock x c                              = cMap (CRun (ECall (prim UNLOCK) [e0]) . CRet) c
  where e0                              = ECast (TId (prim PID)) (EVar x)

cMap f (CRet e)                         = f e
cMap f (CRun e c)                       = CRun e (cMap f c)
cMap f (CBind r bs c)                   = CBind r bs (cMap f c)
cMap f (CAssign e y e' c)               = CAssign e y e' (cMap f c)
cMap f (CSwitch e alts d)               = CSwitch e (map (aMap f) alts) (cMap f d)
cMap f (CSeq c c')                      = CSeq (cMap f c) (cMap f c')
cMap f (CBreak)                         = CBreak

aMap f (ACon y c)                       = ACon y (cMap f c)
aMap f (ALit l c)                       = ALit l (cMap f c)


class CMap a where
    cmap :: (Exp -> Exp) -> a -> a

instance CMap Exp where
    cmap = id

instance CMap Cmd where
    cmap f = cMap (CRet . f)

instance CMap Alt where
    cmap f = aMap (CRet . f)


-- Free variables ------------------------------------------------------------------------------------

instance Ids Exp where
    idents (EVar x)                     = [x]
    idents (EThis)                      = []
    idents (ELit l)                     = []
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
    pr (Module m ns ds bs)              = text "module" <+> prId2 m <+> text "where" $$
                                          text "import" <+> hpr ',' ns $$
                                          vpr ds $$ 
                                          vpr bs


instance Pr (Module,a) where
    pr (m,_)                            = pr m


instance Pr (Name, Decl) where
    pr (c, Struct te xs)                = text "struct" <+> prId2 c <+> text "{" $$
                                          nest 4 (vpr te) $$
                                          text "}" $$
                                          prEnum xs

prEnum []                               = empty
prEnum xs                               = text "enum" <+> text "{" <> commasep pr xs <> text "}"


instance Pr (Name, Type) where
    pr (x, ValT t)                      = pr t <+> prId2 x <> text ";"
    pr (x, FunT ts t)                   = pr t <+> prId2 x <> parens (commasep pr ts) <> text ";"


instance Pr AType where
    pr (TId c)                          = prId2 c
    pr (TArray t)                       = text "Array " <+> parens (pr t)
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
    pr (CAssign e x e' c)               = pr e <> text "->" <> prId2 x <+> text "=" <+> pr e' <> text ";" $$
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
    prn 0 (ECast t e)                   = parens (pr t) <> prn 0 e
    prn 0 e                             = prn 1 e
    
    prn 1 (EVar x)                      = prId2 x
    prn 1 (EThis)                       = text "this"
    prn 1 (ELit l)                      = pr l
    prn 1 (ENew x bs)
      | all isVal bs                    = text "new" <+> prId2 x <+> text "{" <> commasep prInit bs <> text "}"
    prn 1 (ENew x bs)                   = text "new" <+> prId2 x <+> text "{" $$
                                          nest 4 (vpr bs) $$
                                          text "}"
    prn 1 (ECall x es)                  = prId2 x <> parens (commasep pr es)
    prn 1 (ESel e l)                    = prn 1 e <> text "->" <> prId2 l
    prn 1 (EEnter e x es)               = prn 1 e <> text "." <> prId2 x <> parens (commasep pr es)
    prn 1 e                             = parens (prn 0 e)


prInit (x, Val t e)                     = prId2 x <+> text "=" <+> pr e
prInit b                                = pr b

-- Binary --------------------
{-
instance Binary Module where
  put (Module a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Module a b c d)
-}
instance Binary Decl where
  put (Struct a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Struct a b)
      _ -> fail "no parse"
{-
instance Binary Bind where
  put (Val a b) = putWord8 0 >> put a >> put b
  put (Fun a b c) = putWord8 1 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Val a b)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Fun a b c)
      _ -> fail "no parse"
-}
instance Binary Type where
  put (ValT a) = putWord8 0 >> put a
  put (FunT a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ValT a)
      1 -> get >>= \a -> get >>= \b -> return (FunT a b)
      _ -> fail "no parse"

instance Binary AType where
  put (TId a) = putWord8 0 >> put a
  put (TArray a) = putWord8 1 >> put a
  put TWild = putWord8 2
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (TId a)
      1 -> get >>= \a -> return (TArray a)
      2 -> return TWild
      _ -> fail "no parse"

