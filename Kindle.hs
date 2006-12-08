module Kindle where

import Common
import PP

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


-- A Kindle module consists of type declarations, value definitions and function definitions.  The
-- values are immutable and the corresponding RH sides are supposed to be run at initialization time.
-- Types and functions are visible everywhere in the module, while a value definition only scopes over
-- any definitions that appear later.  It is a static error if the value RH sides cannot be evaluated
-- at compile time without encountering a value that hasn't been initialized.
data Module     = Module  Name Decls Vals Funs
                deriving (Eq,Show)

type Decls      = Map Name Decl

-- A type declaration either introduces a struct type that defines the layout of heap-allocated objects,
-- or an enum type that defines a set of (parameterless) constructor names.  A struct may contain both
-- value and function definitions.  Each struct declaration introduces its own namespace for the defined
-- value and function fields.  The constructor names of an enum declaration belong to the top-level
-- value namespace.  All type names belong to a common space that is separate from all other namespaces.
data Decl       = Struct VEnv FEnv
                | Enum   [Name]
                deriving (Eq,Show)


type VEnv       = Map Name Type

type FEnv       = Map Name FType


type Vals       = Map Name Val

-- A value is defined by an expression and the corresponding type.
data Val        = Val    Type Exp
                deriving (Eq,Show)


type Funs       = Map Name Fun

-- A function is defined by function type (parameter types, result type), a list of parameter names and
-- a function body in the shape of a command.  The number of parameter types and parameter names must be 
-- equal.
data Fun        = Fun    FType [Name] Cmd
                deriving (Eq,Show)


-- A function type is just a pair of a parameter type list and a result type.
type FType      = ([Type], Type)

-- A type is either a name, which can be primitive or introduced in a type declaration, or the wildcard
-- type.
data Type       = TId    Name
                | TWild
                deriving (Eq,Show)


-- A function body is a command that computes the desired result, possibly while performing imperative
-- side-effects.  The current type system does not distinguish pure functions from side-effecting ones,
-- although that separation will indeed be maintained by the translation from type-checked Core programs.
data Cmd        = CRet    Exp                 -- simply return $1
                | CVals   Vals Cmd            -- define local values $1 in sequence, then execute $2
                | CFuns   Funs Cmd            -- define local recursive functions $1, execute tail $2
                | CAssign Exp Name Exp Cmd    -- overwrite value field $2 of struct $1 with value $3, execute tail $4
                | CSwitch Exp [Alt] Cmd       -- depending on the value of $1, choose tails from $2, default to $3
                | CSeq    Cmd Cmd             -- execute $1; if fall-through, continue with $2
                | CBreak                      -- break out of a surrounding switch
                deriving (Eq,Show)

-- Note 1: sequential function calls will be expressed as function calls on the RH sides of the value
-- definitions in a CVal command.
-- Note 2: the Cmd alternatives CSeq and CBreak are intended to implement the Fatbar and Fail primitives 
-- of the pattern-matching datatype PMC used in Core.

data Alt        = ACon    Name Cmd            -- execute tail $2 if switch value matches constructor name $1
                | ALit    Lit Cmd             -- execute tail $2 if switch value matches literal $1
                deriving (Eq,Show)

-- Simple expressions that can be RH sides of value definitions as well as function arguments.
data Exp        = EVar    Name                -- local or global value variable or function parameter
                | EThis                       -- the implicit first parameter of a function-valued struct field
                | ELit    Lit                 -- literal
                | ESel    Exp Name            -- selection of value field $2 from struct $1
                | ENew    Name Vals Funs      -- a new struct of type $1 filled with values $2 and functions $3.
                | ECall   Name [Exp]          -- calling local or global function $1 with arguments $2
                | EEnter  Exp Name [Exp]      -- calling function field $2 of struct $1 with arguments ($1++$3)
                | ECast   Type Exp            -- unchecked cast of value $2 to type $1
                deriving (Eq,Show)

-- Note: Kindle allows free variables to occur inside local functions and function-valued struct fields.  A
-- Kindle implementation must either handle this correctly at run-time, or such variable occurrences must be 
-- transformed away at compile-time.  The latter can be done using lambda-lifting for local functions and
-- explicitly closing the struct functions via extra value fields accessed through "this".


-- Tentative concrete syntax

instance Pr Module where
    pr (Module m ds vs fs)              = text "module" <+> prId m <+> text "where" $$
                                          vpr ds $$ 
                                          vpr vs $$
                                          vpr fs


instance Pr (Name, Decl) where
    pr (c, Struct ve fe)                = text "struct" <+> prId c <+> text "{" $$
                                          nest 4 (vpr ve) $$
                                          nest 4 (vpr fe) $$
                                          text "}"
    pr (c, Enum xs)                     = text "enum" <+> prId c <+> text "{" <> commasep pr xs <> text "}"


instance Pr (Name, Type) where
    pr (x, t)                           = pr t <+> prId x

instance Pr (Name, FType) where
    pr (x, (ts,t))                      = pr t <+> prId x <> parens (commasep pr ts) <> text ";"


instance Pr Type where
    pr (TId c)                          = prId c
    pr (TWild)                          = text "?"


instance Pr (Name, Val) where
    pr (x, Val t e)                     = pr t <+> prId x <+> text "=" <+> pr e


instance Pr (Name, Fun) where
    pr (x, Fun (ts,t) xs c)             = pr t <+> prId x <+> parens (commasep pr (xs `zip` ts)) <+> text "{" $$
                                          nest 4 (pr c) $$
                                          text "}"

instance Pr Cmd where
    pr (CRet e)                         = text "return" <+> pr e <> text ";"
    pr (CVals vs c)                     = vpr vs $$
                                          pr c
    pr (CFuns fs c)                     = vpr fs $$
                                          pr c
    pr (CAssign e x e' c)               = pr e <> text "." <> prId x <+> text ":=" <+> pr e' <> text ";" $$
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
    pr (ACon x c)                       = prId x <> text ":" <+> prScope c
    pr (ALit l c)                       = pr l <> text ":" <+> prScope c


prDefault c                             = text "default: " <+> prScope c


instance Pr Exp where
    pr (EVar x)                         = prId x
    pr (ELit l)                         = pr l
    pr (ESel e l)                       = pr e <> text "." <> pr l
    pr (ENew x vs [])                   = text "new" <+> prId x <+> text "{" <> commasep pr vs <> text "}"
    pr (ENew x vs fs)                   = text "new" <+> prId x <+> text "{" $$
                                          nest 4 (vpr vs $$ vpr fs) $$
                                          text "}"
    pr (ECall x es)                     = prId x <+> parens (commasep pr es)
    pr (EEnter e x es)                  = pr e <> text "." <> prId x <> parens (commasep pr es)
    pr (ECast t e)                      = parens (pr t) <> pr e


