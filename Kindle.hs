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


-- A Kindle module consists of type declarations and term bindings.  A type declaration introduces
-- either a struct type or an enumeration type.  A binding either defines either a named function or 
-- a named value of atomic type.  Both binding forms are immutable.  All type declarations are 
-- mutually recursive, whereas term-level recursion is only supported within groups of adjacent 
-- function bindings.  Otherwise bindings scope over subsequent definitions.
data Module     = Module  Name Decls Binds
                deriving (Eq,Show)

-- A type declaration either introduces a struct type that defines the layout of heap-allocated 
-- objects, or an enumeration type that defines a set of (parameterless) constructor names.  A 
-- struct may contain both value and function bindings.  The field names of a struct as well as the 
-- constructor names of an enum belong to the top-level term namespace, and must therefore be
-- globnally unique.  All type names belong to a common namespace that is disjoint from the term
-- namespace.
type Decls      = Map Name Decl

data Decl       = Struct TEnv
                | Enum   [Name]
                deriving (Eq,Show)


-- A term binding is either a named value of atomic type or a named function.  The result and parameter
-- types of a function must all be atomic.
type Binds      = Map Name Bind

data Bind       = Val    AType Exp
                | Fun    AType ATEnv Cmd
                deriving (Eq,Show)


-- The type of a binding is either just an atomic type in case of a value binding, or a pair of
-- result and parameter types in the function binding case.
type TEnv       = Map Name Type

data Type       = TVal   AType
                | TFun   [AType] AType
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
                | CBind   Binds Cmd           -- introduce local value or function bindings $1, then execute tail $2
                | CAssign Exp Name Exp Cmd    -- overwrite value field $2 of struct $1 with value $3, execute tail $4
                | CSwitch Exp [Alt] Cmd       -- depending on the value of $1, choose tails from $2, default to $3
                | CSeq    Cmd Cmd             -- execute $1; if fall-through, continue with $2
                | CBreak                      -- break out of a surrounding switch
                deriving (Eq,Show)

-- Note 1: sequential function calls are expressed as function calls on the RH sides of value bindings
-- in a CBind command.
-- Note 2: scoping rules for the bindings in a CBind command are the same as for binding sequences on the
-- module top level.
-- Note 3: the Cmd alternatives CSeq and CBreak are intended to implement the Fatbar and Fail primitives 
-- of the pattern-matching datatype PMC used in Core.

data Alt        = ACon    Name Cmd            -- execute tail $2 if switch value matches constructor name $1
                | ALit    Lit Cmd             -- execute tail $2 if switch value matches literal $1
                deriving (Eq,Show)

-- Simple expressions that can be RH sides of value definitions as well as function arguments.
data Exp        = EVar    Name                -- local or global value name, enum constructor or function parameter
                | EThis                       -- the implicit first parameter of a function-valued struct field
                | ELit    Lit                 -- literal
                | ESel    Exp Name            -- selection of value field $2 from struct $1
                | ENew    Name Binds          -- a new struct of type $1 filled with values and functions from $2
                | ECall   Name [Exp]          -- calling local or global function $1 with arguments $2
                | EEnter  Exp Name [Exp]      -- calling function field $2 of struct $1 with arguments ($1++$3)
                | ECast   AType Exp           -- unchecked cast of value $2 to type $1
                deriving (Eq,Show)

-- Note: Kindle allows free variables to occur inside local functions and function-valued struct fields.  A
-- Kindle implementation must either handle this correctly at run-time, or such variable occurrences must be 
-- transformed away at compile-time.  The latter can be done using lambda-lifting for local functions and
-- explicitly closing the struct functions via extra value fields accessed through "this".


isVal (_, Val _ _)                      = True
isVal (_, Fun _ _ _)                    = False

extractEnvs vs fs                       = (mapSnd extractVType vs, mapSnd extractFType fs)
  where extractVType (Val t e)          = e
        extractFType (Fun t xs c)       = t 



-- Tentative concrete syntax

instance Pr Module where
    pr (Module m ds bs)                 = text "module" <+> prId m <+> text "where" $$
                                          vpr ds $$ 
                                          vpr bs


instance Pr (Name, Decl) where
    pr (c, Struct te)                   = text "struct" <+> prId c <+> text "{" $$
                                          nest 4 (vpr te) $$
                                          text "}"
    pr (c, Enum xs)                     = text "enum" <+> prId c <+> text "{" <> commasep pr xs <> text "}"


instance Pr (Name, Type) where
    pr (x, TVal t)                      = pr t <+> prId x
    pr (x, TFun ts t)                   = pr t <+> prId x <> parens (commasep pr ts) <> text ";"


instance Pr AType where
    pr (TId c)                          = prId c
    pr (TWild)                          = text "?"

instance Pr (Name, AType) where
    pr (x, t)                           = pr t <+> prId x


instance Pr (Name, Bind) where
    pr (x, Val t e)                     = pr t <+> prId x <+> text "=" <+> pr e
    pr (x, Fun t te c)                  = pr t <+> prId x <+> parens (commasep pr te) <+> text "{" $$
                                          nest 4 (pr c) $$
                                          text "}"

instance Pr Cmd where
    pr (CRet e)                         = text "return" <+> pr e <> text ";"
    pr (CBind bs c)                     = vpr bs $$
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
    pr (EThis)                          = text "this"
    pr (ELit l)                         = pr l
    pr (ESel e l)                       = pr e <> text "." <> pr l
    pr (ENew x bs)
      | all isVal bs                    = text "new" <+> prId x <+> text "{" <> commasep pr bs <> text "}"
    pr (ENew x bs)                      = text "new" <+> prId x <+> text "{" $$
                                          nest 4 (vpr bs) $$
                                          text "}"
    pr (ECall x es)                     = prId x <+> parens (commasep pr es)
    pr (EEnter e x es)                  = pr e <> text "." <> prId x <> parens (commasep pr es)
    pr (ECast t e)                      = parens (pr t) <> pr e


