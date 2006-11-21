module Kindle where

import Common
import PP


data Module     = Module  Name Decls Funs Vals
                deriving (Eq,Show)

type Decls      = Map Name Decl

data Decl       = Struct TEnv
                | Enum   [Name]
                deriving (Eq,Show)


type Funs       = Map Name Fun

data Fun        = Fun    Type TEnv Cmd
                deriving (Eq,Show)


type Vals       = Map Name Val

data Val        = Val    Type Exp
                deriving (Eq,Show)


type TEnv       = Map Name Type

data Type       = TFun   [Type] Type
                | TId    Name
                | TPoly
                deriving (Eq,Show)


data Bind       = BFun    Funs
                | BVal    Name Val
                | BAss    Exp Exp
                deriving (Eq,Show)


data Cmd        = CRet    Name
                | CBind   Bind Cmd
                | CCase   Name [Alt] Cmd
                | CCaseL  Name [AltL] Cmd
                | CSeq    Cmd Cmd
                | CBreak
                deriving (Eq,Show)

data Alt        = Alt     Name Cmd
                deriving (Eq,Show)

data AltL       = AltL    Lit Cmd
                deriving (Eq,Show)

data Exp        = EVar    Name
                | ELit    Lit
                | ESel    Exp Name
                | ENew    Name
                | ECall   Exp [Exp]
                | ECast   Type Exp
                deriving (Eq,Show)



findStructByName [] x                   = error ("findStructByName " ++ show x)
findStructByName ((y,s@(Struct te)):ds) x
  | x == y                              = s
findStructByName (_:ds) x               = findStructByName ds x


findStructByLabel [] x                  = error ("findStructByLabel " ++ show x)
findStructByLabel ((y,s@(Struct te)):ds) x
  | x `elem` dom te                     = s
findStructByLabel (_:ds) x              = findStructByLabel ds x


findEnumByName [] x                     = error ("findEnumByName " ++ show x)
findEnumByName ((y,e@(Enum xs)):ds) x
  | x == y                              = e
findEnumByName (_:ds) x                 = findEnumByName ds x


findEnumByTag [] x                      = error ("findEnumByTag " ++ show x)
findEnumByTag ((y,e@(Enum xs)):ds) x
  | x `elem` xs                         = e
findEnumByTag (_:ds) x                  = findEnumByTag ds x



conOfAlt (Alt c b)         = c

litOfAlt (AltL l b)        = l


instance Ids Exp where
    idents (EVar x)                     = [x]
    idents (ELit l)                     = []
    idents (ECall e es)                 = idents e ++ idents es
    idents (ESel e l)                   = idents e
    idents (ENew c)                     = []
    idents (ECast t e)                  = idents e


instance Ids Cmd where
    idents (CRet x)                     = [x]
    idents (CBind b c)                  = idents b ++ (idents c \\ bvars b)
    idents (CCase x alts d)             = x : idents alts ++ idents d
    idents (CCaseL x alts d)            = x : idents alts ++ idents d
    idents (CSeq c1 c2)                 = idents c1 ++ idents c2
    idents (CBreak)                     = []

instance Ids Bind where
    idents (BFun fs)                    = idents fs \\ dom fs
    idents (BVal x v)                   = idents v
    idents (BAss e e')                  = idents e ++ idents e'

instance Ids Fun where
    idents (Fun t te c)                 = idents c \\ dom te

instance Ids Val where
    idents (Val t e)                    = idents e

instance Ids Alt where
    idents (Alt x c)                    = idents c

instance Ids AltL where
    idents (AltL l c)                   = idents c


instance BVars Bind where
    bvars (BFun fs)                     = dom fs
    bvars (BVal x v)                    = [x]
    bvars (BAss e e')                   = []


-- Note! This substitution algorithm does not alpha convert!
-- Only use when variables are known not to clash

instance Subst Fun Name Name where
    subst s (Fun t te c)                = Fun t te (subst s c)

instance Subst Val Name Name where
    subst s (Val t e)                   = Val t (subst s e)

instance Subst Cmd Name Name where
    subst [] b                          = b
    subst s (CRet x)                    = CRet (substVar s x)
    subst s (CBind b c)                 = CBind (subst s b) (subst s c)
    subst s (CCase x alts d)            = CCase (substVar s x) (subst s alts) (subst s d)
    subst s (CCaseL x alts d)           = CCaseL (substVar s x) (subst s alts) (subst s d)
    subst s (CSeq c1 c2)                = CSeq (subst s c1) (subst s c2)
    subst s (CBreak)                    = CBreak

instance Subst Bind Name Name where
    subst s (BFun fs)                   = BFun (subst s fs)
    subst s (BVal x v)                  = BVal x (subst s v)
    subst s (BAss e e')                 = BAss (subst s e) (subst s e')

instance Subst Alt Name Name where
    subst s (Alt x c)                   = Alt x (subst s c)

instance Subst AltL Name Name where
    subst s (AltL l c)                  = AltL l (subst s c)

instance Subst Exp Name Name where
    subst [] e                          = e
    subst s (EVar x)                    = case lookup x s of
                                            Just x' -> EVar x'
                                            Nothing -> EVar x
    subst s (ELit l)                    = ELit l
    subst s (ESel e l)                  = ESel (subst s e) l
    subst s (ECall e es)                = ECall (subst s e) (subst s es)
    subst s (ENew x)                    = ENew x

    subst s (ECast t e)                 = ECast t (subst s e)





instance Subst Exp Name Exp where
    subst [] e                          = e
    subst s (EVar x)                    = case lookup x s of
                                            Just e  -> e
                                            Nothing -> EVar x
    subst s (ELit l)                    = ELit l
    subst s (ESel e l)                  = ESel (subst s e) l
    subst s (ECall e es)                = ECall (subst s e) (subst s es)
    subst s (ENew x)                    = ENew x

    subst s (ECast t e)                 = ECast t (subst s e)

instance Subst Val Name Exp where
    subst [] v                          = v
    subst s (Val t e)                   = Val t (subst s e)

{-

add (x,y) = x+y

map (f,xs) = case xs of
               Nil  -> Nil
               Cons -> Cons ( f (xs.head), map (f, xs.tail) )

g(x,as) = map (add(x), as)


g(x,as) = map (\y -> add(x,y), as)

g(x,as) = let c(y) = add(x,y) in map (c,as)

c(x,y) = add(x,y)
g(x,as) = map (c(x),as)

-------------

h1 :: List ((A,B)->C->Int) -> Bool
h2 :: List ((A,B,C)->Int) -> Bool

g(xs) = h1(xs) && h2(xs)
  

-}



instance Pr Module where
    pr (Module m cs fs vs)              = text "module" <+> prId m <+> text "where" $$
                                          vpr cs $$
                                          vpr fs $$
                                          vpr vs


instance Pr (Name, Decl) where
    pr (c, Struct te)                   = text "struct" <+> prId c <+> text "{" $$
                                          nest 4 (pr te) $$
                                          text "}"
    pr (c, Enum ce)                     = text "enum" <+> prId c <+> text "{" $$
                                          nest 4 (vpr ce) $$
                                          text "}"

instance Pr TEnv where
    pr te                               = vcat (map prSig te)
      where prSig (x,t)                 = pr t <+> prId x <> text ";"


instance Pr (Name, Fun) where
    pr (x, Fun t te c)                  = pr t <+> prId x <+> parens (commasep pr te) <+> text "{" $$
                                          nest 4 (pr c) $$
                                          text "}"

instance Pr (Name, Type) where
    pr (x, t)                           = pr t <+> prId x


instance Pr Type where
    prn 0 (TFun ts t)                   = parens (commasep pr ts) <> prn 1 t
    prn 0 t                             = prn 1 t
    prn 1 (TId c)                       = prId c
    prn 1 (TPoly)                       = text "?"
    prn 1 t                             = parens (pr t)


instance Pr (Name, Val) where
    pr (x, Val t e)                     = pr t <+> prId x <+> text "=" <+> pr e


instance Pr Cmd where
    pr (CRet x)                         = text "return" <+> prId x <> text ";"
    pr (CBind b c)                      = pr b $$ 
                                          pr c
    pr (CCase x alts d)                 = text "switch" <+> parens (prId x) <+> text "{" $$
                                          nest 2 (vpr alts $$ prDefault d) $$
                                          text "}"
    pr (CCaseL x alts d)                = text "switch" <+> parens (prId x) <+> text "{" $$
                                          nest 2 (vpr alts $$ prDefault d) $$
                                          text "}"
    pr (CSeq c1 c2)                     = pr c1 $$
                                          pr c2
    pr (CBreak)                         = text "break;"


instance Pr Bind where
    pr (BFun fs)                        = text "let {" $$
                                          nest 4 (vpr fs) $$
                                          text "}"
    pr (BVal x (Val t e))               = pr t <+> prId x <+> text "=" <+> pr e <> text ";"
    pr (BAss e e')                      = pr e <+> text ":=" <+> pr e' <> text ";"



prScope (CBreak)                        = pr CBreak
prScope (CRet x)                        = pr (CRet x)
prScope c                               = text "{" <+> pr c $$
                                          text "}"

instance Pr Alt where
    pr (Alt x c)                        = prId x <> text ":" <+> prScope c


instance Pr AltL where
    pr (AltL l c)                       = pr l <> text ": " <+> prScope c


prDefault c                             = text "default: " <+> prScope c


instance Pr Exp where
    pr (ECall (EVar x) es)              = prId x <+> parens (commasep pr es)
    pr (ECall e es)                     = parens (pr e) <> parens (commasep pr es)
    pr (ESel (EVar x) l)                = prId x <> text "." <> pr l
    pr (ESel e l)                       = parens (pr e) <> text "." <> pr l
    pr (EVar x)                         = prId x
    pr (ELit l)                         = pr l
    pr (ECast t e)                      = parens (pr t) <> pr e

    pr (ENew x)                         = text "new" <+> prId x

