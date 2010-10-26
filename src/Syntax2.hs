{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             GeneralizedNewtypeDeriving #-}

-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Syntax2 where

import Common
import Lexer
import Fixity
import PP
import Data.Binary
import Control.Monad(liftM2)

data Module = Module Name [Import] [Decl] [Decl]
            deriving  (Show)

data Import = Import Bool Name
            deriving (Show)
            
data Decl   = DKSig      Name Kind

            | DData      Name [Name] [Type] [Constr]
            | DStruct    Name [Name] [Type] [Sig]
            | DType      Name [Name] Type

            | DTClass    Name
            | DTypeClass Name [Name] [Type] [Sig]

            | DInst      Name
            | DInstance  (Maybe Name) Type [Bind]
            | DDerive    (Maybe Name) Type

            | DDflt      [(Name,Name)]
            | DDefault   [(Type,Type)]

            | DSig       [Name] Type
            | DEqn       Lhs (Rhs Exp)

            | DExtern    [Name]
            deriving  (Eq,Show)

data Constr = Constr  Name [Type] [Pred] [Quant]
            deriving (Eq,Show)

data Sig    = Sig     [Name] Type
            deriving (Eq,Show)

data Bind   = BSig    [Name] Type
            | BEqn    Lhs (Rhs Exp)
            deriving  (Eq,Show)

data Type   = TQual   Type [Pred] [Quant]
	    | TFun    [Type] Type
            | TTup    [Type]
            | TParen  Type
            | TList   Type
            | TAp     Type Type
            | TCon    Name
            | TVar    Name
            | TWild
            deriving  (Eq,Show)

data Pred   = PQual   Pred [Pred] [Quant]
            | PSub    Type Type
            | PClass  Name [Type]
            deriving (Eq,Show)
            
data Quant  = QVar    Name
            | QVarSig Name Kind
            deriving (Eq,Show)

data Lhs    = LFun    Name [Pat]
            | LPat    Pat
            deriving (Eq,Show)

data Pat    = PVar    Name
            | PVarSig Name Type
            | PCon    Name
            | PInfix  Pat Pat Pat
            | PAp     Pat Pat
            | PLit    Lit
            | PTup    [Pat]
            | PList   [Pat]
            | PParen  Pat
            | PWild
            | PStruct (Maybe (Name,Bool)) [Field]
            deriving  (Eq,Show)

data Field  = Field   Name Pat
            deriving (Eq,Show)

data Exp    = EVar    Name
            | EAp     Exp Exp
            | EInfix  Exp Exp Exp
            | ECon    Name
            | ESel    Exp Name
            | ELit    Lit
            | ETup    [Exp]
            | EParen  Exp
            | EList   [Exp]
            | ESig    Exp Type
            | EStruct (Maybe (Name,Bool)) [Bind]
            | ELam    [Pat] Exp
            | ELet    [Bind] Exp
            | ECase   Exp [Alt Exp]
            | EIf     Exp Exp Exp
            | ENeg    Exp
            | ESeq    Exp (Maybe Exp) Exp
            | EComp   Exp [Quals]

            | EAnd    Exp Exp
            | EOr     Exp Exp

            | ESectR  Exp Name
            | ESectL  Name Exp
            | ESelector Name

            | EDo     [Quals] (Maybe Name) (Maybe Name) Stmts 
            | EClass  [Quals] (Maybe Name) (Maybe Name) Stmts
            | EAct    (Maybe Name) (Maybe Exp) Stmts -- 2nd argument is the optional deadline!
            | EReq    (Maybe Name) Stmts 

           -- pre-expressions, only allowed as unshadowed subexpressions of a Stmt
            | ESVar   Name
            | ENew    [Quals] Exp
            | ESend   (Maybe Exp) Exp -- 1st argument is the optional baseline!
            | EGen    Exp
            deriving  (Eq,Show)

data Rhs a  = RExp    a [Bind]
            | RGrd    [GExp a] [Bind]
            deriving  (Eq,Show)

data GExp a = GExp    [Qual] a
            deriving (Eq,Show)
            
data Alt a  = Alt     Pat (Rhs a)
            deriving  (Eq,Show)

type Quals  = [Qual]

data Qual   = QExp    Exp
            | QGen    Pat Exp
            | QLet    [Bind]
            deriving  (Eq,Show)

type Stmts  = [Stmt]

data Stmt   = SExp    Exp
            | SRet    Exp
            | SGen    Pat Exp
            | SSig    [Name] Type
            | SEqn    Lhs (Rhs Exp)
            | SAss    Pat Exp
            | SIf     Exp Stmts [(Exp,Stmts)] (Maybe Stmts)
            | SCase   Exp [Alt Stmts]
            deriving  (Eq,Show)


-- Printing ==================================================================

-- Modules -------------------------------------------------------------------

instance Pr Module where
    pr (Module c is ds ps)      	= text "module" <+> pr c <+> text "where" $$ 
					  vpr is $$ 
					  vpr ds $$
                                  	  text "private" $$ 
					  vpr ps

instance Pr Import where
   pr (Import True n)           	= text "import" <+> pr n
   pr (Import False n)          	= text "use" <+> pr n


-- Declarations --------------------------------------------------------------

instance Pr Decl where
    pr (DKSig c k)              	= pr c <+> text "::" <+> pr k
    pr (DData c vs subs cs)      	= text "data" <+> pr c <+> hsep (map pr vs) <+> prSubs subs <+> prCons cs
    pr (DStruct c vs sups ss)		= text "struct" <+> pr c <+> hsep (map pr vs) <+> prSups sups <+> unless (null ss) (text "where") $$
                                          nest 4 (vpr ss)
    pr (DType c vs t)           	= text "type" <+> pr c <+> hsep (map pr vs) <+> text "=" <+> pr t

    pr (DTClass c)			= text "typeclass" <+> pr c
    pr (DTypeClass c vs sups ss) 	= text "typeclass" <+> pr c <+> hsep (map pr vs) <+> prSups sups <+> unless (null ss) (text "where") $$
    					  nest 4 (vpr ss)

    pr (DInst x)			= text "instance" <+> pr x
    pr (DDerive x t)			= text "deriving" <+> text "instance" <+> prWit x <+> pr t
    pr (DInstance x t bs)		= text "instance" <+> prWit x <+> pr t <+> text "where" $$
                                          nest 4 (vpr bs)

    pr (DDflt ns)			= text "default" <+> hpr ',' ns
    pr (DDefault ts)            	= text "default" <+> hpr ',' ts

    pr (DSig xs t)               	= hpr ',' xs <+> text "::" <+> pr t
    pr (DEqn lhs rhs)			= prEqn lhs (text "=") rhs

    pr (DExtern xs)             	= text "extern" <+> hpr ',' xs

unless True _			= empty
unless False p			= p

prWit Nothing			= empty
prWit (Just x)			= pr x <+> text "::"

instance Pr (Name,Name) where
    pr (x,x')			= pr x <+> text "<" <+> pr x'

instance Pr (Type,Type) where
    pr (t,t')			= pr t <+> text "<" <+> pr t'

-- Sub/supertypes -----------------------------------------------------------

prSups []                       = empty
prSups ts                       = char '<' <+> hpr ',' ts

prSubs []                       = empty
prSubs ts                       = char '>' <+> hpr ',' ts


-- Constructors and contexts -----------------------------------------------


instance Pr Constr where
  pr (Constr c ts ps qs)        = pr c <+> hsep (map (prn 3) ts) <> prPreds ps qs


prCons []                       = empty
prCons (c:cs)                   = vcat (char '=' <+> pr c : map ((char '|' <+>) . pr) cs)



-- Signatures ------------------------------------------------------------

instance Pr Sig where
    pr (Sig vs t)               = hpr ',' vs <+> text "::" <+> pr t


-- Predicates ------------------------------------------------------------

instance Pr Pred where
    pr (PQual p ps qs)		= pr p <> prPreds ps qs
    pr (PSub t t')		= pr t <+> text "<" <+> pr t'
    pr (PClass n ts)           	= pr n <+> hsep (map pr ts)

instance Pr Quant where
    pr (QVar v)                 = pr v
    pr (QVarSig v k)        	= pr v <+> text "::" <+> pr k


prPreds [] []                   = empty
prPreds ps qs                   = text " \\\\" <+> sep (punctuate comma (map pr ps ++ map pr qs))


-- Types -----------------------------------------------------------------

instance Pr Type where
    pr (TQual t ps qs)       	= pr t <> prPreds ps qs
    pr (TFun ts t) 		= hsep (intersperse (text "->") (map pr (ts++[t])))
    pr (TAp t t')            	= pr t <+> pr t'
    pr (TCon c)              	= pr c
    pr (TVar v)              	= pr v
    pr (TWild)               	= text "_"
    pr (TList t)             	= brackets (pr t)
    pr (TTup ts)             	= parens (hpr ',' ts)
    pr (TParen t)		= parens (pr t)


-- Patterns -------------------------------------------------------------

instance Pr Pat where
    pr (PAp p p')		= pr p <+> pr p'
    pr (PInfix l (PCon k) r)	= pr l <+> prOp k <+> pr r
    pr (PInfix l (PVar x) r)	= pr l <+> prOp x <+> pr r
    pr (PInfix l op r)		= pr op <+> parens (pr l) <+> parens (pr r)
    pr (PCon k)			= pr k
    pr (PVarSig v t)	        = pr v <+> text "::" <+> pr t
    pr (PVar v)     	        = pr v
    pr (PWild)              	= text "_"
    pr (PLit l)             	= pr l
    pr (PStruct Nothing fs) 	= braces (hpr ';' fs)
    pr (PStruct (Just h) fs)	= pr (fst h) <+> braces (hpr ';' fs <+> (if snd h then empty else text ".."))
    pr (PTup ps)            	= parens (hpr ',' ps)
    pr (PList ps)           	= brackets (hpr ',' ps)
    pr (PParen p)           	= parens (pr p)


-- Bindings ----------------------------------------------------------------

instance Pr Bind where
    pr (BSig xs t)              = hpr ',' xs <+> text "::" <+> pr t
    pr (BEqn lhs rhs)        	= prEqn lhs (text "=") rhs

prEqn lhs eq (RExp e bs)	= pr lhs <+> eq <+> pr e $$
				  nest 2 (prWhere bs)
prEqn lhs eq (RGrd gs bs)	= pr lhs $$
                                  nest 2 (vcat (map (prGuard eq) gs)) $$
			  	  nest 2 (prWhere bs)

prGuard eq (GExp qs e)          = char '|' <+> hpr ',' qs <+> eq <+> pr e

prWhere []			= empty
prWhere bs			= text "where" <+> vpr bs

instance Pr Lhs where
   pr (LFun v ps)               = pr v <+> hsep (map pr ps)
   pr (LPat p)                  = pr p


-- Expressions -------------------------------------------------------------

instance Pr Exp where
    pr (ELam ps e)           	= char '\\' <> hsep (map pr ps) <+> text "->" <+> pr e
    pr (ELet bs e)           	= text "let" <+> vpr bs $$ text "in" <+> pr e
    pr (EIf e e1 e2)         	= text "if" <+> pr e $$
                                    nest 3 (text "then" <+> pr e1 $$
                                            text "else" <+> pr e2)
    pr (ECase e alts)        	= text "case" <+> pr e <+> text "of" $$ nest 2 (vpr alts)
    pr (EDo qs v t ss)       	= prForall qs <+> text "do" <> prN v <> prN t $$ nest 4 (pr ss)
    pr (EClass qs v t ss)    	= prForall qs <+> text "class" <> prN v <> prN t $$ nest 4 (pr ss)
    pr (EAct v b ss)  		= prBefore b <+> text "action" <> prN v $$ nest 4 (pr ss) 
    pr (EReq v ss)           	= text "request" <> prN v $$ nest 4 (pr ss) 
    pr (EInfix l (ECon k) r)	= pr l <+> prOp k <+> pr r
    pr (EInfix l (EVar x) r)	= pr l <+> prOp x <+> pr r
    pr (EInfix l op r)		= pr op <+> parens (pr l) <+> parens (pr r)
    pr (ENew qs e)         	= prForall qs <+> text "new" <+> pr e
    pr (EGen e)             	= text "<-" <+> pr e
    pr (EAp e e')           	= pr e <+> pr e'
    pr (ESel e l)           	= pr e <> text "." <> pr l
    pr (EVar v)             	= pr v
    pr (ECon c)             	= pr c
    pr (ESVar s)		= pr s
    pr (ELit l)             	= pr l
    pr (EStruct Nothing bs) 	= braces (hpr ';' bs)
    pr (EStruct (Just h) bs) 	= pr (fst h) <+> braces (hpr ';' bs <+> (if snd h then empty else text ".."))
    pr (ENeg e)             	= text "-" <> pr e
    pr (ESig e t)           	= parens (pr e <+> text "::" <+> pr t)
    pr (ETup es)            	= parens (hpr ',' es)
    pr (ESectR e op)        	= parens (pr e <> prOp op)
    pr (ESectL op e)        	= parens (prOp op <> pr e)
    pr (ESelector l)        	= parens (text "." <> pr l)
    pr (EList es)           	= brackets (hpr ',' es)
    pr (ESeq e Nothing to)  	= brackets (pr e <> text ".." <> pr to)
    pr (ESeq e (Just b) to) 	= brackets (pr e <> comma <> pr b <> text ".." <> pr to)
    pr (EComp e qs)         	= brackets (empty <+> pr e <+> char '|' <+> hpr ',' qs)
    pr (EParen e)               = parens (prn 0 e)


prN Nothing 			= empty 
prN (Just v) 			= text "@" <> pr v

prForall []			= empty
prForall qss			= text "forall" <+> hsep (intersperse (text "|") (map pr qss))

prBefore Nothing		= empty
prBefore (Just e)		= text "before" <+> pr e

instance Pr Field where
    pr (Field l e)              = pr l <+> text "=" <+> pr e

instance Pr a => Pr (Alt a) where
    pr (Alt p rhs)              = prEqn p (text "->") rhs

instance Pr [Qual] where
    pr qs			= hpr ',' qs

instance Pr Qual where
    pr (QExp e)                 = pr e
    pr (QGen p e)               = pr p <+> text "<-" <+> pr e
    pr (QLet bs)                = text "let" <+> hpr ';' bs

-- Statements --------------------------------------------------------------

instance Pr [Stmt] where
    pr ss			= vpr ss

instance Pr Stmt where
    pr (SExp e)                 = pr e
    pr (SRet e)                 = text "result" <+> pr e
    pr (SGen p e)               = pr p <+> text "<-" <+> pr e
    pr (SSig xs t)              = hpr ',' xs <+> text "::" <+> pr t
    pr (SEqn lhs rhs)        	= prEqn lhs (text "=") rhs
    pr (SAss p e)               = pr p <+> text ":=" <+> pr e
    pr (SIf e ss elsifs ss')  	= text "if" <+> pr e <+> text "then" $$ nest 4 (pr ss) $$ vcat (map prElsif elsifs) $$ prElse ss'
    pr (SCase e alts)           = text "case" <+> pr e <+> text "of" $$ nest 4 (vpr alts)

prElsif (e,ss)			= text "elsif" <+> pr e <+> text "then" $$ nest 4 (pr ss)

prElse Nothing			= empty
prElse (Just ss)		= text "else" $$ nest 4 (pr ss)


-- Parser helpers ---------------------------------------------------------

mkModule c (is,ds,ps)           = Module c is ds ps

tFun [] t                       = t
tFun ts t                       = TFun ts t

type2cons t                     = case t of
	                            TQual t' ps qs -> t2c t' ps qs
	                            _              -> t2c t [] []
  where t2c t ps qs		= case tFlat t of
                                    (TCon c,ts) -> Constr c ts ps qs
                                    _           -> error ("Bad type constructor in " ++ render (pr t))

tFlat t                         = flat t []
  where flat (TAp t1 t2) ts     = flat t1 (t2:ts)
        flat t ts               = (t,ts)

eFlat e                         = flat e []
  where flat (EAp e e') es      = flat e (e':es)
        flat e es               = (e,es)

exp2lhs e                       = case eFlat e of
                                    (EVar v, ps) | not (null ps) -> LFun v (map exp2pat ps)
                                    _                            -> LPat (exp2pat e)

exp2pat (EVar (Name "_" 0 _ _)) = PWild
exp2pat (EVar x)        	= PVar x
exp2pat (ESig (EVar x) t) 	= PVarSig x t
exp2pat (ECon c) 		= PCon c
exp2pat (ELit l)		= PLit l
exp2pat (EAp p1 p2)		= PAp (exp2pat p1) (exp2pat p2)
exp2pat (EInfix l op r)		= PInfix (exp2pat l) (exp2pat op) (exp2pat r)
exp2pat (ENeg (ELit (LInt p i))) = PLit (LInt p (-i))
exp2pat (ENeg (ELit (LRat p r))) = PLit (LRat p (-r))
exp2pat (ETup ps)       	= PTup (map exp2pat ps)
exp2pat (EParen p)		= PParen (exp2pat p)
exp2pat (EList ps)		= PList (map exp2pat ps)
exp2pat (EStruct h bs)		= PStruct h (map bind2field bs)
  where bind2field (BEqn (LPat (PVar l)) (RExp p []))
                                = Field l (exp2pat p)
        bind2field f            = error ("Illegal struct field: " ++ render (pr f))
exp2pat p			= error ("Illegal pattern: " ++ render (pr p))




type Precedence         	= Int
data Associativity      	= LeftAss | RightAss | NonAss deriving (Eq,Show)

data Fixity             	= Fixity Associativity Precedence deriving (Eq,Show)

data OpExp              	= Nil Exp | Cons OpExp Exp Exp -- Cons l op r

fixity 				:: String -> Fixity
fixity op               	= case lookup op fixTable of
                            	    Just f  -> f
                            	    Nothing -> fixFromChars op
  where fixTable        	= [ ("$",  Fixity RightAss 0),
                                    (">>", Fixity LeftAss  1),
				    (">>=",Fixity LeftAss  1),
				    ("==", Fixity NonAss   4),
				    ("/=", Fixity NonAss   4),
				    ("<",  Fixity NonAss   4),
				    ("<=", Fixity NonAss   4),
				    (">",  Fixity NonAss   4),
				    (">=", Fixity NonAss   4),
				    (":",  Fixity RightAss 5),
				    ("++", Fixity RightAss 5),
				    ("+",  Fixity LeftAss  6),
				    ("-",  Fixity LeftAss  6),
				    ("*",  Fixity LeftAss  7),
				    ("/",  Fixity LeftAss  7),
				    ("div",Fixity LeftAss  7),
				    ("mod",Fixity LeftAss  7),
				    ("^",  Fixity RightAss 8),
				    ("@",  Fixity RightAss 9)
                          	  ]
        fixFromChars op 	= case sort (nub (intersect op "+-*/<>")) of
                            	    "<"  -> Fixity NonAss  4
                            	    ">"  -> Fixity NonAss  4
                            	    "<>" -> Fixity NonAss  4
                            	    "+"  -> Fixity LeftAss 6
                            	    "-"  -> Fixity LeftAss 6
                            	    "+-" -> Fixity LeftAss 6
                	    	    "*"  -> Fixity LeftAss 7
                	    	    "/"  -> Fixity LeftAss 7
                	    	    "*/" -> Fixity LeftAss 7
                            	    _    -> Fixity LeftAss 9

{-
Transforms a tree of infix expressions as produced by the parser 
(i.e., with all operators treated as left associative and of equal precedence) 
to a new tree reflecting operator associativity and precedence as given by
the function fixity.

Invariant: at each call to push, the second and third arguments have
the same length.
-}

transFix :: OpExp -> Exp
transFix e                          	= push e [] []
  where push (Cons l o r) (o':os) es
          | prec == prec' && (ass /= ass' || ass == NonAss)
                                    	= error ("Operator associativity ambiguity with operators " ++ render (pr o) ++ " and " ++ render (pr o'))
          | prec < prec' || (prec == prec' && ass == RightAss)
                                    	= push (Cons l o (EInfix r o' (head es))) os (tail es)
          where Fixity ass  prec    	= fixity (show o)
                Fixity ass' prec'   	= fixity (show o')
        push (Cons l o r) os es     	= push l (o:os) (r:es)
        push (Nil e) os es          	= popAll os (e:es)
         
        popAll (o:os) (e1:e2:es)    	= popAll os (EInfix e1 o e2:es)
        popAll [] es           		= head es
