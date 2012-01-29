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
import PP
import Data.Binary
import Data.List(sort)
import Control.Monad(liftM2)

data Module = Module    Name2 [Import] [Decl] [Decl]
            deriving (Eq,Show)

data Import = Import    QName2
            | Use       (Maybe Name2) QName2
            deriving (Eq,Show)
            
data Decl   = DKSig      Name2 Kind

            | DData      Name2 [Quant] [Type] [Constr]
            | DStruct    Name2 [Quant] [Type] [Sig]
            | DType      Name2 [Quant] Type

            | DTClass    QName2
            | DTypeClass Name2 [Quant] [Type] [Sig]

            | DInst      QName2
            | DInstance  (Maybe Name2) Type [Bind]
            | DDerive    (Maybe Name2) Type

            | DDefault   [(QName2,QName2)]

            | DSig       [Name2] Type
            | DEqn       Pat (Rhs Exp)

            | DExtern    [Name2]
            | DExternSig [Name2] Type
            
            | DModule    Module
            deriving  (Eq,Show)

data Constr = Constr    Name2 [Type] [Quant] [Pred]
            | CInfix    Type Name2 Type [Quant] [Pred]
            deriving (Eq,Show)

data Sig    = Sig       [Name2] Type
            deriving (Eq,Show)

data Bind   = BSig      [Name2] Type
            | BEqn      Pat (Rhs Exp)
            deriving  (Eq,Show)

data Type   = TQual     Type [Quant] [Pred]
	    | TFun      [Type] Type
            | TTup      [Type]
            | TTupC     Int
            | TParen    Type
            | TList     Type
            | TAp       Type Type
            | TSig      Type Kind
            | TCon      QName2
            | TVar      Name2
            | TWild
            deriving  (Eq,Show)

data Pred   = PQual     Pred [Quant] [Pred]
            | PSub      Type Type
            | PClass    QName2 [Type]
            deriving (Eq,Show)
            
data Quant  = QVar      Name2
            | QVarSig   Name2 Kind
            deriving (Eq,Show)

data Pat    = PVar      Name2
            | PVarSig   Name2 Type
            | PCon      QName2
            | PInfix    Pat Pat Pat
            | PAp       Pat Pat
            | PPAp      Pat Pat
            | PTAp      Pat Quant
            | PIndex    Pat Exp
            | PLit      Lit
            | PTup      [Pat]
            | PTupC     Int
            | PList     [Pat]
            | PParen    Pat
            | PWild
            | PStruct   (Maybe (QName2,Bool)) [PField]
            deriving  (Eq,Show)

data PField = PField    QName2 Pat
            deriving (Eq,Show)

data Field  = Field     QName2 Exp
            deriving (Eq,Show)

data Exp    = EVar      QName2
            | EAp       Exp Exp
            | EInfix    Exp Exp Exp
            | ECon      QName2
            | ESel      Exp QName2
            | ELit      Lit
            | ETup      [Exp]
            | ETupC     Int
            | EParen    Exp
            | EList     [Exp]
            | EUpdateL  [EMap Exp]
            | EIndex    Exp Exp
            | ESig      Exp Type
            | EBStruct  (Maybe (QName2,Bool)) [Bind]
            | EStruct   (Maybe (QName2,Bool)) [Field]
            | EUpdateS  (Maybe QName2) [EMap QName2]
            | ELam      [Pat] Exp
            | ELet      [Bind] Exp
            | ECase     Exp [Alt Exp]
            | EIf       Exp Exp Exp
            | ENeg      Exp
            | ESeq      Exp (Maybe Exp) Exp
            | EComp     Exp [Quals]

            | EAnd      Exp Exp
            | EOr       Exp Exp

            | EPAp      Exp Exp
            | ETAp      Exp Type
            | EBigLam   [Quant] [Pat] Exp

            | ESectR    Exp Exp   -- operator to the right
            | ESectL    Exp Exp   -- operator to the left
            | ESectSel  QName2

            | EDo       [Quals] (Maybe QName2) (Maybe Name2) Stmts 
            | EClass    [Quals] (Maybe QName2) (Maybe Name2) Stmts
            | EAct      (Maybe Exp) (Maybe QName2) (Maybe Name2) Stmts -- 1st argument is the optional deadline!
            | EReq      (Maybe QName2) (Maybe Name2) Stmts 

           -- pre-expressions, only allowed as unshadowed subexpressions of a Stmt
            | ESVar     Name2
            | ENew      [Quals] Exp
            | ESend     [Quals] (Maybe Exp) Exp -- 2nd argument is the optional baseline!
            | EGen      Exp
            deriving  (Eq,Show)

data Rhs a  = RExp      a [Bind]
            | RGrd      [GExp a] [Bind]
            deriving  (Eq,Show)

data GExp a = GExp      [Qual] a
            deriving (Eq,Show)
            
data Alt a  = Alt       Pat (Rhs a)
            deriving  (Eq,Show)

data EMap a = EMap      a Exp
            deriving (Eq,Show)
            
type Quals  = [Qual]

data Qual   = QExp      Exp
            | QGen      Pat Exp
            | QLet      [Bind]
            deriving  (Eq,Show)

type Stmts  = [Stmt]

data Stmt   = SRes      Exp
            | SExp      Exp
            | SGen      Pat Exp
            | SSig      [Name2] Type
            | SEqn      Pat (Rhs Exp)
            | SLet      [Bind]
            | SAss      Pat Exp
            | SIf       Exp Stmts [(Exp,Stmts)] (Maybe Stmts)
            | SCase     Exp [Alt Stmts]
            deriving  (Eq,Show)


-- Printing ==================================================================

-- Modules -------------------------------------------------------------------

instance Pr Module where
  pr (Module c is ds []) | null [ m | DModule m <- ds ]
                                        = text "module" <+> pr c <+> text "where" $$ 
					  vpr is $$ 
					  vpr ds
  pr m                                  = prModule m
					  
prModule (Module c is ds [])            = text "module" <+> pr c <+> text "where" $$ 
					  nest 4 (vpr is) $$ 
					  nest 4 (vpr ds)
prModule (Module c is ds ps)      	= text "module" <+> pr c <+> text "where" $$ 
					  nest 4 (vpr is) $$ 
					  nest 4 (vpr ds) $$
                                  	  text "private" $$ 
					  nest 4 (vpr ps)

instance Pr Import where
   pr (Import n)	           	= text "import" <+> pr n
   pr (Use Nothing n)          		= text "use" <+> pr n
   pr (Use (Just n1) n2)                = text "use" <+> pr n1 <+> text "=" <+> pr n2


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

    pr (DDefault ns)            	= text "default" <+> hpr ',' ns

    pr (DSig xs t)               	= hpr ',' xs <+> text "::" <+> pr t
    pr (DEqn p rhs)			= prEqn p (text "=") rhs

    pr (DExtern xs)             	= text "extern" <+> hpr ',' xs
    pr (DExternSig xs t)             	= text "extern" <+> hpr ',' xs <+> text "::" <+> pr t
    
    pr (DModule m)                      = prModule m


unless True _			= empty
unless False p			= p

prWit Nothing			= empty
prWit (Just x)			= pr x <+> text "::"

instance Pr (QName2,QName2) where
    pr (x,x')			= pr x <+> text "<" <+> pr x'


-- Sub/supertypes -----------------------------------------------------------

prSups []                       = empty
prSups ts                       = char '<' <+> hpr ',' ts

prSubs []                       = empty
prSubs ts                       = char '>' <+> hpr ',' ts


-- Constructors and contexts -----------------------------------------------


instance Pr Constr where
  pr (Constr c ts qs ps)        = pr c <+> hsep (map (prn 3) ts) <+> prPreds qs ps
  pr (CInfix t1 c t2 qs ps)	= pr t1 <+> pr c <+> pr t2 <+> prPreds qs ps


prCons []                       = empty
prCons (c:cs)                   = vcat (char '=' <+> pr c : map ((char '|' <+>) . pr) cs)



-- Signatures ------------------------------------------------------------

instance Pr Sig where
    pr (Sig vs t)               = commasep prId2 vs <+> text "::" <+> pr t


-- Predicates ------------------------------------------------------------

instance Pr Pred where
    pr (PQual p qs ps)		= pr p <+> prPreds qs ps
    pr (PSub t t')		= pr t <+> text "<" <+> pr t'
    pr (PClass n ts)           	= pr n <+> hsep (map pr ts)

instance Pr Quant where
    pr (QVar v)                 = pr v
    pr (QVarSig v k)        	= pr v <+> text "::" <+> pr k


prPreds [] []                   = empty
prPreds qs ps                   = text "\\\\" <+> sep (punctuate comma (map pr qs ++ map pr ps))


-- Types -----------------------------------------------------------------

instance Pr Type where
    pr (TQual t qs ps)       	= pr t <+> prPreds qs ps
    pr (TFun ts t) 		= hsep (intersperse (text "->") (map pr (ts++[t])))
    pr (TAp t t')            	= pr t <+> pr t'
    pr (TCon c)              	= pr c
    pr (TVar v)              	= pr v
    pr (TWild)               	= text "_"
    pr (TList t)             	= brackets (pr t)
    pr (TSig t k)               = pr t <+> text "::" <+> pr k
    pr (TTup ts)             	= parens (hpr ',' ts)
    pr (TTupC n)             	= parens (text (replicate n ','))
    pr (TParen t)		= parens (pr t)


-- Patterns -------------------------------------------------------------

instance Pr Pat where
    pr (PAp p p')		= pr p <+> pr p'
    pr (PPAp p p')		= pr p <+> text "@" <> pr p'
    pr (PTAp p q)		= pr p <+> text "@" <> braces (pr q)
    pr (PInfix l op r)		= parens (pr l) <+> prOpPat op <+> parens (pr r)
    pr (PIndex p p')            = pr p <> text "!" <> pr p'
    pr (PCon k)			= pr k
    pr (PVarSig v t)	        = pr v <+> text "::" <+> pr t
    pr (PVar v)     	        = pr v
    pr (PWild)              	= text "_"
    pr (PLit l)             	= pr l
    pr (PStruct Nothing fs) 	= braces (hpr ';' fs)
    pr (PStruct (Just h) fs)	= pr (fst h) <+> braces (hpr ';' fs <+> (if snd h then empty else text ".."))
    pr (PTup ps)            	= parens (hpr ',' ps)
    pr (PTupC n)            	= parens (text (replicate n ','))
    pr (PList ps)           	= brackets (hpr ',' ps)
    pr (PParen p)           	= parens (pr p)

prOpPat (PVar x) | isSym2 x	= prOp2 x
prOpPat (PCon x) | isQSym2 x    = prQOp2 x
prOpPat p			= text "`" <> pr p <> text "`"

-- Bindings ----------------------------------------------------------------

instance Pr Bind where
    pr (BSig xs t)              = hpr ',' xs <+> text "::" <+> pr t
    pr (BEqn p rhs)        	= prEqn p (text "=") rhs

prEqn p eq (RExp e bs)		= pr p <+> eq <+> pr e $$
				  nest 2 (prWhere bs)
prEqn p eq (RGrd gs bs)		= pr p $$
                                  nest 2 (vcat (map (prGuard eq) gs)) $$
			  	  nest 2 (prWhere bs)

prGuard eq (GExp qs e)          = char '|' <+> hpr ',' qs <+> eq <+> pr e

prWhere []			= empty
prWhere bs			= text "where" <+> vpr bs


-- Expressions -------------------------------------------------------------

instance Pr Exp where
    pr (ELam ps e)           	= char '\\' <> hsep (map pr ps) <+> text "->" <+> pr e
    pr (EBigLam qs ps e)        = text "\\\\" <> hsep (map (braces . pr) qs) <+> hsep (map pr ps) <+> text "->" <+> pr e
    pr (ELet bs e)           	= text "let" <+> vpr bs $$ text "in" <+> pr e
    pr (EIf e e1 e2)         	= text "if" <+> pr e $$
                                    nest 3 (text "then" <+> pr e1 $$
                                            text "else" <+> pr e2)
    pr (ECase e alts)        	= text "case" <+> pr e <+> text "of" $$ nest 2 (vpr alts)
    pr (EDo qs v t ss)       	= prForall qs <+> text "do" <> prN v <> prN t $$ nest 3 (pr ss)
    pr (EClass qs v t ss)    	= prForall qs <+> text "class" <> prN v <> prN t $$ nest 4 (pr ss)
    pr (EAct b v t ss)  	= prBefore b <+> text "action" <> prN v <> prN t $$ nest 4 (pr ss) 
    pr (EReq v t ss)           	= text "request" <> prN v <> prN t $$ nest 4 (pr ss) 
    pr (EInfix l op r)		= parens (pr l) <+> prOpExp op <+> parens (pr r)
    pr (EIndex e e')            = pr e <> text "!" <> pr e'
    pr (EOr e1 e2)              = pr e1 <+> text "||" <+> pr e2
    pr (EAnd e1 e2)		= pr e1 <+> text "&&" <+> pr e2
    pr (ENew qs e)         	= prForall qs <+> text "new" <+> pr e
    pr (EGen e)             	= text "<-" <+> pr e
    pr (ESend qs b e)           = prForall qs <+> prAfter b <+> text "send" <+> pr e
    pr (EAp e e')           	= pr e <+> pr e'
    pr (EPAp e e')           	= pr e <+> text "@" <> pr e'
    pr (ETAp e t)           	= pr e <+> text "@" <> braces (pr t)
    pr (ESel e l)           	= pr e <> text "." <> pr l
    pr (EVar v)             	= pr v
    pr (ECon c)             	= pr c
    pr (ESVar s)		= pr s
    pr (ELit l)             	= pr l
    pr (EStruct Nothing fs)     = braces (hpr ',' fs)
    pr (EStruct (Just h) fs)    = pr (fst h) <+> braces (hpr ',' fs <+> (if snd h then empty else text ".."))
    pr (EUpdateS Nothing fs)    = braces (hpr ',' fs)
    pr (EUpdateS (Just c) fs)   = pr c <+> braces (hpr ',' fs)
    pr (EBStruct Nothing bs)    = braces (hpr ';' bs)
    pr (EBStruct (Just h) bs)   = pr (fst h) <+> braces (hpr ';' bs <+> (if snd h then empty else text ".."))
    pr (ENeg e)             	= text "-" <> pr e
    pr (ESig e t)           	= parens (pr e <+> text "::" <+> pr t)
    pr (ETup es)            	= parens (hpr ',' es)
    pr (ETupC n)            	= parens (text (replicate n ','))
    pr (ESectR e op)        	= parens (pr e <> prOpExp op)
    pr (ESectL op e)        	= parens (prOpExp op <> pr e)
    pr (ESectSel l)        	= parens (text "." <> pr l)
    pr (EList es)           	= brackets (hpr ',' es)
    pr (ESeq e Nothing to)  	= brackets (pr e <+> text ".." <+> pr to)
    pr (ESeq e (Just b) to) 	= brackets (pr e <> comma <> pr b <+> text ".." <+> pr to)
    pr (EComp e qs)         	= brackets (empty <+> pr e <+> char '|' <+> hpr ',' qs)
    pr (EUpdateL lmap)          = brackets (hpr ',' lmap)
    pr (EParen e)               = parens (prn 0 e)

instance Pr Field where
    pr (Field l e)              = pr l <+> text "=" <+> pr e
    
instance Pr a => Pr (EMap a) where
    pr (EMap a e)               = pr a <+> text "->" <+> pr e

prOpExp (EVar x) | isQSym2 x	= prQOp2 x
prOpExp (ECon x) | isQSym2 x    = prQOp2 x
prOpExp e			= text "`" <> pr e <> text "`"

prN Nothing 			= empty 
prN (Just v) 			= text "@" <> pr v

prForall []			= empty
prForall qss			= text "forall" <+> hsep (intersperse (text "|") (map pr qss))

prBefore Nothing		= empty
prBefore (Just e)		= text "before" <+> pr e

prAfter Nothing	        	= empty
prAfter (Just e)		= text "after" <+> pr e

instance Pr PField where
    pr (PField l e)             = pr l <+> text "=" <+> pr e

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
    pr (SRes e)                 = text "result" <+> pr e
    pr (SExp e)                 = pr e
    pr (SGen p e)               = pr p <+> text "<-" <+> pr e
    pr (SSig xs t)              = hpr ',' xs <+> text "::" <+> pr t
    pr (SEqn p rhs)        	= prEqn p (text "=") rhs
    pr (SLet bs)                = text "let" <+> vpr bs
    pr (SAss p e)               = pr p <+> text ":=" <+> pr e
    pr (SIf e ss elsifs ss')  	= text "if" <+> pr e <+> text "then" $$ nest 4 (pr ss) $$ vcat (map prElsif elsifs) $$ prElse ss'
    pr (SCase e alts)           = text "case" <+> pr e <+> text "of" $$ nest 4 (vpr alts)

prElsif (e,ss)			= text "elsif" <+> pr e <+> text "then" $$ nest 4 (pr ss)

prElse Nothing			= empty
prElse (Just ss)		= text "else" $$ nest 4 (pr ss)

-- Binary instance declarations -------------------------------------------

instance Binary Module where
  put (Module a b c d) = putWord8 0 >> put a >> put b >> put c >> put d
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Module a b c d)

instance Binary Import where
  put (Use a b) = putWord8 0 >> put a >> put b
  put (Import a) = putWord8 1 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Use a b)
      1 -> get >>= \a -> return (Import a)

instance Binary Decl where
  put (DExternSig a b) = putWord8 0 >> put a >> put b
  put (DExtern a) = putWord8 1 >> put a
  put (DEqn a b) = putWord8 2 >> put a >> put b
  put (DSig a b) = putWord8 3 >> put a >> put b
  put (DDefault a) = putWord8 4 >> put a
  put (DDerive a b) = putWord8 5 >> put a >> put b
  put (DInstance a b c) = putWord8 6 >> put a >> put b >> put c
  put (DInst a) = putWord8 7 >> put a
  put (DTypeClass a b c d) = putWord8 8 >> put a >> put b >> put c >> put d
  put (DTClass a) = putWord8 9 >> put a
  put (DType a b c) = putWord8 10 >> put a >> put b >> put c
  put (DStruct a b c d) = putWord8 11 >> put a >> put b >> put c >> put d
  put (DData a b c d) = putWord8 12 >> put a >> put b >> put c >> put d
  put (DKSig a b) = putWord8 13 >> put a >> put b
  put (DModule a) = putWord8 14 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (DExternSig a b)
      1 -> get >>= \a -> return (DExtern a)
      2 -> get >>= \a -> get >>= \b -> return (DEqn a b)
      3 -> get >>= \a -> get >>= \b -> return (DSig a b)
      4 -> get >>= \a -> return (DDefault a)
      5 -> get >>= \a -> get >>= \b -> return (DDerive a b)
      6 -> get >>= \a -> get >>= \b -> get >>= \c -> return (DInstance a b c)
      7 -> get >>= \a -> return (DInst a)
      8 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (DTypeClass a b c d)
      9 -> get >>= \a -> return (DTClass a)
      10 -> get >>= \a -> get >>= \b -> get >>= \c -> return (DType a b c)
      11 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (DStruct a b c d)
      12 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (DData a b c d)
      13 -> get >>= \a -> get >>= \b -> return (DKSig a b)
      14 -> get >>= \a -> return (DModule a)

instance Binary Constr where
  put (CInfix a b c d e) = putWord8 0 >> put a >> put b >> put c >> put d >> put e
  put (Constr a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (CInfix a b c d e)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Constr a b c d)

instance Binary Sig where
  put (Sig a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Sig a b)

instance Binary Bind where
  put (BEqn a b) = putWord8 0 >> put a >> put b
  put (BSig a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (BEqn a b)
      1 -> get >>= \a -> get >>= \b -> return (BSig a b)

instance Binary Type where
  put (TWild) = putWord8 0
  put (TVar a) = putWord8 1 >> put a
  put (TCon a) = putWord8 2 >> put a
  put (TAp a b) = putWord8 3 >> put a >> put b
  put (TList a) = putWord8 4 >> put a
  put (TParen a) = putWord8 5 >> put a
  put (TTup a) = putWord8 6 >> put a
  put (TTupC a) = putWord8 7 >> put a
  put (TFun a b) = putWord8 8 >> put a >> put b
  put (TQual a b c) = putWord8 9 >> put a >> put b >> put c
  put (TSig a b) = putWord8 10 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return (TWild)
      1 -> get >>= \a -> return (TVar a)
      2 -> get >>= \a -> return (TCon a)
      3 -> get >>= \a -> get >>= \b -> return (TAp a b)
      4 -> get >>= \a -> return (TList a)
      5 -> get >>= \a -> return (TParen a)
      6 -> get >>= \a -> return (TTup a)
      7 -> get >>= \a -> return (TTupC a)
      8 -> get >>= \a -> get >>= \b -> return (TFun a b)
      9 -> get >>= \a -> get >>= \b -> get >>= \c -> return (TQual a b c)
      10 -> get >>= \a -> get >>= \b -> return (TSig a b)

instance Binary Pred where
  put (PClass a b) = putWord8 0 >> put a >> put b
  put (PSub a b) = putWord8 1 >> put a >> put b
  put (PQual a b c) = putWord8 2 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (PClass a b)
      1 -> get >>= \a -> get >>= \b -> return (PSub a b)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> return (PQual a b c)

instance Binary Quant where
  put (QVarSig a b) = putWord8 0 >> put a >> put b
  put (QVar a) = putWord8 1 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (QVarSig a b)
      1 -> get >>= \a -> return (QVar a)

instance Binary Pat where
  put (PStruct a b) = putWord8 0 >> put a >> put b
  put (PWild) = putWord8 1
  put (PParen a) = putWord8 2 >> put a
  put (PList a) = putWord8 3 >> put a
  put (PTup a) = putWord8 4 >> put a
  put (PTupC a) = putWord8 5 >> put a
  put (PLit a) = putWord8 6 >> put a
  put (PAp a b) = putWord8 7 >> put a >> put b
  put (PPAp a b) = putWord8 8 >> put a >> put b
  put (PTAp a b) = putWord8 9 >> put a >> put b
  put (PInfix a b c) = putWord8 10 >> put a >> put b >> put c
  put (PCon a) = putWord8 11 >> put a
  put (PVarSig a b) = putWord8 12 >> put a >> put b
  put (PVar a) = putWord8 13 >> put a
  put (PIndex a b) = putWord8 14 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (PStruct a b)
      1 -> return (PWild)
      2 -> get >>= \a -> return (PParen a)
      3 -> get >>= \a -> return (PList a)
      4 -> get >>= \a -> return (PTup a)
      5 -> get >>= \a -> return (PTupC a)
      6 -> get >>= \a -> return (PLit a)
      7 -> get >>= \a -> get >>= \b -> return (PAp a b)
      8 -> get >>= \a -> get >>= \b -> return (PPAp a b)
      9 -> get >>= \a -> get >>= \b -> return (PTAp a b)
      10 -> get >>= \a -> get >>= \b -> get >>= \c -> return (PInfix a b c)
      11 -> get >>= \a -> return (PCon a)
      12 -> get >>= \a -> get >>= \b -> return (PVarSig a b)
      13 -> get >>= \a -> return (PVar a)
      14 -> get >>= \a -> get >>= \b -> return (PIndex a b)

instance Binary PField where
  put (PField a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (PField a b)

instance Binary Field where
  put (Field a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Field a b)

instance Binary Exp where
  put (EGen a) = putWord8 0 >> put a
  put (ESend a b c) = putWord8 1 >> put a >> put b >> put c
  put (ENew a b) = putWord8 2 >> put a >> put b
  put (ESVar a) = putWord8 3 >> put a
  put (EReq a b c) = putWord8 4 >> put a >> put b >> put c
  put (EAct a b c d) = putWord8 5 >> put a >> put b >> put c >> put d
  put (EClass a b c d) = putWord8 6 >> put a >> put b >> put c >> put d
  put (EDo a b c d) = putWord8 7 >> put a >> put b >> put c >> put d
  put (ESectSel a) = putWord8 8 >> put a
  put (ESectL a b) = putWord8 9 >> put a >> put b
  put (ESectR a b) = putWord8 10 >> put a >> put b
  put (EOr a b) = putWord8 11 >> put a >> put b
  put (EAnd a b) = putWord8 12 >> put a >> put b
  put (EComp a b) = putWord8 13 >> put a >> put b
  put (ESeq a b c) = putWord8 14 >> put a >> put b >> put c
  put (ENeg a) = putWord8 15 >> put a
  put (EIf a b c) = putWord8 16 >> put a >> put b >> put c
  put (ECase a b) = putWord8 17 >> put a >> put b
  put (ELet a b) = putWord8 18 >> put a >> put b
  put (ELam a b) = putWord8 19 >> put a >> put b
  put (EStruct a b) = putWord8 20 >> put a >> put b
  put (EUpdateS a b) = putWord8 21 >> put a >> put b
  put (EBStruct a b) = putWord8 22 >> put a >> put b
  put (ESig a b) = putWord8 23 >> put a >> put b
  put (EList a) = putWord8 24 >> put a
  put (EParen a) = putWord8 25 >> put a
  put (ETup a) = putWord8 26 >> put a
  put (ETupC a) = putWord8 27 >> put a
  put (ELit a) = putWord8 28 >> put a
  put (ESel a b) = putWord8 29 >> put a >> put b
  put (ECon a) = putWord8 30 >> put a
  put (EInfix a b c) = putWord8 31 >> put a >> put b >> put c
  put (EAp a b) = putWord8 32 >> put a >> put b
  put (EVar a) = putWord8 33 >> put a
  put (EUpdateL a) = putWord8 34 >> put a
  put (EIndex a b) = putWord8 35 >> put a >> put b
  put (EPAp a b) = putWord8 36 >> put a >> put b
  put (ETAp a b) = putWord8 37 >> put a >> put b
  put (EBigLam a b c) = putWord8 38 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (EGen a)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ESend a b c)
      2 -> get >>= \a -> get >>= \b -> return (ENew a b)
      3 -> get >>= \a -> return (ESVar a)
      4 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EReq a b c)
      5 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (EAct a b c d)
      6 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (EClass a b c d)
      7 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (EDo a b c d)
      8 -> get >>= \a -> return (ESectSel a)
      9 -> get >>= \a -> get >>= \b -> return (ESectL a b)
      10 -> get >>= \a -> get >>= \b -> return (ESectR a b)
      11 -> get >>= \a -> get >>= \b -> return (EOr a b)
      12 -> get >>= \a -> get >>= \b -> return (EAnd a b)
      13 -> get >>= \a -> get >>= \b -> return (EComp a b)
      14 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ESeq a b c)
      15 -> get >>= \a -> return (ENeg a)
      16 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EIf a b c)
      17 -> get >>= \a -> get >>= \b -> return (ECase a b)
      18 -> get >>= \a -> get >>= \b -> return (ELet a b)
      19 -> get >>= \a -> get >>= \b -> return (ELam a b)
      20 -> get >>= \a -> get >>= \b -> return (EStruct a b)
      21 -> get >>= \a -> get >>= \b -> return (EUpdateS a b)
      22 -> get >>= \a -> get >>= \b -> return (EBStruct a b)
      23 -> get >>= \a -> get >>= \b -> return (ESig a b)
      24 -> get >>= \a -> return (EList a)
      25 -> get >>= \a -> return (EParen a)
      26 -> get >>= \a -> return (ETup a)
      27 -> get >>= \a -> return (ETupC a)
      28 -> get >>= \a -> return (ELit a)
      29 -> get >>= \a -> get >>= \b -> return (ESel a b)
      30 -> get >>= \a -> return (ECon a)
      31 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EInfix a b c)
      32 -> get >>= \a -> get >>= \b -> return (EAp a b)
      33 -> get >>= \a -> return (EVar a)
      34 -> get >>= \a -> return (EUpdateL a)
      35 -> get >>= \a -> get >>= \b -> return (EIndex a b)
      36 -> get >>= \a -> get >>= \b -> return (EPAp a b)
      37 -> get >>= \a -> get >>= \b -> return (ETAp a b)
      38 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EBigLam a b c)

instance (Binary a) => Binary (Rhs a) where
  put (RGrd a b) = putWord8 0 >> put a >> put b
  put (RExp a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (RGrd a b)
      1 -> get >>= \a -> get >>= \b -> return (RExp a b)

instance (Binary a) => Binary (GExp a) where
  put (GExp a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (GExp a b)

instance (Binary a) => Binary (Alt a) where
  put (Alt a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Alt a b)

instance (Binary a) => Binary (EMap a) where
  put (EMap a b) = putWord8 0 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (EMap a b)
      
instance Binary Qual where
  put (QLet a) = putWord8 0 >> put a
  put (QGen a b) = putWord8 1 >> put a >> put b
  put (QExp a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (QLet a)
      1 -> get >>= \a -> get >>= \b -> return (QGen a b)
      2 -> get >>= \a -> return (QExp a)

instance Binary Stmt where
  put (SCase a b) = putWord8 0 >> put a >> put b
  put (SIf a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  put (SAss a b) = putWord8 2 >> put a >> put b
  put (SEqn a b) = putWord8 3 >> put a >> put b
  put (SSig a b) = putWord8 4 >> put a >> put b
  put (SLet a) = putWord8 5 >> put a
  put (SGen a b) = putWord8 6 >> put a >> put b
  put (SExp a) = putWord8 7 >> put a
  put (SRes a) = putWord8 8 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (SCase a b)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (SIf a b c d)
      2 -> get >>= \a -> get >>= \b -> return (SAss a b)
      3 -> get >>= \a -> get >>= \b -> return (SEqn a b)
      4 -> get >>= \a -> get >>= \b -> return (SSig a b)
      5 -> get >>= \a -> return (SLet a)
      6 -> get >>= \a -> get >>= \b -> return (SGen a b)
      7 -> get >>= \a -> return (SExp a)
      8 -> get >>= \a -> return (SRes a)


-- Parser helpers ---------------------------------------------------------

tFun [] t                       = t
tFun ts t                       = TFun ts t

type2cons t                     = case t of
	                            TQual t' qs ps -> t2c t' qs ps
	                            _              -> t2c t [] []
  where t2c t qs ps		        = case tFlat t of
                                    (TCon (Q [] c),ts) -> Constr c ts qs ps
                                    _           -> error ("Bad constructor in " ++ render (pr t))

tFlat t                         = flat t []
  where flat (TAp t1 t2) ts     = flat t1 (t2:ts)
        flat t ts               = (t,ts)

exp2pat (EVar (Q [] (Plain "_" _))) = PWild
exp2pat (EVar (Q [] x))             = PVar x
exp2pat (ESig (EVar (Q [] x)) t)    = PVarSig x t
exp2pat (ECon c) 		    = PCon c
exp2pat (ELit l)		    = PLit l
exp2pat (EAp p1 p2)		    = PAp (exp2pat p1) (exp2pat p2)
exp2pat (EPAp p1 p2)		    = PPAp (exp2pat p1) (exp2pat p2)
exp2pat (ETAp p (TVar v))	    = PTAp (exp2pat p) (QVar v)
exp2pat (ETAp p (TSig (TVar v) k))  = PTAp (exp2pat p) (QVarSig v k)
exp2pat (EIndex p e)                = PIndex (exp2pat p) e
exp2pat (EInfix l op r)		    = PInfix (exp2pat l) (exp2pat op) (exp2pat r)
exp2pat (ENeg (ELit (LInt p i)))    = PLit (LInt p (-i))
exp2pat (ENeg (ELit (LRat p r)))    = PLit (LRat p (-r))
exp2pat (ETup ps)       	    = PTup (map exp2pat ps)
exp2pat (ETupC n)       	    = PTupC n
exp2pat (EParen p)		    = PParen (exp2pat p)
exp2pat (EList ps)		    = PList (map exp2pat ps)
exp2pat (EStruct h fs)              = PStruct h [ PField l (exp2pat e) | Field l e <- fs ]
exp2pat (EBStruct h bs)	            = PStruct h (map bind2field bs)
  where bind2field (BEqn (PVar l) (RExp p []))
                                    = PField (noqual l) (exp2pat p)
        bind2field f                = error ("Illegal struct field: " ++ render (pr f))
exp2pat p			                = error ("Illegal pattern: " ++ render (pr p))




type Precedence         	= Int
data Associativity      	= LeftAss | RightAss | NonAss deriving (Eq,Show)

data Fixity             	= Fixity Associativity Precedence deriving (Eq,Show)

data OpExp              	= Nil Exp | Cons OpExp Exp Exp -- Cons l op r
                                deriving (Show, Eq)

fixity (ECon x)			= fixity' x
fixity (EVar x)               	= fixity' x
fixity _			= Fixity LeftAss 9
	
fixity' n			= case lookup str fixTable of
                            	    Just f  -> f
                            	    Nothing -> fixFromChars str
  where str			= show (basenameOf n)
	fixTable        	= [ ("$",  Fixity RightAss 2),
                                    (">>", Fixity LeftAss  3),
				    (">>=",Fixity LeftAss  3),
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
				    ("^",  Fixity RightAss 8)
                          	  ]
        fixFromChars op 	= case sort (nub (intersect str "+-*/<>")) of
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
                                    	= error ("Operator associativity ambiguity with operators " ++ 
                                    	         render (pr o) ++ " and " ++ render (pr o'))
          | prec < prec' || (prec == prec' && ass == RightAss)
                                    	= push (Cons l o (EInfix r o' (head es))) os (tail es)
          where Fixity ass  prec    	= fixity o
                Fixity ass' prec'   	= fixity o'
        push (Cons l o r) os es     	= push l (o:os) (r:es)
        push (Nil e) os es          	= popAll os (e:es)
         
        popAll (o:os) (e1:e2:es)    	= popAll os (EInfix e1 o e2:es)
        popAll [] es           		= head es
