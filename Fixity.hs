module Fixity where

import Common
import Syntax
import List(sort)

type Precedence         = Int
data Associativity      = LeftAss | RightAss | NonAss deriving (Eq,Show)

data Fixity             = Fixity Associativity Precedence deriving (Eq,Show)

data OpExp              = Nil Exp | Cons OpExp Name Exp 

fixity :: String -> Fixity
fixity op               = case lookup op fixTable of
                            Just f  -> f
                            Nothing -> fixFromChars op
  where fixTable        = [(":",  Fixity RightAss 5),
                           ("++", Fixity RightAss 5),
                           ("+",  Fixity LeftAss  6),
                           ("-",  Fixity LeftAss  6),
                           ("*",  Fixity LeftAss  7),
                           ("/",  Fixity LeftAss  7),
                           ("div",Fixity LeftAss  7),
                           ("mod",Fixity LeftAss  7),
--                           (".",  Fixity RightAss 9),
                           ("^",  Fixity RightAss 8),
                           ("==", Fixity NonAss   4),
                           ("/=", Fixity NonAss   4),
                           ("<",  Fixity NonAss   4),
                           ("<=", Fixity NonAss   4),
                           (">",  Fixity NonAss   4),
                           (">=", Fixity NonAss   4),
                           ("&&", Fixity RightAss 3),
                           ("||", Fixity RightAss 2),
                           (">>", Fixity LeftAss  1),
                           (">>=",Fixity LeftAss  1),
                           ("$",  Fixity RightAss 0)
                          ]
        fixFromChars op = case sort (nub (intersect op "+-*/<>")) of
                            "+"  -> Fixity LeftAss 6
                            "-"  -> Fixity LeftAss 6
                            "+-" -> Fixity LeftAss 6
                            "*"  -> Fixity LeftAss 7
                            "/"  -> Fixity LeftAss 7
                            "*/" -> Fixity LeftAss 7
                            "<"  -> Fixity NonAss  4
                            ">"  -> Fixity NonAss  4
                            "<>" -> Fixity NonAss  4
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
transFix e                          = push e [] []
  where push (Cons l o r) (o':os) es
          | prec==prec' && (ass/=ass' || ass==NonAss)
                                    = errorIds "Operator associativity ambiguity with operators" [o,o']
          | prec<prec' || (prec==prec' && ass==RightAss)
                                    = push (Cons l o (opApp r o' (head es))) os (tail es)
          where Fixity ass  prec    = fixity (show o)
                Fixity ass' prec'   = fixity (show o')
        push (Cons l o r) os es     = push l (o:os) (r:es)
        push (Nil e) os es          = popAll os (e:es)
        opApp l o r
          | s == "!"                = EIndex l r
          | s == "||"               = EAp (EAp (EVar (Prim LazyOr (annot o))) l) r
          | s == "&&"               = EAp (EAp (EVar (Prim LazyAnd (annot o))) l) r
          | otherwise               = EAp (EAp (op2exp o) l) r
          where s                   = show o
         
        popAll (o:os) (e1:e2:es)    = popAll os (opApp e1 o e2:es)
        popAll [] es                = head es
        