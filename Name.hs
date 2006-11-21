module Name where

import PP
import Token
import Char

-- The type of names ---------------------------------------------------------------------

data Name                       = Name  { str :: String, tag :: Int, annot :: Annot }
                                | Prim  { con :: Prim, annot :: Annot }
                                | Tuple { width :: Int, annot :: Annot }


data Annot                      = Annot { location :: Maybe (Int,Int), explicit :: Bool }


-- The built-in primitives ----------------------------------------------------------------

data Prim                       = 

                                -- Constructor identifiers

                                  Action                -- Types
                                | Request
                                | Template
                                | Cmd
                                | O

                                | Message
                                | Ref
                                | PID
                                | PMC

                                | Int
                                | Float
                                | Char
                                | Bool

                                -- Constructor symbols (special syntax)

                                | LIST                  -- Type
                                | UNIT                  -- Type,term
                                | NIL                   -- Term
                                | CONS                  -- Term

                                -- Constructor identifiers (special syntax)

                                | FALSE                 -- Terms
                                | TRUE

                                -- Variable identifiers

                                | Refl                  -- Terms

                                | ActToCmd
                                | ActToO
                                | ReqToCmd
                                | ReqToO
                                | TemplToCmd
                                | TemplToO
                                | CmdToO
                                | RefToPID

                                | IntPlus
                                | IntMinus
                                | IntTimes
                                | IntDiv
                                | IntMod
                                | IntNeg

                                | IntEQ
                                | IntNE
                                | IntLT
                                | IntLE
                                | IntGE
                                | IntGT
                                
                                | FloatPlus
                                | FloatMinus
                                | FloatTimes
                                | FloatDiv
                                | FloatNeg
                                
                                | FloatEQ
                                | FloatNE
                                | FloatLT
                                | FloatLE
                                | FloatGE
                                | FloatGT

                                | CharToInt
                                | IntToChar

                                | MsgEQ
                                | MsgNE

                                | PidEQ
                                | PidNE

                                | Fail
                                | Commit
                                | Match
                                | Fatbar

                                | After
                                | Before

                                | Current               -- special syntax, not global
                                deriving (Eq,Ord,Enum,Bounded,Show)

isConPrim p                     = p < Refl

isIdPrim p                      = p `notElem` primSyms

primSyms                        = [LIST .. CONS]

primTypes                       = map primKeyValue [Action .. UNIT]

primTerms                       = map primKeyValue [UNIT .. Before]

primCurrent                     = [primKeyValue Current]

primKeyValue p                  = (name0 (strRep p), prim p)


strRep LIST                     = "[]"
strRep UNIT                     = "()"
strRep NIL                      = "[]"
strRep CONS                     = ":"
strRep TRUE                     = "True"
strRep FALSE                    = "False"
strRep Current                  = "current"
strRep p | isConPrim p          = show p
         | otherwise            = "prim" ++ show p
                                


-- Name construction ------------------------------------------------------------

noAnnot                         = Annot { location = Nothing, explicit = False }

loc l                           = noAnnot { location = Just l }


name l s                        = Name s 0 (loc l)

name0 s                         = Name s 0 noAnnot

prim p                          = Prim p noAnnot

tuple n                         = Tuple n noAnnot


annotExplicit n                 = n { annot = a { explicit = True } }
  where a                       = annot n


-- Testing Names ----------------------------------------------------------------

isId (Name s _ _)               = isIdent (head s)
isId (Tuple _ _)                = False
isId (Prim p _)                 = isIdPrim p

isSym i                         = not (isId i)


isCon (Name (c:_) _ _)          = isIdent c && isUpper c || c == ':'
isCon (Tuple _ _)               = True
isCon (Prim p _)                = isConPrim p

isVar i                         = not (isCon i)

isGenerated (Name _ _ a)        = location a == Nothing
isGenerated _                   = False


-- var <-> sel --------------------------------------------------------------------

mkSel (Name s n a)              = Name ('.':s) n noAnnot
mkSel n                         = n

chopSel (Name ('.':l) n a)      = Name l n noAnnot
chopSel n                       = n


-- Equality & Order ----------------------------------------------------------------

instance Eq Name where
  Name a 0 _ == Name b 0 _      = a == b
  Name _ a _ == Name _ b _      = a == b
  Tuple a _  == Tuple b _       = a == b
  Prim a _   == Prim b _        = a == b
  _          == _               = False

instance Ord Name where
  Prim a _   <= Prim b _        = a <= b
  Prim _ _   <= _               = True
  Tuple a _  <= Tuple b _       = a <= b
  Tuple _ _  <= Name _ _ _      = True
  Name a 0 _ <= Name b 0 _      = a <= b
  Name _ a _ <= Name _ b _      = a <= b
  _          <= _               = False


-- Printing Names -----------------------------------------------------------------

instance Show Name where
  show (Name s 0 _)             = s
  show (Name s n _)             = s ++ "_" ++ show n
  show (Tuple n _)              = '(' : replicate (n-1) ',' ++ ")"
  show (Prim p _)               = strRep p


instance Pr Name where
  pr x                          = text (show x)


prId i                          = if isSym i then parens (pr i) else pr i

prOp i                          = if isSym i then pr i else backQuotes (pr i)




packName n                      = show n

unpackName x                    = case break (=='_') x of
                                    (s,"") -> name0 s
                                    (s,n)  -> (name0 s) { tag = read n }
