module Name where

import PP
import Token
import Char

-- The type of names ---------------------------------------------------------------------

data Name                       = Name  { str :: String, tag :: Int, annot :: Annot }
                                | Prim  { con :: Prim, annot :: Annot }
                                | Tuple { width :: Int, annot :: Annot }


data Annot                      = Annot { location :: Maybe (Int,Int), explicit :: Bool, stateVar :: Bool }


-- The built-in primitives ----------------------------------------------------------------

data Prim                       = 

                                -- Constructor identifiers

                                  Action                -- Types
                                | Request
                                | Template
                                | Cmd

                                | Msg
                                | Ref
                                | PID
                                | PMC
                                | Time

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
                                | ReqToCmd
                                | TemplToCmd
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
                                
                                | Sec
                                | MilliSec
                                | MicroSec
                                | NanoSec
                                | Infinity
                                
                                | TimePlus
                                | TimeMinus
                                | TimeMin
                                
                                | TimeEQ
                                | TimeNE
                                | TimeLT
                                | TimeLE
                                | TimeGE
                                | TimeGT

                                | Fail
                                | Commit
                                | Match
                                | Fatbar

                                | After
                                | Before

-- invisible ------------------------------------------------------------------------------------

                                | ASYNC                 -- RTS entry points
                                | LOCK
                                | UNLOCK
                                | NEW
                                
                                | Code                  -- selectors of struct Msg
                                | Baseline
                                | Deadline
                                
                                | Box                   -- The Kindle Struct for boxed values
                                | Value                 -- selector of struct Box
                                
                                | LISTtags              -- The Kindle Enum for built-in LIST tags
                                deriving (Eq,Ord,Enum,Bounded,Show)

isConPrim p                     = p <= TRUE

invisible p                     = p >= ASYNC

isIdPrim p                      = p `notElem` primSyms

primSyms                        = [LIST, NIL, CONS]

primTypes                       = map primKeyValue [Action .. UNIT]

primTerms                       = map primKeyValue [UNIT .. Before]

primKeyValue p                  = (name0 (strRep p), prim p)


strRep LIST                     = "[]"
strRep UNIT                     = "()"
strRep NIL                      = "[]"
strRep CONS                     = ":"
strRep TRUE                     = "True"
strRep FALSE                    = "False"
strRep Sec                      = "sec"
strRep MilliSec                 = "millisec"
strRep MicroSec                 = "microsec"
strRep NanoSec                  = "nanosec"
strRep Infinity                 = "infinity"
strRep p                        = strRep2 p
                                
strRep2 p
  | isConPrim p || invisible p  = show p
  | otherwise                   = "prim" ++ show p


-- Name construction ------------------------------------------------------------

noAnnot                         = Annot { location = Nothing, explicit = False, stateVar = False }

loc l                           = noAnnot { location = Just l }


name l s                        = Name s 0 (loc l)

name0 s                         = Name s 0 noAnnot

prim p                          = Prim p noAnnot

tuple n                         = Tuple n noAnnot

isTuple (Tuple _ _)             = True
isTuple _                       = False

annotExplicit n                 = n { annot = a { explicit = True } }
  where a                       = annot n

annotState n                    = n { annot = a { stateVar = True } }
  where a                       = annot n


-- Testing Names ----------------------------------------------------------------

isId (Name s _ _)               = isIdent (head s)
isId (Tuple _ _)                = True
isId (Prim p _)                 = isIdPrim p

isSym i                         = not (isId i)


isCon (Name (c:_) _ _)          = isIdent c && isUpper c || c == ':'
isCon (Tuple _ _)               = True
isCon (Prim p _)                = isConPrim p

isVar i                         = not (isCon i)

isGenerated (Name _ _ a)        = location a == Nothing
isGenerated _                   = False

isState n                       = stateVar (annot n)


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
  show (Name s 0 _)             = show s
  show (Name s n _)             = show (s ++ "_" ++ show n)
  show (Tuple n _)              = show ('(' : replicate (n-1) ',' ++ ")")
  show (Prim p _)               = show (strRep p)


instance Pr Name where
  pr (Name s 0 a)               = {- prExpl a <> -} text s
  pr (Name s n a)               = {- prExpl a <> -} text (s ++ "_" ++ show n)
  pr (Tuple n a)                = text ('(' : replicate (n-1) ',' ++ ")")
  pr (Prim p a)                 = text (strRep p)


prExpl a                        = if explicit a then text "~" else empty


prId i                          = if isSym i then parens (pr i) else pr i

prOp i                          = if isSym i then pr i else backQuotes (pr i)

prId2 (Prim p _)                = text (strRep2 p)
prId2 (Tuple n _)               = text ("TUP" ++ show n)
prId2 n                         = prId n

prId3 (Name s n a)
  | n == 0                      = text s
  | otherwise                   = pre <> text ('_' : show n)
  where pre                     = if isAlpha (head s) && all isAlphaNum (tail s) then text s else text "SYM"
prId3 n                         = prId2 n



packName n                      = show n

unpackName x                    = case break (=='_') x of
                                    (s,"") -> name0 s
                                    (s,n)  -> (name0 s) { tag = read n }
