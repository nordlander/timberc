module Name where

import List
import PP
import Token
import Char
import Data.Binary 

-- The type of names ---------------------------------------------------------------------

data Name                       = Name  { str :: String, tag :: Int, fromMod :: Maybe String, annot :: Annot }
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

primTypes                       = map primKeyValue [Action .. UNIT] ++ alreadyPrimed [Action .. UNIT]

primTerms                       = map primKeyValue [UNIT .. Before] ++ alreadyPrimed [UNIT,NIL,CONS]

primKeyValue p                  = (name0 (strRep p), prim p)

alreadyPrimed ps                = map (\p -> (prim p, prim p)) ps

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


name l s                        = qualName(Name s 0 Nothing (loc l))

name0 s                         = Name s 0 Nothing noAnnot

mName m c                       = c {fromMod = m}

prim p                          = Prim p noAnnot

tuple n                         = Tuple n noAnnot


splitString s                   = case break2 s of
                                    (local,[])  -> [local]                                 
                                    (local,suf) 
                                        | all (isUpper . head) mods -> local : mods
                                        | otherwise -> error ("Illegal module name in qualified name " ++ s)
                                        where mods = splitString suf
   where break2 xs              = move (break (== '\'') xs)
         move (xs,y:z:ys)
                 | z == '\''    = move (xs++[y],z : ys)
                 | otherwise    = (xs, z : ys)
         move (xs,ys)           = (xs ++ ys, [])

joinString [x]                  = x
joinString (x : xs)             = x ++ '\'' : joinString xs                             
   
qualName n@(Name s _ Nothing _) = case splitString s of
                                    [x] -> n
                                    (x : xs) -> n {str = x, fromMod = Just(joinString xs)}
qualName n                      = n

tag0 (Name s t m a)             = Name s 0 m a
tag0 n                          = n

isTuple (Tuple _ _)             = True
isTuple _                       = False

annotExplicit n                 = n { annot = a { explicit = True } }
  where a                       = annot n

annotState n                    = n { annot = a { stateVar = True } }
  where a                       = annot n


-- Testing Names ----------------------------------------------------------------

isId (Name s _ _ _)             = isIdent (head s)
isId (Tuple _ _)                = True
isId (Prim p _)                 = isIdPrim p

isSym i                         = not (isId i)


isCon (Name (c:_) _ _ _)        = isIdent c && isUpper c || c == ':'
isCon (Tuple _ _)               = True
isCon (Prim p _)                = isConPrim p

isVar i                         = not (isCon i)

isGenerated (Name _ _ _ a)      = location a == Nothing
isGenerated _                   = False

isState n                       = stateVar (annot n)

isQualified n                   = fromMod n /= Nothing

-- Equality & Order ----------------------------------------------------------------

instance Eq Name where
  Name a 0 Nothing _  == Name b 0 Nothing _  = a == b
  Name a m Nothing _  == Name b n Nothing _  = m == n
  Name a 0 (Just m) _ == Name b _ (Just n) _ = a == b && m == n
  Name a _ (Just m) _ == Name b 0 (Just n) _ = a == b && m == n
  Name _ a (Just m) _ == Name _ b (Just n) _ = a == b && m == n
  Tuple a _           == Tuple b _           = a == b
  Prim a _            == Prim b _            = a == b
  _                   == _                   = False

-- BvS: I do not understand the use of the Ord instance for Name in Env (for deriving Ord for Rank)
-- but I guess that the ordering of Names is arbitrary for that purpose. 
-- The order below is what we want for sorting selectors in Desugar1.

instance Ord Name where
  Prim a _   <= Prim b _        = a <= b
  Prim _ _   <= _               = True
  Tuple a _  <= Tuple b _       = a <= b
  Tuple _ _  <= Name _ _ _ _    = True
  Name a _ (Just m) _ <= Name b _ (Just n) _  = a < b || ( a == b && m <= n )
  Name _ a _ _ <= Name _ b _ _  = a <= b
  _          <= _               = False


-- Printing Names -----------------------------------------------------------------

instance Show Name where
  show (Name s 0 Nothing _)     = show s
  show (Name s n Nothing _)     = show (s ++ "_" ++ show n)
  show (Name s n (Just m) a) 
     |location a == Nothing     = show (s ++ "_" ++ show n ++ "'" ++ m)
     |otherwise                 = show (s ++ "'" ++ m)
  show (Tuple n _)              = show ('(' : replicate (n-1) ',' ++ ")")
  show (Prim p _)               = show (strRep p)


instance Pr Name where
  pr (Name s 0 Nothing a)       = {- prExpl a <> -} text s
  pr (Name s n Nothing a)       = {- prExpl a <> -} text (s ++ "_" ++ show n)
  pr (Name s n (Just m) a)      
        |location a == Nothing  =  {- prExpl a <> -} text (s ++ "_" ++ show n ++ "'" ++ m)
        |otherwise              =  {- prExpl a <> -} text (s ++ "'" ++ m)
  pr (Tuple n a)                = text ('(' : replicate (n-1) ',' ++ ")")
  pr (Prim p a)                 = text (strRep p)


prExpl a                        = if explicit a then text "~" else empty


prId i                          = if isSym i then parens (pr i) else pr i

prOp i                          = if isSym i then pr i else backQuotes (pr i)

prId2 (Prim p _)                = text (strRep2 p)
prId2 (Tuple n _)               = text ("TUP" ++ show n)
prId2 n                         = prId n

prId3 (Name s n m a)
  | n == 0                      = text s
  | otherwise                   = pre <> text ('_' : show n) <> text post
  where pre                     = if isAlpha (head s) && all isAlphaNum (tail s) then text s else text "SYM"
        post                    = maybe "" (('_' :) . modToundSc) m
prId3 n                         = prId2 n

modToPath m                     = concat (List.intersperse "/" (splitString m))

modToundSc m                    = concat (List.intersperse "_" (splitString m))

packName n                      = show n

unpackName x                    = case break (=='_') x of
                                    (s,"") -> name0 s
                                    (s,n)  -> (name0 s) { tag = read n }

------- Binary --------------------------------------------------------

instance Binary Name where
  put (Name a b c d) = putWord8 0 >> put a >> put b >> put c >> put d
  put (Prim a b) = putWord8 1 >> put a >> put b
  put (Tuple a b) = putWord8 2 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Name a b c d)
      1 -> get >>= \a -> get >>= \b -> return (Prim a b)
      2 -> get >>= \a -> get >>= \b -> return (Tuple a b)
      _ -> fail "no parse"


instance Binary Annot where
  put (Annot a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Annot a b c)

instance Binary Prim where
  put Action = putWord8 0
  put Request = putWord8 1
  put Template = putWord8 2
  put Cmd = putWord8 3
  put Msg = putWord8 4
  put Ref = putWord8 5
  put PID = putWord8 6
  put PMC = putWord8 7
  put Time = putWord8 8
  put Int = putWord8 9
  put Float = putWord8 10
  put Char = putWord8 11
  put Bool = putWord8 12
  put LIST = putWord8 13
  put UNIT = putWord8 14
  put NIL = putWord8 15
  put CONS = putWord8 16
  put FALSE = putWord8 17
  put TRUE = putWord8 18
  put Refl = putWord8 19
  put ActToCmd = putWord8 20
  put ReqToCmd = putWord8 21
  put TemplToCmd = putWord8 22
  put RefToPID = putWord8 23
  put IntPlus = putWord8 24
  put IntMinus = putWord8 25
  put IntTimes = putWord8 26
  put IntDiv = putWord8 27
  put IntMod = putWord8 28
  put IntNeg = putWord8 29
  put IntEQ = putWord8 30
  put IntNE = putWord8 31
  put IntLT = putWord8 32
  put IntLE = putWord8 33
  put IntGE = putWord8 34
  put IntGT = putWord8 35
  put FloatPlus = putWord8 36
  put FloatMinus = putWord8 37
  put FloatTimes = putWord8 38
  put FloatDiv = putWord8 39
  put FloatNeg = putWord8 40
  put FloatEQ = putWord8 41
  put FloatNE = putWord8 42
  put FloatLT = putWord8 43
  put FloatLE = putWord8 44
  put FloatGE = putWord8 45
  put FloatGT = putWord8 46
  put CharToInt = putWord8 47
  put IntToChar = putWord8 48
  put MsgEQ = putWord8 49
  put MsgNE = putWord8 50
  put PidEQ = putWord8 51
  put PidNE = putWord8 52
  put Sec = putWord8 53
  put MilliSec = putWord8 54
  put MicroSec = putWord8 55
  put NanoSec = putWord8 56
  put Infinity = putWord8 57
  put TimePlus = putWord8 58
  put TimeMinus = putWord8 59
  put TimeMin = putWord8 60
  put TimeEQ = putWord8 61
  put TimeNE = putWord8 62
  put TimeLT = putWord8 63
  put TimeLE = putWord8 64
  put TimeGE = putWord8 65
  put TimeGT = putWord8 66
  put Fail = putWord8 67
  put Commit = putWord8 68
  put Match = putWord8 69
  put Fatbar = putWord8 70
  put After = putWord8 71
  put Before = putWord8 72
  put ASYNC = putWord8 73
  put LOCK = putWord8 74
  put UNLOCK = putWord8 75
  put NEW = putWord8 76
  put Code = putWord8 77
  put Baseline = putWord8 78
  put Deadline = putWord8 79
  put Box = putWord8 80
  put Value = putWord8 81
  put LISTtags = putWord8 82
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Action
      1 -> return Request
      2 -> return Template
      3 -> return Cmd
      4 -> return Msg
      5 -> return Ref
      6 -> return PID
      7 -> return PMC
      8 -> return Time
      9 -> return Int
      10 -> return Float
      11 -> return Char
      12 -> return Bool
      13 -> return LIST
      14 -> return UNIT
      15 -> return NIL
      16 -> return CONS
      17 -> return FALSE
      18 -> return TRUE
      19 -> return Refl
      20 -> return ActToCmd
      21 -> return ReqToCmd
      22 -> return TemplToCmd
      23 -> return RefToPID
      24 -> return IntPlus
      25 -> return IntMinus
      26 -> return IntTimes
      27 -> return IntDiv
      28 -> return IntMod
      29 -> return IntNeg
      30 -> return IntEQ
      31 -> return IntNE
      32 -> return IntLT
      33 -> return IntLE
      34 -> return IntGE
      35 -> return IntGT
      36 -> return FloatPlus
      37 -> return FloatMinus
      38 -> return FloatTimes
      39 -> return FloatDiv
      40 -> return FloatNeg
      41 -> return FloatEQ
      42 -> return FloatNE
      43 -> return FloatLT
      44 -> return FloatLE
      45 -> return FloatGE
      46 -> return FloatGT
      47 -> return CharToInt
      48 -> return IntToChar
      49 -> return MsgEQ
      50 -> return MsgNE
      51 -> return PidEQ
      52 -> return PidNE
      53 -> return Sec
      54 -> return MilliSec
      55 -> return MicroSec
      56 -> return NanoSec
      57 -> return Infinity
      58 -> return TimePlus
      59 -> return TimeMinus
      60 -> return TimeMin
      61 -> return TimeEQ
      62 -> return TimeNE
      63 -> return TimeLT
      64 -> return TimeLE
      65 -> return TimeGE
      66 -> return TimeGT
      67 -> return Fail
      68 -> return Commit
      69 -> return Match
      70 -> return Fatbar
      71 -> return After
      72 -> return Before
      73 -> return ASYNC
      74 -> return LOCK
      75 -> return UNLOCK
      76 -> return NEW
      77 -> return Code
      78 -> return Baseline
      79 -> return Deadline
      80 -> return Box
      81 -> return Value
      82 -> return LISTtags
      _ -> fail "no parse"
