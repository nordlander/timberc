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

                                | Array

                                -- Constructor symbols (special syntax)

                                | LIST                  -- Type
                                | UNITTYPE              -- Type
                                
                                | UNITTERM              -- Term
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

                                | IntToFloat
                                | FloatToInt

                                | CharToInt
                                | IntToChar

                                | LazyOr
                                | LazyAnd

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

                                | ListArray
                                | ConstArray
                                | SizeArray
                                | IndexArray
                                | UpdateArray
                                | CloneArray


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
                                
                                | Inherit               -- default Time value

                                | Tag                   -- first selector of every datatype/constructor struct
                                
                                | Code                  -- selectors of struct Msg
                                | Baseline
                                | Deadline
                                
                                | CharBox               -- The Kindle Struct for boxed values
                                | FloatBox                
                                | IntBox                 
                                | TimeBox 
                                | Value                 -- selector of struct Box
                                
                                | LISTtags              -- The Kindle Enum for built-in LIST tags
                                deriving (Eq,Ord,Enum,Bounded,Show)

isConPrim p                     = p <= TRUE

invisible p                     = p >= ASYNC

isIdPrim p                      = p `notElem` primSyms

primSyms                        = [LIST, NIL, CONS]

primTypes                       = map primKeyValue [Action .. UNITTYPE] ++ alreadyPrimed [Action .. UNITTYPE]

primTerms                       = map primKeyValue [NIL .. Before] ++ alreadyPrimed [UNITTERM,NIL,CONS,LazyAnd,LazyOr]

primKeyValue p                  = (name0 (strRep p), prim p)

alreadyPrimed ps                = map (\p -> (prim p, prim p)) ps


strRep LIST                     = "[]"
strRep UNITTYPE                 = "()"
strRep UNITTERM                 = "()"
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


name l s                        = qualName (Name s 0 Nothing (loc l))


opName l "||"                   = prim LazyOr
opName l "&&"                   = prim LazyAnd
opName l s                      = name l s

name0 s                         = Name s 0 Nothing noAnnot

mName m c                       = c {fromMod = m}

qName m n                       = mName (Just m) (name0 n)

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

splitQual s def                 = case splitString s of
                                    [x]    -> (x, def)
                                    (x:xs) -> (x, joinString xs)

qualName n@(Name s _ Nothing _) = case splitString s of
                                    [x] -> n
                                    (x : xs) -> n {str = x, fromMod = Just(joinString xs)}
qualName n                      = n

isQual m n                      = fromMod n == Just (str m)


tag0 (Name s t m a)             = Name s 0 m a
tag0 n                          = n

isTuple (Tuple _ _)             = True
isTuple _                       = False

annotExplicit n                 = n { annot = a { explicit = True } }
  where a                       = annot n

annotState n                    = n { annot = a { stateVar = True } }
  where a                       = annot n


-- Textual name supply ---------------------------------------------------------------------------

abcSupply                               = map name0 (gensupply "abcdefghijklmnopqrstuvwxyz")


gensupply                               :: [Char] -> [String]
gensupply chars                         = map (:"") chars ++ map (:"'") chars ++ concat (map g [1..])
  where g n                             = map (replicate n) chars


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
  Name a s Nothing _  == Name b t Nothing _  = s == t
  Name a 0 (Just m) _ == Name b _ (Just n) _ = a == b && m == n
  Name a _ (Just m) _ == Name b 0 (Just n) _ = a == b && m == n
  Name _ s (Just m) _ == Name _ t (Just n) _ = s == t && m == n
  Tuple a _           == Tuple b _           = a == b
  Prim a _            == Prim b _            = a == b
  _                   == _                   = False

instance Ord Name where
  Prim a _   <= Prim b _        = a <= b
  Prim _ _   <= _               = True
  Tuple a _  <= Tuple b _       = a <= b
  Tuple _ _  <= Name _ _ _ _    = True
  Name a _ (Just m) _ <= Name b _ (Just n) _  = a < b || ( a == b && m <= n )
  Name a 0 _ _ <= Name b 0 _ _  = a <= b
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
  show (Prim p _)               = "Prim "++show p


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

name2str n                      = render (prId3 n)

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
  put Array = putWord8 13
  put LIST = putWord8 14
  put UNITTYPE = putWord8 15
  put UNITTERM = putWord8 16
  put NIL = putWord8 17
  put CONS = putWord8 18
  put FALSE = putWord8 19
  put TRUE = putWord8 20
  put Refl = putWord8 21
  put ActToCmd = putWord8 22
  put ReqToCmd = putWord8 23
  put TemplToCmd = putWord8 24
  put RefToPID = putWord8 25
  put IntPlus = putWord8 26
  put IntMinus = putWord8 27
  put IntTimes = putWord8 28
  put IntDiv = putWord8 29
  put IntMod = putWord8 30
  put IntNeg = putWord8 31
  put IntEQ = putWord8 32
  put IntNE = putWord8 33
  put IntLT = putWord8 34
  put IntLE = putWord8 35
  put IntGE = putWord8 36
  put IntGT = putWord8 37
  put FloatPlus = putWord8 38
  put FloatMinus = putWord8 39
  put FloatTimes = putWord8 40
  put FloatDiv = putWord8 41
  put FloatNeg = putWord8 42
  put FloatEQ = putWord8 43
  put FloatNE = putWord8 44
  put FloatLT = putWord8 45
  put FloatLE = putWord8 46
  put FloatGE = putWord8 47
  put FloatGT = putWord8 48
  put IntToFloat = putWord8 49
  put FloatToInt = putWord8 50
  put CharToInt = putWord8 51
  put IntToChar = putWord8 52
  put LazyOr = putWord8 53
  put LazyAnd = putWord8 54
  put MsgEQ = putWord8 55
  put MsgNE = putWord8 56
  put PidEQ = putWord8 57
  put PidNE = putWord8 58
  put Sec = putWord8 59
  put MilliSec = putWord8 60
  put MicroSec = putWord8 61
  put NanoSec = putWord8 62
  put Infinity = putWord8 63
  put TimePlus = putWord8 64
  put TimeMinus = putWord8 65
  put TimeMin = putWord8 66
  put TimeEQ = putWord8 67
  put TimeNE = putWord8 68
  put TimeLT = putWord8 69
  put TimeLE = putWord8 70
  put TimeGE = putWord8 71
  put TimeGT = putWord8 72
  put ListArray = putWord8 73
  put ConstArray = putWord8 74
  put SizeArray = putWord8 75
  put IndexArray = putWord8 76
  put UpdateArray = putWord8 77
  put CloneArray = putWord8 78
  put Fail = putWord8 79
  put Commit = putWord8 80
  put Match = putWord8 81
  put Fatbar = putWord8 82
  put After = putWord8 83
  put Before = putWord8 84
  put ASYNC = putWord8 85
  put LOCK = putWord8 86
  put UNLOCK = putWord8 87
  put NEW = putWord8 88
  put Inherit = putWord8 89
  put Tag = putWord8 90
  put Code = putWord8 91
  put Baseline = putWord8 92
  put Deadline = putWord8 93
  put CharBox = putWord8 94
  put FloatBox = putWord8 95
  put IntBox = putWord8 96
  put TimeBox = putWord8 97
  put Value = putWord8 98
  put LISTtags = putWord8 99

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
      13 -> return Array
      14 -> return LIST
      15 -> return UNITTYPE
      16 -> return UNITTERM
      17 -> return NIL
      18 -> return CONS
      19 -> return FALSE
      20 -> return TRUE
      21 -> return Refl
      22 -> return ActToCmd
      23 -> return ReqToCmd
      24 -> return TemplToCmd
      25 -> return RefToPID
      26 -> return IntPlus
      27 -> return IntMinus
      28 -> return IntTimes
      29 -> return IntDiv
      30 -> return IntMod
      31 -> return IntNeg
      32 -> return IntEQ
      33 -> return IntNE
      34 -> return IntLT
      35 -> return IntLE
      36 -> return IntGE
      37 -> return IntGT
      38 -> return FloatPlus
      39 -> return FloatMinus
      40 -> return FloatTimes
      41 -> return FloatDiv
      42 -> return FloatNeg
      43 -> return FloatEQ
      44 -> return FloatNE
      45 -> return FloatLT
      46 -> return FloatLE
      47 -> return FloatGE
      48 -> return FloatGT
      49 -> return IntToFloat
      50 -> return FloatToInt
      51 -> return CharToInt
      52 -> return IntToChar
      53 -> return LazyOr
      54 -> return LazyAnd
      55 -> return MsgEQ
      56 -> return MsgNE
      57 -> return PidEQ
      58 -> return PidNE
      59 -> return Sec
      60 -> return MilliSec
      61 -> return MicroSec
      62 -> return NanoSec
      63 -> return Infinity
      64 -> return TimePlus
      65 -> return TimeMinus
      66 -> return TimeMin
      67 -> return TimeEQ
      68 -> return TimeNE
      69 -> return TimeLT
      70 -> return TimeLE
      71 -> return TimeGE
      72 -> return TimeGT
      73 -> return ListArray
      74 -> return ConstArray
      75 -> return SizeArray
      76 -> return IndexArray
      77 -> return UpdateArray
      78 -> return CloneArray
      79 -> return Fail
      80 -> return Commit
      81 -> return Match
      82 -> return Fatbar
      83 -> return After
      84 -> return Before
      85 -> return ASYNC
      86 -> return LOCK
      87 -> return UNLOCK
      88 -> return NEW
      89 -> return Inherit
      90 -> return Tag
      91 -> return Code
      92 -> return Baseline
      93 -> return Deadline
      94 -> return CharBox
      95 -> return FloatBox
      96 -> return IntBox
      97 -> return TimeBox
      98 -> return Value
      99 -> return LISTtags
      _ -> fail "no parse"
