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
  put LIST = putWord8 13
  put UNITTYPE = putWord8 14
  put UNITTERM = putWord8 15
  put NIL = putWord8 16
  put CONS = putWord8 17
  put FALSE = putWord8 18
  put TRUE = putWord8 19
  put Refl = putWord8 20
  put ActToCmd = putWord8 21
  put ReqToCmd = putWord8 22
  put TemplToCmd = putWord8 23
  put RefToPID = putWord8 24
  put IntPlus = putWord8 25
  put IntMinus = putWord8 26
  put IntTimes = putWord8 27
  put IntDiv = putWord8 28
  put IntMod = putWord8 29
  put IntNeg = putWord8 30
  put IntEQ = putWord8 31
  put IntNE = putWord8 32
  put IntLT = putWord8 33
  put IntLE = putWord8 34
  put IntGE = putWord8 35
  put IntGT = putWord8 36
  put FloatPlus = putWord8 37
  put FloatMinus = putWord8 38
  put FloatTimes = putWord8 39
  put FloatDiv = putWord8 40
  put FloatNeg = putWord8 41
  put FloatEQ = putWord8 42
  put FloatNE = putWord8 43
  put FloatLT = putWord8 44
  put FloatLE = putWord8 45
  put FloatGE = putWord8 46
  put FloatGT = putWord8 47
  put IntToFloat = putWord8 48
  put FloatToInt = putWord8 49
  put CharToInt = putWord8 50
  put IntToChar = putWord8 51
  put LazyOr = putWord8 52
  put LazyAnd = putWord8 53
  put MsgEQ = putWord8 54
  put MsgNE = putWord8 55
  put PidEQ = putWord8 56
  put PidNE = putWord8 57
  put Sec = putWord8 58
  put MilliSec = putWord8 59
  put MicroSec = putWord8 60
  put NanoSec = putWord8 61
  put Infinity = putWord8 62
  put TimePlus = putWord8 63
  put TimeMinus = putWord8 64
  put TimeMin = putWord8 65
  put TimeEQ = putWord8 66
  put TimeNE = putWord8 67
  put TimeLT = putWord8 68
  put TimeLE = putWord8 69
  put TimeGE = putWord8 70
  put TimeGT = putWord8 71
  put Fail = putWord8 72
  put Commit = putWord8 73
  put Match = putWord8 74
  put Fatbar = putWord8 75
  put After = putWord8 76
  put Before = putWord8 77
  put ASYNC = putWord8 78
  put LOCK = putWord8 79
  put UNLOCK = putWord8 80
  put NEW = putWord8 81
  put Code = putWord8 82
  put Baseline = putWord8 83
  put Deadline = putWord8 84
  put CharBox = putWord8 85
  put FloatBox = putWord8 86
  put IntBox = putWord8 87
  put Value = putWord8 88
  put LISTtags = putWord8 89
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
      14 -> return UNITTYPE
      15 -> return UNITTERM
      16 -> return NIL
      17 -> return CONS
      18 -> return FALSE
      19 -> return TRUE
      20 -> return Refl
      21 -> return ActToCmd
      22 -> return ReqToCmd
      23 -> return TemplToCmd
      24 -> return RefToPID
      25 -> return IntPlus
      26 -> return IntMinus
      27 -> return IntTimes
      28 -> return IntDiv
      29 -> return IntMod
      30 -> return IntNeg
      31 -> return IntEQ
      32 -> return IntNE
      33 -> return IntLT
      34 -> return IntLE
      35 -> return IntGE
      36 -> return IntGT
      37 -> return FloatPlus
      38 -> return FloatMinus
      39 -> return FloatTimes
      40 -> return FloatDiv
      41 -> return FloatNeg
      42 -> return FloatEQ
      43 -> return FloatNE
      44 -> return FloatLT
      45 -> return FloatLE
      46 -> return FloatGE
      47 -> return FloatGT
      48 -> return IntToFloat
      49 -> return FloatToInt
      50 -> return CharToInt
      51 -> return IntToChar
      52 -> return LazyOr
      53 -> return LazyAnd
      54 -> return MsgEQ
      55 -> return MsgNE
      56 -> return PidEQ
      57 -> return PidNE
      58 -> return Sec
      59 -> return MilliSec
      60 -> return MicroSec
      61 -> return NanoSec
      62 -> return Infinity
      63 -> return TimePlus
      64 -> return TimeMinus
      65 -> return TimeMin
      66 -> return TimeEQ
      67 -> return TimeNE
      68 -> return TimeLT
      69 -> return TimeLE
      70 -> return TimeGE
      71 -> return TimeGT
      72 -> return Fail
      73 -> return Commit
      74 -> return Match
      75 -> return Fatbar
      76 -> return After
      77 -> return Before
      78 -> return ASYNC
      79 -> return LOCK
      80 -> return UNLOCK
      81 -> return NEW
      82 -> return Code
      83 -> return Baseline
      84 -> return Deadline
      85 -> return CharBox
      86 -> return FloatBox
      87 -> return IntBox
      88 -> return Value
      89 -> return LISTtags
      _ -> fail "no parse"
