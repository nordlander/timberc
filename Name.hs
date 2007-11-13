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


data Annot                      = Annot { location :: Maybe (Int,Int), explicit :: Bool, stateVar :: Bool , generated :: Bool}


-- The built-in primitives ----------------------------------------------------------------

data Prim                       = 

                                -- Constructor identifiers

                                  MIN____TYPE

                                | Action                -- Types
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

                                | MAX____TYPE
                                
                                | MIN____CONS
                                
                                | UNITTERM              -- Term
                                | NIL                   -- Term
                                | CONS                  -- Term

                                -- Constructor identifiers (special syntax)

                                | FALSE                 -- Terms
                                | TRUE

                                | MAX____CONS
                                
                                -- Variable identifiers

                                | MIN____VAR
                                
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

                                | Raise
                                | Catch
                                
                                | ListArray
                                | ConstArray
                                | SizeArray
                                | IndexArray
                                | UpdateArray
                                | CloneArray
                                
                                | MAX____KINDLEVAR

-- transformed away before Kindle conversion ----------------------------------------------------

                                | Fail
                                | Commit
                                | Match
                                | Fatbar

                                | After
                                | Before
                                
                                | MAX____VAR

-- invisible ------------------------------------------------------------------------------------

                                | MIN____INVISIBLE
                                
                                | ASYNC                 -- RTS entry points
                                | LOCK
                                | UNLOCK
                                
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
                                
                                | MAX____INVISIBLE
                                
                                deriving (Eq,Ord,Enum,Bounded,Show)

minPrim                         = minBound :: Prim
maxPrim                         = maxBound :: Prim

isConPrim p                     = p <= MAX____CONS

invisible p                     = p >= MIN____INVISIBLE

isIdPrim p                      = p `notElem` primSyms

primSyms                        = [LIST, NIL, CONS]

primTypes                       = map primKeyValue typerange ++ alreadyPrimed typerange
  where typerange               = [MIN____TYPE .. MAX____TYPE]

primTerms                       = map primKeyValue [MIN____CONS .. MAX____VAR] ++ alreadyPrimed [UNITTERM,NIL,CONS,LazyAnd,LazyOr]

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
strRep Raise                    = "raise"
strRep Catch                    = "catch"
strRep Infinity                 = "infinity"
strRep p                        = strRep2 p
                                
strRep2 p
  | isConPrim p || invisible p  = show p
  | otherwise                   = "prim" ++ show p


-- Name construction ------------------------------------------------------------

noAnnot                         = Annot { location = Nothing, explicit = False, stateVar = False, generated = False }

name l s                        = qualName (Name s 0 Nothing (loc l))

name' s                         = Name s 0 Nothing noAnnot

loc l                           = noAnnot { location = Just l }

opName l "||"                   = (prim LazyOr) {annot = loc l}
opName l "&&"                   = (prim LazyAnd) {annot = loc l}
opName l s                      = name l s

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

splitQual s def                 = case splitString s of
                                    [x]    -> (x, def)
                                    (x:xs) -> (x, joinString xs)

qualName n@(Name s _ Nothing _) = case splitString s of
                                    [x] -> n
                                    (x : xs) -> n {str = x, fromMod = Just(joinString xs)}
qualName n                      = n

tag0 (Name s t m a)             = Name s 0 m a
tag0 n                          = n

annotExplicit n                 = n { annot = a { explicit = True } }
  where a                       = annot n

annotState n                    = n { annot = a { stateVar = True } }
  where a                       = annot n

pos n                           = location (annot n)

-- Generated names ----------------------------------------------------------------

genAnnot                        = noAnnot { generated = True }

name0 s                         = Name s 0 Nothing genAnnot

qName m n                       = mName (Just m) (name0 n)


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

isTuple (Tuple _ _)             = True
isTuple _                       = False

isVar i                         = not (isCon i)

isQual m n                      = fromMod n == Just (str m)

isGenerated (Name _ _ _ a)      = generated a 
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
  pr (Name s n Nothing a)
        |generated a            =  {- prExpl a <> -} text (s ++ "_" ++ show n)
        |otherwise              =  {- prExpl a <> -} text s
  pr (Name s n (Just m) a)      
        |generated a            =  {- prExpl a <> -} text (s ++ "_" ++ show n ++ "'" ++ m)
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


-- Binary --------------------------------------------------------

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
  put (Annot _ b c d) = put b >> put c >> put d
  get = get >>= \b -> get >>= \c -> get >>= \d -> return (Annot Nothing b c d)


maxPrimWord = fromIntegral (fromEnum maxPrim) :: Word8

instance Binary Prim where
  put p = putWord8 (fromIntegral (fromEnum p))
  get = do
    w <- getWord8
    if w <= maxPrimWord
        then return (toEnum (fromIntegral w))
        else fail "no parse"
