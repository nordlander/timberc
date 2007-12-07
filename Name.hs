module Name where

import List
import PP
import Token
import Char
import Maybe
import Data.Binary 

-- The type of names ---------------------------------------------------------------------

data Name                       = Name  { str :: String, tag :: Int, fromMod :: Maybe String, annot :: Annot }
                                | Prim  { con :: Prim, annot :: Annot }
                                | Tuple { width :: Int, annot :: Annot }


data Annot                      = Annot { location :: Maybe (Int,Int), 
                                          explicit :: Bool, 
                                          stateVar :: Bool , 
                                          generated :: Bool,
                                          suppressMod :: Bool}


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

                                | MIN____KINDLE_INFIX
                                
                                | IntPlus
                                | IntMinus
                                | IntTimes
                                | IntDiv
                                | IntMod

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
                                
                                | FloatEQ
                                | FloatNE
                                | FloatLT
                                | FloatLE
                                | FloatGE
                                | FloatGT

                                | TimeEQ
                                | TimeNE
                                | TimeLT
                                | TimeLE
                                | TimeGE
                                | TimeGT

                                | MsgEQ
                                | MsgNE

                                | PidEQ
                                | PidNE
                                
                                | LazyOr
                                | LazyAnd               
                                
                                | MAX____KINDLE_INFIX

                                | IntNeg
                                | FloatNeg

                                | IntToFloat
                                | FloatToInt

                                | CharToInt
                                | IntToChar

                                | Sec
                                | Millisec
                                | Microsec
                                | Nanosec
                                | Infinity
                                
                                | TimePlus
                                | TimeMinus
                                | TimeMin
                                
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
                                
                                | Obj                   -- first selector of every state struct
                                | Object                -- its type
                                | ObjInit               -- its initializer
                                
                                | Code                  -- selectors of struct Msg
                                | Baseline
                                | Deadline
                                | Next
                                
                                | AbsTime               -- type of selectors Baseline and Deadline
                                
                                | OwnedBy               -- selectors of struct Object
                                | WantedBy
                                
                                | CharBox               -- The Kindle structs for boxed values
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

lowPrims                        = [Sec,Millisec,Microsec,Nanosec,Raise,Catch,Baseline,Deadline,Next,OwnedBy,WantedBy,Infinity]

strRep LIST                     = "[]"
strRep UNITTYPE                 = "()"
strRep UNITTERM                 = "()"
strRep NIL                      = "[]"
strRep CONS                     = ":"
strRep TRUE                     = "True"
strRep FALSE                    = "False"
strRep p                        = strRep2 p
                                
strRep2 p
  | p `elem` lowPrims           = toLower (head s) : tail s
  | isConPrim p || invisible p  = s
  | otherwise                   = "prim" ++ s
  where s                       = show p


-- Name construction ------------------------------------------------------------

noAnnot                         = Annot { location = Nothing, explicit = False, stateVar = False, generated = False, suppressMod = False }

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
  show (Name s n m a)           = s ++ tag ++ mod
     where tag                  = if n/=0 && generated a then '_' : show n else ""
           mod                  = if m==Nothing || suppressMod a then "" else "'" ++ fromJust m
{-
  show (Name s 0 Nothing _)     = show s
  show (Name s n Nothing _)     = show (s ++ "_" ++ show n)
  show (Name s n (Just m) a) 
     |location a == Nothing     = show (s ++ "_" ++ show n ++ "'" ++ m)
     |otherwise                 = show (s ++ "'" ++ m)
-}
  show (Tuple n _)              = show ('(' : replicate (n-1) ',' ++ ")")
  show (Prim p _)               = strRep p


instance Pr Name where
  pr n                          = text (show n)
{-
--  pr (Name s 0 Nothing a)       = {- prExpl a <> -} text s
{-
  pr (Name s n Nothing a)
        |generated a            =  {- prExpl a <> -} text (s ++ "_" ++ show n)
        |otherwise              =  {- prExpl a <> -} text s
  pr (Name s n (Just m) a)      
        |generated a            =  {- prExpl a <> -} text (s ++ "_" ++ show n ++ "'" ++ m)
        |otherwise              =  {- prExpl a <> -} text (s ++ "'" ++ m)
-}
  pr (Tuple n a)                = text ('(' : replicate (n-1) ',' ++ ")")
  pr (Prim p a)                 = text (strRep p)
-}

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
  put (Annot _ b c d e) = put b >> put c >> put d >> put e
  get = get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (Annot Nothing b c d e)


maxPrimWord = fromIntegral (fromEnum maxPrim) :: Word8

instance Binary Prim where
  put p = putWord8 (fromIntegral (fromEnum p))
  get = do
    w <- getWord8
    if w <= maxPrimWord
        then return (toEnum (fromIntegral w))
        else fail "no parse"
