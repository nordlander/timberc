module Name where

import Debug.Trace
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
                                          suppressMod :: Bool
                                        }
                                deriving Show


-- The built-in primitives ----------------------------------------------------------------

data Prim                       = 

                                -- Constructor identifiers

                                  MIN____TYPE

                                | Action                -- Types
                                | Request
                                | Class
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

                                | BITS8
                                | BITS16
                                | BITS32

                                | Array
                                | EITHER

                                -- Constructor symbols (special syntax)

                                | LIST                  -- Type
                                | UNITTYPE              -- Type
                                | TIMERTYPE             

                                | MAX____TYPE
                                
                                | MIN____CONS
                                
                                | UNITTERM              -- Term
                                | NIL                   -- Term
                                | CONS                  -- Term

                                -- Constructor identifiers

                                | FALSE                 -- Terms
                                | TRUE
                                | LEFT
                                | RIGHT

                                | MAX____CONS
                                | MIN____SELS

                                | Reset
                                | Sample
 
                                | MAX____SELS
                                
                                -- Variable identifiers

                                | MIN____VAR
                                
                                | Refl                  -- Terms

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

                                | PidEQ
                                | PidNE
                                
                                | LazyOr
                                | LazyAnd

                                | AND8
                                | OR8
                                | EXOR8
                                | SHIFTL8
                                | SHIFTR8
                                
                                | AND16
                                | OR16
                                | EXOR16
                                | SHIFTL16
                                | SHIFTR16
                                
                                | AND32
                                | OR32
                                | EXOR32
                                | SHIFTL32
                                | SHIFTR32

                                | MAX____KINDLE_INFIX

                                | IntNeg
                                | FloatNeg

                                | IntToFloat
                                | FloatToInt

                                | NOT8
                                | NOT16
                                | NOT32
                                
                                | Sqrt
                                | Log
                                | Log10
                                | Exp
                                | Sin
                                | Cos
                                | Tan
                                | Asin
                                | Acos
                                | Atan
                                | Sinh
                                | Cosh
                                | ShowFloat

                                | Sec
                                | Millisec
                                | Microsec
                                | Nanosec
                                | Infinity
                                | SecOf
                                | MicrosecOf
                                
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
                                | UniArray
                                | SizeArray
                                | IndexArray
                                | UpdateArray
                                
                                | MAX____KINDLEVAR

                                | Abort                 -- Preserved in Kindle, but given new translated types
                                | TIMERTERM             --                       -"-

                                
-- transformed away during Kindle conversion ----------------------------------------------------

                                | ActToCmd
                                | ReqToCmd
                                | RefToPID

                                | CharToInt
                                | IntToChar

                                | BITS8ToInt
                                | BITS16ToInt
                                | BITS32ToInt
                                
                                | IntToBITS8
                                | IntToBITS16
                                | IntToBITS32
                                
                                | MAX____VAR

-- invisible ------------------------------------------------------------------------------------

                                | MIN____INVISIBLE
                                
                                | New                   -- Encoding of the class instantiation syntax in terms of an operator
                                
                                | Fail
                                | Commit
                                | Match
                                | Fatbar

                                | After
                                | Before
                                
                                | NEWREF                -- RTS entry points
                                | ASYNC
                                | LOCK
                                | UNLOCK
                                
                                | EmptyArray            -- For efficient creation of listArrays with constant lists
                                | CloneArray            -- To open up for destructive updates in Kindle

                                | Inherit               -- default Time value

                                | Tag                   -- common selector of datatype/constructor structs
                                
                                | GCINFO                -- first selector of all structs / node constructor in gcinfo tables
                                
                                | STATE                 -- selector of primitive struct Ref
                                | STATEOF               -- shortcut alternative to the above
                                
                                | Code                  -- selectors of primitive struct Msg
                                | Baseline
                                | Deadline
                                | Next
                                
                                | AbsTime               -- type of selectors Baseline and Deadline
                                
                                | POLY                  -- Type representing polymorhic values
                                                                
                                | Float2POLY            -- Conversion macros required to circumvent C casting irreguliarity
                                | POLY2Float
                                
                                | MAX____INVISIBLE
                                
                                deriving (Eq,Ord,Enum,Bounded,Show)

minPrim                         = minBound :: Prim
maxPrim                         = maxBound :: Prim

isConPrim p                     = p <= MAX____CONS

invisible p                     = p >= MIN____INVISIBLE

doEtaExpand (Prim p _)          = not (invisible p)
doEtaExpand (Tuple _ _)         = True
doEtaExpand _                   = False

isIdPrim p                      = p `notElem` primSyms

primSyms                        = [LIST, NIL, CONS, LazyAnd, LazyOr, IndexArray]

primTypes                       =  map primKeyValue [MIN____TYPE .. MAX____TYPE]

primTerms                       =  map primKeyValue ([MIN____CONS .. MAX____CONS] ++ [MIN____VAR .. MAX____VAR])

primSels                        = map primKeyValue [MIN____SELS .. MAX____SELS]
                                  
primKeyValue p                  = (name0 (strRep p), prim p)

rigidNames			            = map rigidKeyValue [IndexArray, LazyAnd, LazyOr]

rigidKeyValue p			        = (strRep p, prim p)

lowPrims                        = [New,Sec,Millisec,Microsec,Nanosec,Raise,Catch,Baseline,Deadline,Next,
                                   Infinity,Reset,Sample,SecOf,MicrosecOf,Abort,
                                   Sqrt,Log,Log10,Exp,Sin,Cos,Tan,Asin,Acos,Atan,Sinh,Cosh]

strRep LIST                     = "[]"
strRep EITHER                   = "Either"
strRep UNITTYPE                 = "()"
strRep UNITTERM                 = "()"
strRep NIL                      = "[]"
strRep CONS                     = ":"
strRep TRUE                     = "True"
strRep FALSE                    = "False"
strRep LEFT                     = "Left"
strRep RIGHT                    = "Right"
strRep LazyAnd                  = "&&"
strRep LazyOr                   = "||"
strRep IndexArray               = "!"
strRep ListArray                = "array"
strRep UniArray                 = "uniarray"
strRep SizeArray                = "size"
strRep TIMERTYPE                = "Timer"
strRep TIMERTERM                = "timer"
strRep p                        = strRep2 p
                                
strRep2 p
  | p `elem` lowPrims           = toLower (head s) : tail s
  | isConPrim p || invisible p  = s
  | otherwise                   = "prim" ++ s
  where s                       = show p


-- Name construction -----
-------------------------------------------------------

noAnnot                         = Annot { location = Nothing, explicit = False, stateVar = False, 
                                          generated = False, suppressMod = False }

-- This function is used (only) by the parser to build Names
name l (q,s)                    = case lookup s rigidNames of
                                    Just n -> n 
                                    Nothing -> Name s 0 (m q) (noAnnot {location = Just l})
                                       where m "" = Nothing
                                             m q = Just q
joinString [x]                  = x
joinString (x : xs)             = x ++ '.' : joinString xs                             

dropMod n                       = n {fromMod = Nothing}
qName m n                       = n {fromMod = Just m}

-- Used for module names in import clauses
modId n@(Name _ _ Nothing _)    = n
modId (Name s t (Just m) a)     = Name (m++'.':s) t Nothing a

prim p                          = Prim p noAnnot

tuple n                         = Tuple n noAnnot


splitString s                   = case break (=='.') s of
                                    (local,[])  -> [local]                                 
                                    (qualStart,suf)  -> qualStart : splitString (tail suf)

splitQual s def                 = case splitString s of
                                    [x]    -> (x, def)
                                    xs -> (last xs, joinString (init xs))


tag0 (Name s t m a)             = Name s 0 m a
tag0 n                          = n

annotExplicit n                 = n { annot = a { explicit = True } }
  where a                       = annot n

annotGenerated n                = n { annot = a { generated = True } }
  where a                       = annot n

pos n                           = location (annot n)

-- Generated names ----------------------------------------------------------------

genAnnot                        = noAnnot { generated = True }
name0 s                         = Name s 0 Nothing genAnnot



-- Textual name supply ---------------------------------------------------------------------------

abcSupply                       = map name0 (gensupply "abcdefghijklmnopqrstuvwxyz")

_abcSupply                      = map (name0 . ('_':)) (gensupply "abcdefghijklmnopqrstuvwxyz")

_ABCSupply                      = map (name0 . ('_':)) (gensupply "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

gensupply                       :: [Char] -> [String]
gensupply chars                 = map (:"") chars ++ concat (map g [1..])
  where g n                     = map (replicate n) chars


-- Internal identifier conventions -----------------------------------------------------

witnessSym                      = "w"
assumptionSym                   = "v"
tempSym                         = "x"
patSym                          = "p"
functionSym                     = "f"
dummySym                        = "d"
paramSym                        = "a"
tyvarSym                        = "t"
coercionSym                     = "c"
labelSym                        = "l"
constrSym                       = "C"
typeSym                         = "T"
stateTypeSym                    = "S"
skolemSym                       = "sk"
selfSym                         = "self"
thisSym                         = "this"
instanceSym                     = "inst"
closureSym                      = "CLOS"
tappSym                         = "TApp"
tabsSym                         = "TAbs"
gcinfoSym                       = "__GC__"

isCoercion n                    = isGenerated n && str n == coercionSym
isPatTemp n                     = isGenerated n && str n == patSym
isClosure n                     = isGenerated n && isPrefixOf closureSym (str n)
isDummy n                       = isGenerated n && str n == dummySym
isWitness n                     = isGenerated n && str n == witnessSym
isLabel n                       = isGenerated n && isPrefixOf labelSym (str n)
isTApp n                        = isGenerated n && str n == tappSym
isTAbs n                        = isGenerated n && str n == tabsSym
isGCInfo n                      = isGenerated n && isPrefixOf gcinfoSym (str n)

explicitSyms                    = [coercionSym, assumptionSym, witnessSym]


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

isQualified (Name _ _ (Just _) _) = True
isQualified _                    = False

isLocal (Name _ _ Nothing _)    = True
isLocal _                       = False

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
  show (Name s n m a)           = mod ++ s ++ tag
     where tag                  = if n/=0 && generated a  then '_' : show n else ""
           mod                  = if m==Nothing || suppressMod a then "" else fromJust m ++ "."
  show (Tuple n _)              = '(' : replicate (n-1) ',' ++ ")"
  show (Prim p _)               = strRep p

instance Pr Name where
--  pr (Name s n m a)             = prExpl a <> text (maybe "" (++ ".") m ++ s++'_':show n)
  pr n                          = text (show n)

prExpl a                        = if explicit a then text "~" else empty


prId i                          = if isSym i then parens (pr i) else pr i

prOp i                          = if isSym i then pr i else backQuotes (pr i)

prId2 (Prim p _)                = text (strRep2 p)
prId2 (Tuple n _)               = text ("TUP" ++ show n)
prId2 n                         = prId n


prId3 n@(Name s t m a)
  | t == 0 || isClosure n || isLabel n
                                = text (s ++ maybe "" (('_' :) . modToundSc) m)
prId3 (Name s n m a)            = text (id ++ tag ++ mod ++ suff)
  where 
    id                          = if okForC s then s else "_sym"
    tag                         = if mod=="" || generated a || id=="_sym"  then '_':show n else ""
    suff                        = if take 2 id == "_sym" then "/* "++s++" */" else ""
    mod                         = maybe "" (('_' :) . modToundSc) m
    okForC cs                   = all (\c -> isAlphaNum c || c=='_') cs
prId3 n                         = prId2 n

name2str n                      = render (prId3 n)

modToPath m                     = m -- concat (List.intersperse "/" (splitString m))

modToundSc m                    = concat (List.intersperse "_" (splitString m))

packName n                      = show n

unpackName x                    = case break (=='_') x of
                                    (s,"") -> name0 s
                                    (s,n)  -> (name0 s) { tag = read (tail n) }


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
  put (Annot _ b c d e) = put b >> put c >> put d >> put e >> put False
  get = get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \(f::Bool) -> return (Annot Nothing b c d e)


maxPrimWord = fromIntegral (fromEnum maxPrim) :: Word8

instance Binary Prim where
  put p = putWord8 (fromIntegral (fromEnum p))
  get = do
    w <- getWord8
    if w <= maxPrimWord
        then return (toEnum (fromIntegral w))
        else fail "no parse"
