{-# LANGUAGE DeriveDataTypeable #-}
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

module Name where

import Debug.Trace
import Data.List
import PP
import Token
import Data.Char
import Data.Maybe
import Data.Typeable
import Data.Binary 

-- The type of names ---------------------------------------------------------------------

type NameRef                    = Name

data Name                       = Name  { str :: String, tag :: Int, fromMod :: Maybe String, annot :: Annot }
                                | Prim  { con :: Prim, annot :: Annot }
                                | Tuple { width :: Int, annot :: Annot }
                                deriving (Typeable)


data Annot                      = Annot { location :: Maybe (Int,Int), 
                                          explicit :: Bool, 
                                          stateVar :: Bool , 
                                          generated :: Bool,
                                          suppress :: Bool,
                                          public :: Bool
                                        }
                                deriving (Show,Typeable)


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
                                | OID
                                | PMC
                                | Time

                                | World
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
                                | TIMERTYPE             

                                | MAX____TYPE
                                
                                | MIN____CONS
                                
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

                                | SHIFTRA8
                                | SET8
                                | CLR8
                                | TST8

                                | SHIFTRA16
                                | SET16
                                | CLR16
                                | TST16

                                | SHIFTRA32
                                | SET32
                                | CLR32
                                | TST32

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
                                | SecOf
                                | MicrosecOf
                                
                                | TimePlus
                                | TimeMinus
                                
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
                                
                                | CharToInt
                                | IntToChar

                                | MAX____KINDLEVAR

                                | Abort                 -- Preserved in Kindle, but given new translated types
                                | TIMERTERM             --                       -"-

				| ClassRefl
				| ReqRefl
				| CmdRefl
                                
-- transformed away during Kindle conversion ----------------------------------------------------

                                | ActToCmd
                                | ReqToCmd
                                | RefToOID

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
                                
                                | CLOS                  -- Generic closure type
                                
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
                                | Obj
                                | Sender
                                | Next
                                | Baseline
                                | Deadline
                                
                                | AbsTime               -- type of selectors Baseline and Deadline
                                | Thread                -- type of selector Sender
                                | POLY                  -- Type representing polymorhic values
                                | WORD                  -- Target-dependent int/ptr type

				| MUTLIST		-- type of lists extendable through mutation, for implementing tail-recursion modulo CONS
				| MUTLISTINIT           -- initial state of extendable lists
				| MUTLISTEXTEND		-- add one element to the end
				| MUTLISTEXTRACT	-- access the built-up list (no more MUTLISTEXTEND must follow afterwards)
				| MUTLISTAPPEXTRACT	-- access the built-up list after appending a final tail
                                
                                | Float2POLY            -- Conversion macros required to circumvent C casting irreguliarity
                                | POLY2Float
                                
                                | MAX____INVISIBLE
                                
                                deriving (Eq,Ord,Enum,Bounded,Show,Typeable)

minPrim                         = minBound :: Prim
maxPrim                         = maxBound :: Prim

isConPrim p                     = p <= MAX____CONS

invisible p                     = p >= MIN____INVISIBLE

doEtaExpand (Prim p _)          = not (invisible p)
doEtaExpand (Tuple _ _)         = True
doEtaExpand _                   = False

isIdPrim p                      = p `notElem` primSyms

primSyms                        = [LIST, NIL, CONS, LazyAnd, LazyOr, IndexArray]

primTypes                       = map primKeyValue [MIN____TYPE .. MAX____TYPE]

primTerms                       = map primKeyValue [MIN____VAR .. MAX____VAR]

primCons                        = map primKeyValue [MIN____CONS .. MAX____CONS]

primSels                        = map primKeyValue [MIN____SELS .. MAX____SELS]
                                  
primKeyValue p                  = (name0 (strRep p), prim p)

lowPrims                        = [New,Sec,Millisec,Microsec,Nanosec,Raise,Catch,Baseline,Deadline,Next,
                                   Reset,Sample,SecOf,MicrosecOf,Abort,
                                   Sqrt,Log,Log10,Exp,Sin,Cos,Tan,Asin,Acos,Atan,Sinh,Cosh]

strRep LIST                     = "[]"
strRep EITHER                   = "Either"
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
strRep p                        = strRepV2 p
                                
strRepV2 p
  | p `elem` lowPrims           = toLower (head s) : tail s
  | isConPrim p || invisible p  = s
  | otherwise                   = "prim" ++ s
  where s                       = show p


-- Primitive symbol representations valid in the C language ---------------------------------------------------------

cSym IntPlus                    = text "+"
cSym IntMinus                   = text "-"
cSym IntTimes                   = text "*"
cSym IntDiv                     = text "/"
cSym IntMod                     = text "%"
cSym IntNeg                     = text "-"

cSym IntEQ                      = text "=="
cSym IntNE                      = text "!="
cSym IntLT                      = text "<"
cSym IntLE                      = text "<="
cSym IntGE                      = text ">="
cSym IntGT                      = text ">"
                                
cSym FloatPlus                  = text "+"
cSym FloatMinus                 = text "-"
cSym FloatTimes                 = text "*"
cSym FloatDiv                   = text "/"
cSym FloatNeg                   = text "-"
                                
cSym FloatEQ                    = text "=="
cSym FloatNE                    = text "!="
cSym FloatLT                    = text "<"
cSym FloatLE                    = text "<="
cSym FloatGE                    = text ">="
cSym FloatGT                    = text ">"

cSym AND8                       = text "&"
cSym OR8                        = text "|"
cSym EXOR8                      = text "^"
cSym SHIFTL8                    = text "<<"
cSym SHIFTR8                    = text ">>"
cSym NOT8                       = text "~"
                                
cSym AND16                      = text "&"
cSym OR16                       = text "|"
cSym EXOR16                     = text "^"
cSym SHIFTL16                   = text "<<"
cSym SHIFTR16                   = text ">>"
cSym NOT16                      = text "~"
                                
cSym AND32                      = text "&"
cSym OR32                       = text "|"
cSym EXOR32                     = text "^"
cSym SHIFTL32                   = text "<<"
cSym SHIFTR32                   = text ">>"
cSym NOT32                      = text "~"

cSym LazyOr                     = text "||"
cSym LazyAnd                    = text "&&"

cSym PidEQ                      = text "=="
cSym PidNE                      = text "!="

cSym Sqrt                       = text "sqrtf"
cSym Log                        = text "logf"
cSym Log10                      = text "log10f"
cSym Exp                        = text "expf"
cSym Sin                        = text "sinf"
cSym Cos                        = text "cosf"
cSym Tan                        = text "tanf"
cSym Asin                       = text "asinf"
cSym Acos                       = text "acosf"
cSym Atan                       = text "atanf"
cSym Sinh                       = text "sinhf"
cSym Cosh                       = text "coshf"

cSym p                          = text (strRepV2 p)


-- Name construction ------------------------------------------------------------

noAnnot                         = Annot { location = Nothing, explicit = False, stateVar = False, 
                                          generated = False, suppress = False,  public = False}

-- These functions are used (only) by the parser to build Names
qname l (q,s)                   = Name s 0 (Just q) (noAnnot {location = Just l})
name l s                        = Name s 0 Nothing (noAnnot {location = Just l})

joinString [x]                  = x
joinString (x : xs)             = x ++ '.' : joinString xs                             

dropMod n                       = n {fromMod = Nothing}
qName m n                       = n {fromMod = Just m}

-- Used for module names in import clauses
modId n@(Name _ _ Nothing _)    = n
modId (Name s t (Just m) a)     = Name (m++'.':s) t Nothing a

prim p                          = Prim p noAnnot

tuple n                         = Tuple n noAnnot


isModuleSyntax s                = all isConId (splitString s)
  where isConId (x:xs)          = isUpper x && all isAlphaNum xs

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

genAnnot                        = noAnnot { generated = True}
name0 s                         = Name s 0 Nothing genAnnot



-- Textual name supply ---------------------------------------------------------------------------

abcSupply                       = map name0 (gensupply "abcdefghijklmnopqrstuvwxyz")

_abcSupply                      = map (name0 . ('_':)) (gensupply "abcdefghijklmnopqrstuvwxyz")

_ABCSupply                      = map (name0 . ('_':)) (gensupply "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

gensupply                       :: [Char] -> [String]
gensupply chars                 = concat (map g [1..])
  where g n                     = map (replicate n) chars


-- Internal identifier conventions -----------------------------------------------------

witnessSym                      = "w"
assumptionSym                   = "v"
tempSym                         = "x"
patSym                          = "p"
functionSym                     = "f"
dummySym                        = "d"
paramSym                        = "a"
etaExpSym                       = "eta"
tyvarSym                        = "t"
coercionSym                     = "c"
coerceLabelSym                  = "l"
coerceConstrSym                 = "K"
typeSym                         = "T"
stateTypeSym                    = "S"
skolemSym                       = "sk"
selfSym                         = "self"
thisSym                         = "this"
instanceSym                     = "inst"
tappSym                         = "TApp"
tabsSym                         = "TAbs"
gcinfoSym                       = "__GC__"

isWitness n                     = isGenerated n && str n == witnessSym
isAssumption n                  = isGenerated n && str n == assumptionSym
isEtaExp n                      = isGenerated n && str n == etaExpSym
isCoercion n                    = isGenerated n && str n == coercionSym
isPatTemp n                     = isGenerated n && str n == patSym
isDummy n                       = isGenerated n && str n == dummySym
isCoerceLabel n                 = isGenerated n && isPrefixOf coerceLabelSym (str n)
isCoerceConstr n                = isGenerated n && isPrefixOf coerceConstrSym (tail(str n))
isTApp n                        = isGenerated n && str n == tappSym
isTAbs n                        = isGenerated n && str n == tabsSym
isGCInfo n                      = isGenerated n && isPrefixOf gcinfoSym (str n)
isStateType n                   = isGenerated n && str n == stateTypeSym
explicitSyms                    = [coercionSym, assumptionSym, witnessSym]


-- Testing Names ----------------------------------------------------------------

isId (Name s _ _ _)             = isIdent (head s)
isId (Prim p _)                 = isIdPrim p
isId _                          = True

isSym i                         = not (isId i)


isCon (Name (c:_) _ _ _)        = isIdent c && isUpper c || c == ':'
isCon (Prim p _)                = isConPrim p
isCon _                         = True

isTuple (Tuple _ _)             = True
isTuple _                       = False

isVar i                         = not (isCon i)

isQual m n                      = fromMod n == Just (str m)

isGenerated (Name _ _ _ a)      = generated a 
isGenerated _                   = False

isState n                       = stateVar (annot n)

isQualified (Name _ _ (Just _) _) = True
isQualified _                    = False

isPublic (Name _ _ _ a)         = public a
isPublic _                      = True

isPrivate n                     = not (isPublic n)

isNameTag n (Name _ n' _ _)     = n == n'
isNameTag n _                   = False

isNameStr s (Name s' _ _ _)     = s == s'
isNameStr s _                   = False


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
           mod                  = if m==Nothing || suppress a then "" else fromJust m ++ "."
  show (Tuple n _)              = '(' : replicate (n-1) ',' ++ ")"
  show (Prim p _)               = strRep p

showFull (Name s n m a)		= (if m==Nothing then "" else fromJust m ++ ".") ++ s ++ "_" ++ show n
showFull n			= show n

prFullId i			= text (showFull i)

instance Pr Name where
  -- pr (Name s n m a)             = text (s ++ '_' : maybe (show n) id m)
  pr n                          = prId n

prExpl a                        = if explicit a then text "~" else empty


prId i                          = if isSym i then parens d else d
  where d			= text (show i)

prOp i                          = if isSym i then d else backQuotes d
  where d			= text (show i)

prIdV2 (Prim p _)               = text (strRepV2 p)
prIdV2 (Tuple n _)              = text ("TUP" ++ show n)
prIdV2 n                        = prId n


prIdV3 n@(Name s t m a)
  | t == 0 || isCoerceLabel n || isCoerceConstr n 
                                = text (s ++ maybe "" ('_' :) m)
{-
prIdV3 (Name s t (Just _) a)
  | not (public a)            = text (id ++ '_':show t ++ suff)
  where 
    id                          = if okForC s then s else "_sym"
    suff                        = if okForC s then "" else  "/* "++s++" */"
-}
prIdV3 (Name s n m a)           = text (id ++ tag ++ mod ++ suff)
  where 
    id                          = if okForC s then s else "_sym"
    tag                         = if mod=="" || generated a || id=="_sym"  then '_':show n else ""
    suff                        = if take 4 id == "_sym" then "/* "++s++" */" else ""
    mod                         = maybe "" ('_' :) m
prIdV3 n                        = prIdV2 n

okForC cs                       = all (\c -> isAlphaNum c || c=='_') cs

name2str n                      = render (prIdV3 n)

packName n                      = show n

unpackName x                    = case break (=='_') x of
                                    (s,"") -> let n0 = name0 s in 
                                              case lookup n0 primTerms of
                                                Just p  -> p
                                                Nothing -> n0
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
  put (Annot _ b c d e f) = put b >> put c >> put d >> put e >> put f
  get = get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> get >>= \f -> return (Annot Nothing b c d e f)


maxPrimWord = fromIntegral (fromEnum maxPrim) :: Word8

instance Binary Prim where
  put p = putWord8 (fromIntegral (fromEnum p))
  get = do
    w <- getWord8
    if w <= maxPrimWord
        then return (toEnum (fromIntegral w))
        else fail "no parse"
