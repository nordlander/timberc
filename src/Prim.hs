{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             GeneralizedNewtypeDeriving #-}

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

module Prim where

import Prelude hiding (sqrt,log,exp,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh)
import Common hiding (prim,name)
import Data.Binary
import Syntax2


modname         = name2 "Prim"

prim s          = Q [modname] (name2 s)

nAction         = prim "Action"
nRequest        = prim "Request"
nClass          = prim "Class"
nCmd            = prim "Cmd"
nMsg            = prim "Msg"
nRef            = prim "Ref"
nOid            = prim "Oid"
nTime           = prim "Time"
nWorld          = prim "World"
nInt            = prim "Int"
nFloat          = prim "Float"
nChar           = prim "Char"
nBool           = prim "Bool"
nBits8          = prim "Bits8"
nBits16         = prim "Bits16"
nBits32         = prim "Bits32"
nArray          = prim "Array"
nEither         = prim "Either"
nList           = prim "[]"
nTimer          = prim "Timer"
nArrow          = prim "->"

tAction         = TCon nAction
tRequest a      = TAp (TCon nRequest) a
tClass a        = TAp (TCon nClass) a
tCmd a b        = TAp (TAp (TCon nCmd) a) b
tMsg            = TCon nMsg
tRef a          = TAp (TCon nRef) a
tOid            = TCon nOid
tTime           = TCon nTime
tWorld          = TCon nWorld
tInt            = TCon nInt
tFloat          = TCon nFloat
tChar           = TCon nChar
tBool           = TCon nBool
tBits8          = TCon nBits8
tBits16         = TCon nBits16
tBits32         = TCon nBits32
tArray a        = TAp (TCon nArray) a
tEither a b     = TAp (TAp (TCon nEither) a) b
tList a         = TAp (TCon nList) a
tTimer          = TCon nTimer
tArrow a b      = TAp (TAp (TCon nArrow) a) b
tString         = tList tChar
a `to` b        = TFun [a] b

ta              = TVar (name2 "a")
tb              = TVar (name2 "b")
tc              = TVar (name2 "c")

qa              = QVarSig (name2 "a") Star
qb              = QVarSig (name2 "b") Star
qc              = QVarSig (name2 "c") Star


mk t ns         = map (\n -> (basenameOf n, t)) ns

kenv            = mk Star                               [nAction,nMsg,nOid,nTime,nWorld,nInt,nFloat,nChar,nBool,
                                                         nBits8,nBits16,nBits32,nTimer] ++
                  mk (KFun Star Star)                   [nRequest,nClass,nRef,nArray,nList] ++
                  mk (KFun Star (KFun Star Star))       [nCmd,nEither,nArrow]

tenv            = mk (TFun [tInt,tInt] tInt)            [intPlus,intMinus,intTimes,intDiv,intMod] ++
                  mk (TFun [tInt] tInt)                 [intNeg] ++
                  mk (TFun [tInt,tInt] tBool)           [intEQ,intNE,intLT,intLE,intGT,intGE] ++

                  mk (TFun [tFloat,tFloat] tFloat)      [floatPlus,floatMinus,floatTimes,floatDiv] ++
                  mk (TFun [tFloat] tFloat)             [floatNeg] ++
                  mk (TFun [tFloat,tFloat] tBool)       [floatEQ,floatNE,floatLT,floatLE,floatGT,floatGE] ++
                  
                  mk (TFun [tOid,tOid] tBool)           [oidEQ,oidNE] ++
                  
                  mk (TFun [tBits8,tBits8] tBits8)      [and8,or8,exor8] ++
                  mk (TFun [tBits8] tBits8)             [not8] ++
                  mk (TFun [tBits8,tInt] tBits8)        [shiftl8,shiftr8,shiftra8,set8,clr8] ++
                  mk (TFun [tBits8,tInt] tBool)         [tst8] ++
                  
                  mk (TFun [tBits16,tBits16] tBits16)   [and16,or16,exor16] ++
                  mk (TFun [tBits16] tBits16)           [not16] ++
                  mk (TFun [tBits16,tInt] tBits16)      [shiftl16,shiftr16,shiftra16,set16,clr16] ++
                  mk (TFun [tBits16,tInt] tBool)        [tst16] ++
                  
                  mk (TFun [tBits32,tBits32] tBits32)   [and32,or32,exor32] ++
                  mk (TFun [tBits32] tBits32)           [not32] ++
                  mk (TFun [tBits32,tInt] tBits32)      [shiftl32,shiftr32,shiftra32,set32,clr32] ++
                  mk (TFun [tBits32,tInt] tBool)        [tst32] ++
                  
                  mk (TFun [tTime,tTime] tTime)         [timePlus,timeMinus] ++
                  mk (TFun [tTime,tTime] tBool)         [timeEQ,timeNE,timeGT,timeGE,timeLT,timeLE] ++
                  mk (TFun [tInt] tTime)                [sec,millisec,microsec,nanosec] ++
                  mk (TFun [tTime] tInt)                [secOf,microsecOf] ++
                  
                  mk (TFun [tFloat] tFloat)             [sqrt,log,log10,exp,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh] ++

                  mk (TFun [tFloat] tString)            [showFloat] ++                  
                  mk (TFun [tFloat] tInt)               [floatToInt] ++
                  mk (TFun [tInt] tFloat)               [intToFloat] ++
                  mk (TFun [tChar] tInt)                [charToInt] ++
                  mk (TFun [tInt] tChar)                [intToChar] ++
                  mk (TFun [tBits8] tInt)               [bits8ToInt] ++
                  mk (TFun [tBits16] tInt)              [bits16ToInt] ++
                  mk (TFun [tBits32] tInt)              [bits32ToInt] ++
                  mk (TFun [tInt] tBits8)               [intToBits8] ++
                  mk (TFun [tInt] tBits16)              [intToBits16] ++
                  mk (TFun [tInt] tBits32)              [intToBits32] ++

                  mk (TQual (TFun [tAction] (tCmd ta tMsg)) [qa] [])       [actToCmd] ++
                  mk (TQual (TFun [tRequest ta] (tCmd tb ta)) [qa,qb] [])  [reqToCmd] ++
                  mk (TQual (TFun [tAction] (tCmd ta tMsg)) [qa] [])       [refToOID] ++

                  mk (TQual (TFun [ta] ta) [qa] [])                                     [refl] ++
                  mk (TQual (TFun [ta `to` tb, tClass ta] (tClass tb)) [qa] [])         [classRefl] ++
                  mk (TQual (TFun [ta `to` tb, tRequest ta] (tRequest tb)) [qa] [])     [reqRefl] ++
                  mk (TQual (TFun [ta `to` tb, tCmd tc ta] (tCmd tc tb)) [qa,qb,qc] []) [cmdRefl] ++

                  mk (TQual (TFun [tTimer] (tCmd ta (TTup []))) [qa] []) [reset] ++
                  mk (TQual (tCmd ta tTime) [qa] [])                     [sample] ++
                  mk (tClass tTimer)                                     [timer] ++
                  mk (TQual (TFun [tMsg] (tCmd ta (TTup []))) [qa] [])   [abort] ++
                  mk (TQual (TFun [tInt] ta) [qa] [])                    [raise] ++
                  
                  mk (TQual (TFun [tList ta] (tArray ta)) [qa] [])       [array] ++
                  mk (TQual (TFun [ta] (tArray ta)) [qa] [])             [uniarray] ++
                  mk (TQual (TFun [tArray ta] tInt) [qa] [])             [size]

decls           = [ DData (basenameOf nBool) [] []
                        [ Constr (basenameOf false) [] [] [],
                          Constr (basenameOf true) [] [] [] ],
                    DData (basenameOf nList) [qa] [] 
                        [ Constr (basenameOf nil) [] [] [], 
                          Constr (basenameOf cons) [ta, tList ta] [] [] ],
                    DData (basenameOf nEither) [qa,qb] []
                        [ Constr (basenameOf left) [ta] [] [],
                          Constr (basenameOf right) [tb] [] [] ]
                  ]

modul           = Module modname [] (map ksig kenv ++ decls ++ map tsig tenv) []
  where 
    ksig (n,k)  = DKSig n k
    tsig (n,t)  = DSig [n] t


nil             = prim "[]"
cons            = prim ":"
false           = prim "False"
true            = prim "True"
left            = prim "Left"
right           = prim "Right"

reset           = prim "reset"
sample          = prim "sample"
timer           = prim "timer"
abort           = prim "abort"
raise           = prim "raise"

array           = prim "array"
uniarray        = prim "uniarray"
size            = prim "size"


intPlus         = prim "intPlus"
intMinus        = prim "intMinus"
intTimes        = prim "intTimes"
intDiv          = prim "intDiv"
intMod          = prim "intMod"
intNeg          = prim "intNeg"
intEQ           = prim "intEQ"
intNE           = prim "intNE"
intGT           = prim "intGT"
intGE           = prim "intGE"
intLT           = prim "intLT"
intLE           = prim "intLE"

floatPlus       = prim "floatPlus"
floatMinus      = prim "floatMinus"
floatTimes      = prim "floatTimes"
floatDiv        = prim "floatDiv"
floatNeg        = prim "floatNeg"
floatEQ         = prim "floatEQ"
floatNE         = prim "floatNE"
floatGT         = prim "floatGT"
floatGE         = prim "floatGE"
floatLT         = prim "floatLT"
floatLE         = prim "floatLE"

oidEQ           = prim "oidEQ"
oidNE           = prim "oidNE"

and8            = prim "and8"
or8             = prim "or8"
exor8           = prim "exor8"
not8            = prim "not8"
shiftl8         = prim "shiftl8"
shiftr8         = prim "shiftr8"
shiftra8        = prim "shiftra8"
set8            = prim "set8"
clr8            = prim "clr8"
tst8            = prim "tst8"

and16           = prim "and16"
or16            = prim "or16"
exor16          = prim "exor16"
not16           = prim "not16"
shiftl16        = prim "shiftl16"
shiftr16        = prim "shiftr16"
shiftra16       = prim "shiftra16"
set16           = prim "set16"
clr16           = prim "clr16"
tst16           = prim "tst16"

and32           = prim "and32"
or32            = prim "or32"
exor32          = prim "exor32"
not32           = prim "not32"
shiftl32        = prim "shiftl32"
shiftr32        = prim "shiftr32"
shiftra32       = prim "shiftra32"
set32           = prim "set32"
clr32           = prim "clr32"
tst32           = prim "tst32"

timePlus        = prim "timePlus"
timeMinus       = prim "timeMinus"
timeTimes       = prim "timeTimes"
timeDiv         = prim "timeDiv"
timeMod         = prim "timeMod"
timeEQ          = prim "timeEQ"
timeNE          = prim "timeNE"
timeGT          = prim "timeGT"
timeGE          = prim "timeGE"
timeLT          = prim "timeLT"
timeLE          = prim "timeLE"

sec             = prim "sec"
millisec        = prim "millisec"
microsec        = prim "microsec"
nanosec         = prim "nanosec"
secOf           = prim "secOf"
millisecOf      = prim "millisecOf"
microsecOf      = prim "microsecOf"
nanosecOf       = prim "nanosecOf"

sqrt            = prim "sqrt"
log             = prim "log"
log10           = prim "log10"
exp             = prim "exp"
sin             = prim "sin"
cos             = prim "cos"
tan             = prim "tan"
asin            = prim "asin"
acos            = prim "acos"
atan            = prim "atan"
sinh            = prim "sinh"
cosh            = prim "cosh"
tanh            = prim "tanh"

showFloat       = prim "showFloat"
floatToInt      = prim "floatToInt"
intToFloat      = prim "intToFloat"
charToInt       = prim "charToInt"
intToChar       = prim "intToChar"
bits8ToInt      = prim "bits8ToInt"
bits16ToInt     = prim "bits16ToInt"
bits32ToInt     = prim "bits32ToInt"
intToBits8      = prim "intToBits8"
intToBits16     = prim "intToBits16"
intToBits32     = prim "intToBits32"
actToCmd        = prim "actToCmd"
reqToCmd        = prim "reqToCmd"
refToOID        = prim "refToOID"

refl            = prim "refl"
classRefl       = prim "classRefl"
reqRefl         = prim "reqRefl"
cmdRefl         = prim "cmdRefl"

