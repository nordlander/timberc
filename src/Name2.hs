{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}
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

module Name2 where

import Debug.Trace
import Data.List
import PP
import Token
import Data.Char
import Data.Maybe
import Data.Typeable
import Data.Binary 

-- The type of names ---------------------------------------------------------------------

data Position                   = None
                                | Position Int Int

data Name2                      = Plain String Position
                                | Tagged String Int

data QName2                     = Q [Name2] Name2

basenameOf (Q _ n)              = n

modprefixOf (Q q _)             = q

instance Eq Name2 where
  Plain s1 _ == Plain s2 _      = s1 == s2
  Tagged s1 t1 == Tagged s2 t2  = s1 == s2 && t1 == t2

instance Eq QName2 where
  Q q1 n1 == Q q2 n2            = q1 == q2 && n1 == n2
  

qnamePos (row,col) str          = Q (init names) (last names)
  where strs                    = words (map f str)
        f '.'                   = ' '
        f c                     = c
        offsets                 = scanl (+) 0 (map ((+1) . length) strs)
        names                   = zipWith nm offsets strs
        nm off s                = Plain s (Position row (col+off))
        
namePos (row,col) str           = Plain str (Position row col)

name2 str                       = Plain str None

noqual x                        = Q [] x

qname2 str                      = Q (init names) (last names)
  where names                   = map name2 (words (map f str))
        f '.'                   = ' '
        f c                     = c

isSym2 (Plain s _)              = not (isIdent (head s))
isSym2 _                        = False

isQSym2 (Q _ n)                 = isSym2 n

prId2 n                         = if isSym2 n then parens (pr n) else pr n

prQId2 n                        = if isQSym2 n then parens (pr n) else pr n

prOp2 n                         = if isSym2 n then pr n else backQuotes (pr n)

prQOp2 n                        = if isQSym2 n then pr n else backQuotes (pr n)

instance Show Name2 where
  show                          = nstring
  
nstring (Plain "_" _)           = "_"
nstring (Plain ('_':s) _)       = '_' : '_' : s
nstring (Plain s _)             = s
nstring (Tagged s i)            = '_' : s ++ show i

instance Show QName2 where
  show                          = qnstring
  
qnstring (Q q n)                = concat (intersperse "." (map show q ++ [show n]))

instance Pr Name2 where
  pr n                          = text (show n)

instance Pr QName2 where
  pr qn                         = text (show qn)
  
instance Binary Position where
  put None                      = putWord8 0
  put (Position a b)            = putWord8 1 >> put a >> put b
  get                           = do tag_ <- getWord8
                                     case tag_ of
                                        0 -> return None
                                        1 -> get >>= \a -> get >>= \b -> return (Position a b)
                                        _ -> fail "no parse"

instance Binary Name2 where
  put (Plain a b)               = putWord8 0 >> put a >> put b
  put (Tagged a b)              = putWord8 1 >> put a >> put b
  get                           = do tag_ <- getWord8
                                     case tag_ of
                                        0 -> get >>= \a -> get >>= \b -> return (Plain a b)
                                        1 -> get >>= \a -> get >>= \b -> return (Tagged a b)
                                        _ -> fail "no parse"


instance Binary QName2 where
  put (Q a b)                   = putWord8 0 >> put a >> put b
  get                           = do tag_ <- getWord8
                                     case tag_ of
                                        0 -> get >>= \a -> get >>= \b -> return (Q a b)
                                        _ -> fail "no parse"
