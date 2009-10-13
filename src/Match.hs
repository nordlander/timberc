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

{-# LANGUAGE FlexibleContexts #-}
module Match(pmc,pmc',eCase) where

import Common
import PP
import Syntax hiding (eLam,eLet,rAp)
import qualified Syntax
import Monad
import qualified List

match0, pmc :: Match r => Exp -> [Alt r] -> M s r
pmc e alts                      = do e' <- match0 e alts
                                     return (eMatch e')

match, pmc' :: Match r => [Name] -> [([Pat],Rhs r)] -> M s r
pmc' ws eqs                     = do e <- match ws eqs
                                     return (eMatch e)


-- The primitive pmc constants -----------------------------------------------------------

class (Show r, Subst r Name Exp) => Match r where
  -- Same as before introduction of class Match:
  eFail :: r
  eMatch, eCommit :: r -> r
  eFatbar :: r -> r -> r
  -- New functions:
  eCase :: Exp -> [Alt r] -> r
  eLet :: [Bind] -> r -> r
--eLam :: [Pat] -> r -> r
--rAp :: Rhs r -> [Exp] -> Rhs r

instance Match Exp where
  -- Same as before introduction of class Match
  eFatbar (EVar (Prim Fail _)) e  = e
  eFatbar e (EVar (Prim Fail _))  = e
  eFatbar e e'                    = eAp (EVar (prim Fatbar)) [e,e']
  eFail                           = EVar (prim Fail)
  eCommit e                       = EAp (EVar (prim Commit)) e
  eMatch e                        = case e of
                                      EAp (EVar (Prim Commit _)) e' -> e'
                                      ELet bs (EAp (EVar (Prim Commit _)) e') -> ELet bs e'
                                      _  -> EAp (EVar (prim Match)) e
  -- New functions:
  eCase = ECase
  eLet  = Syntax.eLet
--eLam  = Syntax.eLam
--rAp   = Syntax.rAp

instance Match Stmts where
  eFatbar (Stmts ss1) (Stmts ss2) = Stmts (ss1++ss2)
  eFail                           = Stmts [SExp eFail]
  eCommit ss                      = ss
  eMatch ss                       = ss
  --
  eCase e as                      = Stmts [SCase e as]
  eLet bs (Stmts ss)              = Stmts (SBind bs:ss)

fat []                          = return eFail
fat [m]                         = m
fat (m:ms)                      = do e1 <- m
                                     e2 <- fat ms
                                     return (eFatbar e1 e2)


-- Pattern-matching compiler proper -----------------------------------------------------

match0 (EVar w) alts            = match [w] [ ([p], rh) | Alt p rh <- alts ]
match0 e alts
  | all isTriv alts             = return (eCase e [ Alt p (RExp (eCommit r)) | Alt p (RExp r) <- alts ])
  | otherwise                   = do w <- newNamePos tempSym e
                                     e' <- match0 (EVar w) alts
                                     return (eLet [simpleEqn w e] e')
  where isTriv (Alt (PCon _) (RExp _))  = True
        isTriv _                        = False


match ws eqs                    = fat (match1 ws eqs)

match1 ws []                    = []
match1 [] (([],rhs):eqs)        = matchRhs rhs : match1 [] eqs
match1 (w:ws) eqs
  | all isVarEq eqs             = match1 ws (map f eqs)
  where f (PVar v : ps, rh)     = (ps, subst (v +-> EVar w) rh)

match1 ws (eq:eqs)
  | isSigVarEq eq               = matchVar ws eq : match1 ws eqs
  | isLitEq eq                  = matchLits ws [eq] eqs
  | isERecEq eq                 = matchRecs ws [eq] eqs
  | otherwise                   = matchCons ws [prepConEq eq] eqs

isLitEq    (p:ps,rh)            = isPLit p
isVarEq    (p:ps,rh)            = isPVar p
isSigVarEq (p:ps,rh)            = isPSigVar p
isERecEq   (p:ps,rh)            = isPRec p
isConEq    (p:ps,rh)            = isPConApp p

matchVar (w:ws) (PVar v:ps, rh) = match ws [(ps, subst (v +-> EVar w) rh)]
matchVar (w:ws) (PSig (PVar v) t : ps, rh)
                                = match ws [(ps, RWhere rh bs)]
  where bs                      = [BSig [v] t, BEqn (LFun v []) (RExp (EVar w))]


matchLits ws eqs (eq:eqs')
  | isLitEq eq                  = matchLits ws (eq:eqs) eqs'
matchLits ws eqs eqs'           = matchLit ws (reverse eqs) : match1 ws eqs'


matchLit (w:ws) eqs             = do alts <- mapM matchAlt lits
                                     return (eCase (EVar w) alts)
  where lits                    = nub [ l | (PLit l : ps, rhs) <- eqs ]
        matchAlt l              = do e' <- match ws eqs'
                                     return (Alt (PLit l) (RExp e'))
          where eqs'            = [ (ps,rhs) | (PLit l' : ps, rhs) <- eqs, l'==l ]

matchRecs ws eqs (eq:eqs')
  | isERecEq eq                 = matchRecs ws (eq:eqs) eqs'
matchRecs ws eqs eqs'           = matchRec ws (reverse eqs) : match1 ws eqs'

matchRec (w:ws) eqs             = do vs <- newNamesPos tempSym ls
                                     e <- match (vs ++ ws) (map matchAlt eqs)
                                     return (foldr eLet e (zipWith mkEqn vs ls))
  where PRec _ fs               = head (fst (head eqs))
        ls                      = nub [ l | (PRec _ fs:_,_) <- eqs, Field l p <- fs, not (isWildPat p) ]
        mkEqn v l               = [simpleEqn v (ESelect (EVar w) l)]
        matchAlt (PRec _ fs:ps,rh)
                                = ([ p | Field l p <- fs, l `elem` ls ] ++ ps, rh)


prepConEq (p:ps,rhs)            = (c, ps', ps, rhs)
  where (PCon c, ps')           = pFlat p

matchCons ws ceqs (eq:eqs')
  | isConEq eq                  = matchCons ws (prepConEq eq : ceqs) eqs'
matchCons ws ceqs eqs'          = matchCon ws (reverse ceqs) : match1 ws eqs'


matchCon  :: Match r => [Name] -> [(Name, [Pat], [Pat], Rhs r)] -> M s r
matchCon (w:ws) ceqs            = do alts <- mapM matchAlt cs
                                     return (eCase (EVar w) alts)
  where cs                      = nub [ c | (c,_,_,_) <- ceqs ]
        matchAlt c              = do vs <- newNamesPos tempSym (arity_check (map fst3 eqs_c))
                                     e  <- match (vs++ws) (map mkeq eqs_c)
                                     return (Alt (conP c (map PVar vs)) (RExp e))
          where eqs_c           = [ (ps', ps, rhs) | (c',ps',ps,rhs) <- ceqs, c==c' ]
                arity_check (ps':pss') =
                      if all ((==n).length) pss'
                      then ps'
                      else errorTree "Inconsistent number of constructor arguments in patterns"
                                     (map (conP c) (ps':pss'))
                    where
                      n=length ps'
        mkeq (ps',ps,rhs)    = (ps'++ps, rhs)


matchRhs (RExp e)               = return (eCommit e)
matchRhs (RWhere rhs bs)        = do e <- matchRhs rhs
                                     return (eLet bs e)
matchRhs (RGrd gs)              = fat [ matchQuals qs e | GExp qs e <- gs ]


matchQuals [] e                 = return (eCommit e)
matchQuals (QGen p e' : qs) e   = match0 e' [Alt p (RGrd [GExp qs e])]
matchQuals (QLet bs : qs) e     = do e' <- matchQuals qs e
                                     return (eLet bs e')
matchQuals (QExp e' : qs) e     = matchQuals (QGen trueP e' : qs) e
