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

module Syntax2Syntax2 where

import Common
import Syntax 
import qualified Syntax2


s2Module (Module n is ds1 ds2)     = Syntax2.Module n (map s2Import is) (s2Decls ds1) (s2Decls ds2)

s2Import (Import b n)              = Syntax2.Import b n

s2Decls (DKSig n k : ds)           = Syntax2.DKSig n k : s2Decls ds
s2Decls (DData n vs ts cs : ds)    = Syntax2.DData n vs (map s2Type ts) (map s2Constr cs) : s2Decls ds
s2Decls (DRec False n vs ts ss : ds)= Syntax2.DStruct n vs (map s2Type ts) (map s2Sig ss) : s2Decls ds
s2Decls (DRec True n vs ts ss : ds)= Syntax2.DTypeClass n vs (map s2Type ts) (map s2Sig ss) : s2Decls ds
s2Decls (DType n vs t : ds)        = Syntax2.DType n vs (s2Type t) : s2Decls ds
s2Decls (DPSig n t : ds)           = Syntax2.DInst n : Syntax2.DSig [n] (s2Type t) : s2Decls ds
s2Decls (DDefault defs : ds)       = map s2Default defs ++ s2Decls ds
s2Decls (DInstance ns : ds)        = map Syntax2.DInst ns ++ s2Decls ds
s2Decls (DInst mn t bs : ds)       = Syntax2.DInstance mn (s2Type t) (map s2Bind bs) : s2Decls ds
s2Decls (DTClass ns : ds)          = map Syntax2.DTClass ns ++ s2Decls ds
s2Decls (DBind bs : ds)            = map s2BindD bs ++ s2Decls ds
s2Decls (DExtern es : ds)          = concatMap s2Extern es ++ s2Decls ds 
s2Decls []                         = []

s2Constr (Constr n ts ps)          = Syntax2.Constr n (map s2Type ts) ps' qs
   where (ps',qs)                   = s2Preds ps

s2Sig (Sig ns t)                   = Syntax2.Sig ns (s2Type t)

s2BindD (BSig ns t)                = Syntax2.DSig ns (s2Type t)
s2BindD (BEqn lh rh)               = Syntax2.DEqn (s2Lhs lh) (s2Rhs s2Exp rh)

s2Bind (BSig ns t)                 = Syntax2.BSig ns (s2Type t)
s2Bind (BEqn lh rh)                = Syntax2.BEqn (s2Lhs lh) (s2Rhs s2Exp rh)


s2Type (TQual t ps)                = Syntax2.TQual (s2Type t) ps' qs
  where (ps',qs)                   = s2Preds ps
s2Type t                           = s2Type0 t

s2Type0 (TCon n)                    = Syntax2.TCon n
s2Type0 (TVar n)                    = Syntax2.TVar n
s2Type0 (TAp t1 t2)                 = Syntax2.TAp (s2Type0 t1) (s2Type0 t2)
s2Type0 t@(TSub _ _)                = internalError "subtype constraint used as type" t
s2Type0 TWild                       = Syntax2.TWild
s2Type0 (TList t)                   = Syntax2.TList (s2Type0 t)
s2Type0 (TTup ts)                   = Syntax2.TTup (map s2Type0 ts)
s2Type0 (TFun ts t)                 = Syntax2.TFun (map s2Type0 ts) (s2Type0 t) 

s2Preds ps                         = s2p ps [] []
   where s2p [] rs qs              = (reverse rs, reverse qs)
         s2p (PType (TSub t1 t2) : ps) rs qs
                                   = s2p ps (Syntax2.PSub (s2Type0 t1) (s2Type0 t2) : rs) qs
         s2p (PType (TQual t ps') : ps) rs qs
                                   = s2p ps (Syntax2.PQual (mkPred t) ps'' qs' : rs) qs
            where (ps'',qs')       = s2Preds ps'
                  mkPred (TSub t1 t2) = Syntax2.PSub (s2Type0 t1) (s2Type0 t2)
                  mkPred t            = Syntax2.PClass (s2Type0 t)
         s2p (PType t : ps) rs qs  = s2p ps (Syntax2.PClass (s2Type t) : rs) qs
         s2p (PKind n k : ps) rs qs= s2p ps rs (Syntax2.QVar n (Just k) : qs)

s2Default (Default _ n1 n2)        = Syntax2.DDflt [(n1,n2)]
s2Default (Derive n t)             = Syntax2.DDerive (Just n) (s2Type t)

s2Extern (Extern n t)              = [Syntax2.DExtern [n], Syntax2.DSig [n] (s2Type t)]

s2Lhs (LFun n ps)                  = Syntax2.LFun n (map s2Pat ps)
s2Lhs (LPat p)                     = Syntax2.LPat (s2Pat p)

s2Pat (PVar n)                     = Syntax2.PVar n Nothing
s2Pat (PAp (PAp p@(PCon c) p1) p2)
   | isSym c                       = Syntax2.PInfix (s2Pat p1) (Syntax2.PCon c) (s2Pat p2)
s2Pat (PAp (PAp p@(PVar x) p1) p2)
   | isSym x                       = Syntax2.PInfix (s2Pat p1) (Syntax2.PVar x Nothing) (s2Pat p2)
s2Pat (PAp p1 p2)                  = Syntax2.PAp (s2Pat p1) (s2Pat p2)
s2Pat (PCon c)                     = Syntax2.PCon c       
s2Pat (PLit lit)                   = Syntax2.PLit lit
s2Pat (PTup ps)                    = Syntax2.PTup (map s2Pat ps)
s2Pat (PList ps)                   = Syntax2.PList (map s2Pat ps)
s2Pat PWild                        = Syntax2.PWild
s2Pat (PSig (PVar n) t)            = Syntax2.PVar n (Just (s2Type t))
s2Pat (PRec mnb fs)                = Syntax2.PStruct mnb (map s2Field fs)

s2Exp (EVar n)                     = Syntax2.EVar n
s2Exp (EAp (EAp e@(EVar n) e1) e2)
   | isSym n                       = Syntax2.EInfix (s2Exp e1) (Syntax2.EVar n) (s2Exp e2)
s2Exp (EAp e1 e2)                  = Syntax2.EAp (s2Exp e1) (s2Exp e2)  -- infix expressions not identified
s2Exp (ECon c)                     = Syntax2.ECon c
s2Exp (ESel n)                     = Syntax2.ESelector n
s2Exp (ELit lit)                   = Syntax2.ELit lit
s2Exp (ETup es)                    = Syntax2.ETup (map s2Exp es)
s2Exp (EList es)                   = Syntax2.EList (map s2Exp es)
s2Exp EWild                        = error "s2Exp: argument is EWild"
s2Exp (ESig e t)                   = Syntax2.ESig (s2Exp e) (s2Type t)
s2Exp (ERec mnb fs)                = Syntax2.EStruct mnb (map mkBind fs)
   where mkBind (Field n e)        = Syntax2.BEqn (Syntax2.LPat (Syntax2.PVar n Nothing))
                                                  (Syntax2.RExp (s2Exp e) [])
s2Exp (EBStruct mnb bs)            = Syntax2.EStruct mnb (map s2Bind bs)
s2Exp (ELam ps e)                  = Syntax2.ELam (map s2Pat ps) (s2Exp e)
s2Exp (ELet bs e)                  = Syntax2.ELet (map s2Bind bs) (s2Exp e)
s2Exp (ECase e as)                 = Syntax2.ECase (s2Exp e) (map (s2Alt s2Exp) as)
s2Exp (EIf e1 e2 e3)               = Syntax2.EIf (s2Exp e1) (s2Exp e2) (s2Exp e3) 
s2Exp (ENeg e)                     = Syntax2.ENeg (s2Exp e)
s2Exp (ESeq e1 Nothing e2)         = Syntax2.ESeq (s2Exp e1) Nothing (s2Exp e2)
s2Exp (ESeq e1 (Just e2) e3)       = Syntax2.ESeq (s2Exp e1) (Just (s2Exp e2)) (s2Exp e3)
s2Exp (EComp e qs)                 = Syntax2.EComp (s2Exp e) [map s2Qual qs]
s2Exp (ESectR e n)                 = Syntax2.ESectR (s2Exp e) n
s2Exp (ESectL n e)                 = Syntax2.ESectL n (s2Exp e)
s2Exp (ESelect e n)                = Syntax2.ESel (s2Exp e) n
s2Exp (EDo mbn Nothing ss)         = Syntax2.EDo [] mbn Nothing (s2Stmts ss)
s2Exp (EDo mbn (Just (TCon c)) ss) = Syntax2.EDo [] mbn (Just c) (s2Stmts ss)
s2Exp (ETempl mbn Nothing ss)      = Syntax2.EClass [] mbn Nothing (s2Stmts ss)
s2Exp (ETempl mbn (Just (TCon c)) ss) = Syntax2.EClass [] mbn (Just c) (s2Stmts ss)
s2Exp (EAct mbn ss)                = Syntax2.EAct mbn Nothing (s2Stmts ss) 
s2Exp (EReq mbn ss)                = Syntax2.EReq mbn (s2Stmts ss) 
s2Exp (EAfter e1 e2)               = Syntax2.ESend (Just (s2Exp e1)) (s2Exp e2)
s2Exp (EBefore e1 e2)              = case s2Exp e2 of
                                        Syntax2.EAct mn _ e2' -> Syntax2.EAct mn (Just (s2Exp e1)) e2'
                                        _ -> error "before construct without action expr as 2nd argument"
s2Exp (EForall qs ss)              = Syntax2.EDo [map s2Qual qs] Nothing Nothing (s2Stmts ss)  -- forallclass
s2Exp (ENew e)                     = Syntax2.ENew [] (s2Exp e)  -- forallnew
s2Exp (EGen e)                     = Syntax2.EGen (s2Exp e)
s2Exp e@(EMatch  _)                = internalError "s2Exp: argument is" e 

s2Field (Field n p)                = Syntax2.Field n (s2Pat p)

s2Rhs :: (a -> b) -> Rhs a -> Syntax2.Rhs b
s2Rhs f (RExp a)                   = Syntax2.RExp (f a) []
s2Rhs f (RGrd ges)                 = Syntax2.RGrd (map (s2GExp f) ges) []
s2Rhs f (RWhere a bs)              = case s2Rhs f a of
                                        Syntax2.RExp a [] -> Syntax2.RExp a (map s2Bind bs)
                                        Syntax2.RGrd gs [] -> Syntax2.RGrd gs (map s2Bind bs)

s2GExp :: (a -> b) -> GExp a -> Syntax2.GExp b
s2GExp f (GExp qs a)               = Syntax2.GExp (map s2Qual qs) (f a)

s2Alt :: (a -> b) -> (Alt a) -> Syntax2.Alt b
s2Alt f (Alt p rh)                 = Syntax2.Alt (s2Pat p) (s2Rhs f rh)

s2Qual (QExp e)                    = Syntax2.QExp (s2Exp e)
s2Qual (QGen p e)                  = Syntax2.QGen (s2Pat p) (s2Exp e) 
s2Qual (QLet bs)                   = Syntax2.QLet (map s2Bind bs)

s2Stmts (Stmts ss)                 = stmts ss
  where stmts (SExp e : ss)        = Syntax2.SExp (s2Exp e) : stmts ss
        stmts (SRet e : ss)        = Syntax2.SRet (s2Exp e) : stmts ss
        stmts (SGen p e : ss)      = Syntax2.SGen (s2Pat p) (s2Exp e) : stmts ss
        stmts (SBind bs : ss)      = map (e2s . s2Bind) bs ++ stmts ss         
        stmts (SAss p e : ss)      = Syntax2.SAss (s2Pat p) (s2Exp e) : stmts ss
        stmts (SIf e ss' : ss)     = s2if (s2Exp e) (s2Stmts ss') [] ss 
        stmts (SCase e as : ss)    = Syntax2.SCase (s2Exp e) (map (s2Alt s2Stmts) as) : stmts ss
        stmts []                   = []
        -- should catch illegal elsif/else here

        e2s (Syntax2.BEqn lh rh)   = Syntax2.SEqn lh rh
        e2s (Syntax2.BSig ns t)    = Syntax2.SSig ns t

        s2if e th es (SElsif e' ss' : ss)
                                   = s2if e th ((s2Exp e', s2Stmts ss') : es) ss
        s2if e th es (SElse ss' : ss)
                                   = Syntax2.SIf e th (reverse es) (Just (s2Stmts ss')) : stmts ss
        s2if e th es ss            = Syntax2.SIf e th (reverse es) Nothing : stmts ss

