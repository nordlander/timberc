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

module Syntax22Syntax where

import Common
import Syntax2 
import qualified Syntax



sModule (Module n is ds1 ds2)      = Syntax.Module n (map sImport is) (sDecls True ds1) (sDecls False ds2)

sImport (Import b n)              = Syntax.Import b n

sDecls b (DKSig n k : ds)           = Syntax.DKSig n k : sDecls b ds

sDecls b (DData n vs ts cs : ds)    = Syntax.DData n vs (map sType ts) (map sConstr cs) : sDecls b ds
sDecls b (DType n vs t : ds)        = Syntax.DType n vs (sType t) : sDecls b ds
sDecls b (DStruct n vs ts ss : ds)  = Syntax.DRec False n vs (map sType ts) (map sSig ss) : sDecls b ds

sDecls b (DTClass n : ds)           = Syntax.DTClass [n] : sDecls b ds
sDecls b (DTypeClass n vs ts ss : ds)= Syntax.DRec True n vs (map sType ts) (map sSig ss) : sDecls b ds

sDecls b (DInst n : ds)             = Syntax.DInstance [n] : sDecls b ds
sDecls b (DInstance mn t bs : ds)   = Syntax.DInst mn (sType t) (map sBind bs) : sDecls b ds
sDecls b (DDerive (Just n) t : ds)  = Syntax.DDefault [Derive n (sType t)] : sDecls b ds

sDecls b (DDefault _ : ds)          = error "sDecls: DDefault"
sDecls b (DDflt defs : ds)          = Syntax.DDefault (map (sDflt b) defs) : sDecls b ds
sDecls b (DSig ns t : ds)           = Syntax.DBind [Syntax.BSig ns (sType t)] : sDecls b ds
sDecls b (DEqn lh rh : ds)          = Syntax.DBind [Syntax.BEqn (sLhs lh) (sRhs sExp rh)] : sDecls b ds
sDecls b (DExtern [n] : DSig [m] t : ds)
  |m==n                           = Syntax.DExtern [Extern n (sType t)] : sDecls b ds
sDecls b []                         = []

sConstr (Constr n ts ps qs)       = Syntax.Constr n (map sType ts) (map sPred ps ++ map sQuant qs)


sSig (Sig ns t)                   = Syntax.Sig ns (sType t)

sDflt b (n1,n2)                   = Default b n1 n2

sQuant (QVar n)                   = Syntax.PKind n KWild
sQuant (QVarSig n k)              = Syntax.PKind n k

sBind (BSig ns t)                 = Syntax.BSig ns (sType t)
sBind (BEqn lh rh)                = Syntax.BEqn (sLhs lh) (sRhs sExp rh)


sType (TQual t ps qs)              = Syntax.TQual (sType t) (map sPred ps ++ map sQuant qs)
sType t                            = sType0 t

sType0 (TFun ts t)                 = Syntax.TFun (map sType0 ts) (sType0 t) 
sType0 (TTup ts)                   = Syntax.TTup (map sType0 ts)
sType0 (TParen t)                  = sType0 t
sType0 (TList t)                   = Syntax.TList (sType0 t)
sType0 (TAp t1 t2)                 = Syntax.TAp (sType0 t1) (sType0 t2)
sType0 (TCon n)                    = Syntax.TCon n
sType0 (TVar n)                    = Syntax.TVar n
sType0 TWild                       = Syntax.TWild

sPred p                            = Syntax.PType (sPr p)
  where sPr (PQual p ps qs)        = Syntax.TQual (sPr p) (map sPred ps ++ map sQuant qs)
	sPr (PSub t1 t2)           = Syntax.TSub (sType t1) (sType t2)
	sPr (PClass n ts)          = foldl Syntax.TAp (Syntax.TCon n) (map sType ts)

sLhs (LFun n ps)                  = Syntax.LFun n (map sPat ps)
sLhs (LPat p)                     = Syntax.LPat (sPat p)

sPat (PVar n)         		  = Syntax.PVar n 
sPat (PVarSig n t)                = Syntax.PSig (Syntax.PVar n) (sType t) 
sPat (PCon c)                     = Syntax.PCon c
sPat (PInfix p1 p p2)             = Syntax.PAp (Syntax.PAp (sPat p) (sPat p1)) (sPat p2)
sPat (PAp p1 p2)                  = Syntax.PAp (sPat p1) (sPat p2)
sPat (PLit lit)                   = Syntax.PLit lit
sPat (PTup ps)                    = Syntax.PTup (map sPat ps)
sPat (PList ps)                   = Syntax.PList (map sPat ps)
sPat PWild                        = Syntax.PWild
sPat (PStruct mnb fs)             = Syntax.PRec mnb (map sField fs)


sExp (EVar n)                     = Syntax.EVar n
sExp (EInfix e1 e e2)             = Syntax.EAp (Syntax.EAp (sExp e) (sExp e1)) (sExp e2) 
sExp (EAp e1 e2)                  = Syntax.EAp (sExp e1) (sExp e2)  -- infix expressions not identified
sExp (ECon c)                     = Syntax.ECon c
sExp (ESel e n)                   = Syntax.ESelect (sExp e) n
sExp (ESelector n)                = Syntax.ESel n
sExp (ELit lit)                   = Syntax.ELit lit
sExp (ETup es)                    = Syntax.ETup (map sExp es)
sExp (EList es)                   = Syntax.EList (map sExp es)
sExp (ESig e t)                   = Syntax.ESig (sExp e) (sType t)
sExp (EStruct mnb@(Just (c,False)) bs) = Syntax.ERec mnb (map b2Field bs)
   where b2Field (BEqn (LPat (PVar n)) (Syntax2.RExp e [])) = Syntax.Field n (sExp e)
sExp (EStruct mnb bs)             = Syntax.EBStruct mnb (map sBind bs)
sExp (ELam ps e)                  = Syntax.ELam (map sPat ps) (sExp e)
sExp (ELet bs e)                  = Syntax.ELet (map sBind bs) (sExp e)
sExp (ECase e as)                 = Syntax.ECase (sExp e) (map (sAlt sExp) as)
sExp (EIf e1 e2 e3)               = Syntax.EIf (sExp e1) (sExp e2) (sExp e3) 
sExp (ENeg e)                     = Syntax.ENeg (sExp e)
sExp (ESeq e1 Nothing e2)         = Syntax.ESeq (sExp e1) Nothing (sExp e2)
sExp (ESeq e1 (Just e2) e3)       = Syntax.ESeq (sExp e1) (Just (sExp e2)) (sExp e3)
sExp (EComp e [qs])               = Syntax.EComp (sExp e) (map sQual qs)
sExp (ESectR e n)                 = Syntax.ESectR (sExp e) n
sExp (ESectL n e)                 = Syntax.ESectL n (sExp e)
sExp (EDo [qs] Nothing Nothing ss)= Syntax.EForall (map sQual qs) (sStmts ss)
sExp (EDo [] mbn (Just c) ss)
                                  = Syntax.EDo mbn (Just (Syntax.TCon c)) (sStmts ss)
sExp (EDo [] mbn Nothing ss)      = Syntax.EDo mbn Nothing (sStmts ss)
sExp (EClass [] mbn (Just c) ss)
                                  = Syntax.ETempl mbn (Just (Syntax.TCon c)) (sStmts ss)
sExp (EClass [] mbn Nothing ss)   = Syntax.ETempl mbn Nothing (sStmts ss)
sExp (EClass [qs] mbn (Just c) ss)   = Syntax.forallClass (map sQual qs) (Syntax.ETempl mbn (Just (Syntax.TCon c)) (sStmts ss))
sExp (EClass [qs] mbn Nothing ss)    = Syntax.forallClass (map sQual qs) (Syntax.ETempl mbn Nothing (sStmts ss))
sExp (EAct mbn Nothing ss)        = Syntax.EAct mbn (sStmts ss) 
sExp (EAct mbn (Just e) ss)       = Syntax.EBefore (sExp e) (Syntax.EAct mbn (sStmts ss)) 
sExp (EReq mbn ss)                = Syntax.EReq mbn (sStmts ss) 
sExp (ESend (Just e1) e2)         = Syntax.EAfter (sExp e1) (sExp e2)    
sExp (ENew [] e)                  = Syntax.ENew (sExp e)  -- forallnew
sExp (EGen e)                     = Syntax.EGen (sExp e)
sExp e                            = error ("sExp: " ++ show e)

sField (Field n p)                = Syntax.Field n (sPat p)

sRhs :: (a -> b) -> Rhs a -> Syntax.Rhs b
sRhs f (RExp a [])                = Syntax.RExp (f a)
sRhs f (RExp a bs)                = Syntax.RWhere (Syntax.RExp (f a)) (map sBind bs)
sRhs f (RGrd ges [])              = Syntax.RGrd (map (sGExp f) ges)
sRhs f (RGrd ges bs)              = Syntax.RWhere (Syntax.RGrd (map (sGExp f) ges)) (map sBind bs)

sGExp :: (a -> b) -> GExp a -> Syntax.GExp b
sGExp f (GExp qs a)               = Syntax.GExp (map sQual qs) (f a)

sAlt :: (a -> b) -> (Alt a) -> Syntax.Alt b
sAlt f (Alt p rh)                 = Syntax.Alt (sPat p) (sRhs f rh)

sQual (QExp e)                    = Syntax.QExp (sExp e)
sQual (QGen p e)                  = Syntax.QGen (sPat p) (sExp e) 
sQual (QLet bs)                   = Syntax.QLet (map sBind bs)

sStmts ss                         = Syntax.Stmts (stmts ss)
  where stmts (SExp e : ss)        = Syntax.SExp (sExp e) : stmts ss
        stmts (SRet e : ss)        = Syntax.SRet (sExp e) : stmts ss
        stmts (SGen p e : ss)      = Syntax.SGen (sPat p) (sExp e) : stmts ss
        stmts (SEqn lh rh : ss)    = Syntax.SBind [Syntax.BEqn (sLhs lh) (sRhs sExp rh)]  : stmts ss
        stmts (SSig ns t : ss)     = Syntax.SBind [Syntax.BSig ns (sType t)] : stmts ss
        stmts (SAss p e : ss)      = Syntax.SAss (sPat p) (sExp e) : stmts ss
        stmts (SIf e th eis mbe : ss) 
                                   = Syntax.SIf (sExp e) (sStmts th) (map elsif eis) (els mbe) : stmts ss 
        stmts (SCase e as : ss)    = Syntax.SCase (sExp e) (map (sAlt sStmts) as) : stmts ss
        stmts []                   = []

        elsif (e,ss)               = (sExp e, sStmts ss)

        els (Just ss)              = Just (sStmts ss)
        els Nothing                = Nothing


