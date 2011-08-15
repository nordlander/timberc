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

module SyntaxConv where

import Common
import PP
import Syntax2 
import qualified Syntax


syntaxconv m                       = return (sModule m)

sModule (Module n is ds1 ds2)      = Syntax.mkModule (sName n) (map sImport is, sDecls True ds1, sDecls False ds2)

sImport (Import n)                 = Syntax.Import True (sQName n)
sImport (Use Nothing n)            = Syntax.Import False (sQName n)

sDecls b (DKSig n k : ds)           = Syntax.DKSig (sName n) k : sDecls b ds

sDecls b (DData n vs ts cs : ds)    = Syntax.DData (sName n) (map sQuant2Name vs) (map sType ts) (map sConstr cs) : sDecls b ds
sDecls b (DType n vs t : ds)        = Syntax.DType (sName n) (map sQuant2Name vs) (sType t) : sDecls b ds
sDecls b (DStruct n vs ts ss : ds)  = Syntax.DRec False (sName n) (map sQuant2Name vs) (map sType ts) (map sSig ss) : sDecls b ds

sDecls b (DTClass n : ds)           = Syntax.DTClass [sQName n] : sDecls b ds
sDecls b (DTypeClass n vs ts ss : ds)= Syntax.DRec True (sName n) (map sQuant2Name vs) (map sType ts) (map sSig ss) : sDecls b ds

sDecls b (DInst n : ds)             = Syntax.DInstance [sQName n] : sDecls b ds
sDecls b (DInstance mn t bs : ds)   = Syntax.DInst (sMN mn) (sType t) (map sBind bs) : sDecls b ds
sDecls b (DDerive (Just n) t : ds)  = Syntax.DDefault [Derive (sName n) (sType t)] : sDecls b ds

sDecls b (DDefault defs : ds)       = Syntax.DDefault (map (sDflt b) defs) : sDecls b ds
sDecls b (DExtern [n] : DSig [m] t : ds)
  | m == n                          = Syntax.DExtern [Extern (sName n) (sType t)] : sDecls b ds
sDecls b (DSig [m] t : DExtern [n] : ds)
  | m == n                          = Syntax.DExtern [Extern (sName n) (sType t)] : sDecls b ds
sDecls b (DExternSig ns t : ds)     = Syntax.DExtern [Extern (sName n) (sType t) | n <- ns ] : sDecls b ds
sDecls b (DSig ns t : ds)           = Syntax.DBind [Syntax.BSig (map sName ns) (sType t)] : sDecls b ds
sDecls b (DEqn lh rh : ds)          = Syntax.DBind [Syntax.BEqn (sLhs lh) (sRhs sExp rh)] : sDecls b ds
sDecls b []                         = []

sConstr (Constr n ts qs ps)       = Syntax.Constr (sName n) (map sType ts) (map sPred ps ++ map sQuant qs)
sConstr (CInfix t n t' qs ps)     = Syntax.Constr (sName n) (map sType [t,t']) (map sPred ps ++ map sQuant qs)


sSig (Sig ns t)                   = Syntax.Sig (map sName ns) (sType t)

sDflt b (n1,n2)                   = Default b (sQName n1) (sQName n2)

sQuant2Name (QVar n)              = sName n
sQuant2Name (QVarSig n _)         = sName n

sQuant (QVar n)                   = Syntax.PKind (sName n) KWild
sQuant (QVarSig n k)              = Syntax.PKind (sName n) k

sBind (BSig ns t)                 = Syntax.BSig (map sName ns) (sType t)
sBind (BEqn lh rh)                = Syntax.BEqn (sLhs lh) (sRhs sExp rh)


sType (TQual t qs ps)              = Syntax.TQual (sType t) (map sPred ps ++ map sQuant qs)
sType t                            = sType0 t

sType0 (TFun ts t)                 = Syntax.TFun (map sType0 ts) (sType0 t) 
sType0 (TTup ts)                   = Syntax.TTup (map sType0 ts)
sType0 (TTupC n)                   = Syntax.TCon (tuple n)
sType0 (TParen t)                  = sType0 t
sType0 (TList t)                   = Syntax.TList (sType0 t)
sType0 (TAp t1 t2)                 = Syntax.TAp (sType0 t1) (sType0 t2)
sType0 (TCon n)                    = Syntax.TCon (sQName n)
sType0 (TVar n)                    = Syntax.TVar (sName n)
sType0 TWild                       = Syntax.TWild

sPred p                            = Syntax.PType (sPr p)
  where sPr (PQual p qs ps)        = Syntax.TQual (sPr p) (map sPred ps ++ map sQuant qs)
	sPr (PSub t1 t2)           = Syntax.TSub (sType t1) (sType t2)
	sPr (PClass n ts)          = foldl Syntax.TAp (Syntax.TCon (sQName n)) (map sType ts)

sLhs p0				  = flat p0 []
  where flat (PInfix p1 op p2) ps = flat op (p1:p2:ps)
        flat (PAp p p') ps        = flat p (p':ps)
        flat (PVar n) ps          = Syntax.LFun (sName n) (map sPat ps)
        flat _ _                  = Syntax.LPat (sPat p0)

sPat (PVar n)         		  = Syntax.PVar (sName n) 
sPat (PVarSig n t)                = Syntax.PSig (Syntax.PVar (sName n)) (sType t) 
sPat (PCon c)                     = Syntax.PCon (sQName c)
sPat (PInfix p1 p p2)             = Syntax.PAp (Syntax.PAp (sPat p) (sPat p1)) (sPat p2)
sPat (PIndex p e)                 = Syntax.PAp (Syntax.PAp (Syntax.PVar (prim IndexArray)) (sPat p)) (sPat (exp2pat e))
sPat (PAp p1 p2)                  = Syntax.PAp (sPat p1) (sPat p2)
sPat (PLit lit)                   = Syntax.PLit lit
sPat (PTup ps)                    = Syntax.PTup (map sPat ps)
sPat (PTupC n)                    = Syntax.PCon (tuple n)
sPat (PList ps)                   = Syntax.PList (map sPat ps)
sPat PWild                        = Syntax.PWild
sPat (PStruct mnb fs)             = Syntax.PRec (sMNB mnb) (map sField fs)
sPat (PParen p)			  = sPat p
sPat p                            = error ("#####PAT: " ++ render (pr p))


sExp (EVar n)                     = Syntax.EVar (sQName n)
sExp (EInfix e1 e e2)             = Syntax.EAp (Syntax.EAp (sExp e) (sExp e1)) (sExp e2) 
sExp (EIndex e e')                = Syntax.EAp (Syntax.EAp (Syntax.EVar (prim IndexArray)) (sExp e)) (sExp e')
sExp (EOr e1 e2)		  = Syntax.EAp (Syntax.EAp (Syntax.EVar (prim LazyOr)) (sExp e1)) (sExp e2)
sExp (EAnd e1 e2)		  = Syntax.EAp (Syntax.EAp (Syntax.EVar (prim LazyAnd)) (sExp e1)) (sExp e2)
sExp (EAp e1 e2)                  = Syntax.EAp (sExp e1) (sExp e2)  -- infix expressions not identified
sExp (ECon c)                     = Syntax.ECon (sQName c)
sExp (ESel e n)                   = Syntax.ESelect (sExp e) (sQName n)
sExp (ESelector n)                = Syntax.ESel (sQName n)
sExp (ELit lit)                   = Syntax.ELit lit
sExp (ETup es)                    = Syntax.ETup (map sExp es)
sExp (ETupC n)                    = Syntax.ECon (tuple n)
sExp (EList es)                   = Syntax.EList (map sExp es)
sExp (ESig e t)                   = Syntax.ESig (sExp e) (sType t)
sExp (EStruct (Just (c,False)) bs) = Syntax.ERec (Just (sQName c,False)) (map b2Field bs)
   where b2Field (BEqn (PVar n) (Syntax2.RExp e [])) = Syntax.Field (sName n) (sExp e)
sExp (EStruct mnb bs)             = Syntax.EBStruct (sMNB mnb) (map sBind bs)
sExp (ELam ps e)                  = Syntax.ELam (map sPat ps) (sExp e)
sExp (ELet bs e)                  = Syntax.ELet (map sBind bs) (sExp e)
sExp (ECase e as)                 = Syntax.ECase (sExp e) (map (sAlt sExp) as)
sExp (EIf e1 e2 e3)               = Syntax.EIf (sExp e1) (sExp e2) (sExp e3) 
sExp (ENeg e)                     = Syntax.ENeg (sExp e)
sExp (ESeq e1 Nothing e2)         = Syntax.ESeq (sExp e1) Nothing (sExp e2)
sExp (ESeq e1 (Just e2) e3)       = Syntax.ESeq (sExp e1) (Just (sExp e2)) (sExp e3)
sExp (EComp e [qs])               = Syntax.EComp (sExp e) (map sQual qs)
sExp (ESectR e (EVar n))          = Syntax.ESectR (sExp e) (sQName n)
sExp (ESectR e (ECon n))          = Syntax.ESectR (sExp e) (sQName n)
sExp (ESectL (EVar n) e)          = Syntax.ESectL (sQName n) (sExp e)
sExp (ESectL (ECon n) e)          = Syntax.ESectL (sQName n) (sExp e)
sExp (EDo [qs] Nothing Nothing ss)= Syntax.EForall (map sQual qs) (sStmts ss)
sExp (EDo [] mn (Just c) ss)
                                  = Syntax.EDo (sMN mn) (Just (Syntax.TCon (sQName c))) (sStmts ss)
sExp (EDo [] mn Nothing ss)       = Syntax.EDo (sMN mn) Nothing (sStmts ss)
sExp (EClass [] mn (Just c) ss)
                                  = Syntax.ETempl (sMN mn) (Just (Syntax.TCon (sQName c))) (sStmts ss)
sExp (EClass [] mn Nothing ss)    = Syntax.ETempl (sMN mn) Nothing (sStmts ss)
sExp (EClass [qs] mn (Just c) ss) = Syntax.forallClass (map sQual qs) (Syntax.ETempl (sMN mn) (Just (Syntax.TCon (sQName c))) (sStmts ss))
sExp (EClass [qs] mn Nothing ss)  = Syntax.forallClass (map sQual qs) (Syntax.ETempl (sMN mn) Nothing (sStmts ss))
sExp (EAct Nothing mn _ ss)       = Syntax.EAct (sMN mn) (sStmts ss) 
sExp (EAct (Just e) mn _ ss)      = Syntax.EBefore (sExp e) (Syntax.EAct (sMN mn) (sStmts ss)) 
sExp (EReq mn _ ss)               = Syntax.EReq (sMN mn) (sStmts ss) 
sExp (ESend e1 e2)                = Syntax.EGen (sSend e1 e2)
sExp (ENew [] e)                  = Syntax.ENew (sExp e)  -- forallnew
sExp (ENew [qs] e)                = Syntax.ENew (Syntax.forallClass (map sQual qs) (sExp e))
sExp (EGen e)                     = Syntax.EGen (sExp e)
sExp (EParen e)			  = sExp e
sExp e                            = error ("sExp: " ++ show e)

sSend Nothing e2                  = sExp e2
sSend (Just e1) e2                = Syntax.EAfter (sExp e1) (sExp e2)

sField (Field n p)                = Syntax.Field (sQName n) (sPat p)

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
  where stmts (SExp (ESend e1 e2) : ss) = Syntax.SExp (sSend e1 e2) : stmts ss
        stmts (SExp e : ss)        = Syntax.SExp (sExp e) : stmts ss
        stmts (SGen p e : ss)      = Syntax.SGen (sPat p) (sExp e) : stmts ss
        stmts (SRes e : ss)        = Syntax.SRet (sExp e) : stmts ss
        stmts (SEqn lh rh : ss)    = Syntax.SBind [Syntax.BEqn (sLhs lh) (sRhs sExp rh)]  : stmts ss
        stmts (SSig ns t : ss)     = Syntax.SBind [Syntax.BSig (map sName ns) (sType t)] : stmts ss
        stmts (SLet bs : ss)       = Syntax.SBind (map sBind bs) : stmts ss
        stmts (SAss p e : ss)      = Syntax.SAss (sPat p) (sExp e) : stmts ss
        stmts (SIf e th eis mbe : ss) 
                                   = Syntax.SIf (sExp e) (sStmts th) (map elsif eis) (els mbe) : stmts ss 
        stmts (SCase e as : ss)    = Syntax.SCase (sExp e) (map (sAlt sStmts) as) : stmts ss
        stmts []                   = []

        elsif (e,ss)               = (sExp e, sStmts ss)

        els (Just ss)              = Just (sStmts ss)
        els Nothing                = Nothing


sName (Plain s pos)             = Name { str = s, tag = 0, fromMod = Nothing, annot = posAnnot pos }
sName (Tagged s i)              = Name { str = s, tag = i, fromMod = Nothing, annot = noAnnot }


posAnnot pos                    = noAnnot { location = sPos pos }

sQName (Q [Plain "Prim" _] (Plain s _))
                                = sPrim s
sQName (Q [] nm)                = sName nm
sQName (Q mods nm)              = nm' { annot = posAnnot (head [ p | Plain _ p <- mods ]),
                                        fromMod = Just (concat . intersperse "." . map show $ mods) }
  where nm'                     = sName nm

sPos None                       = Nothing
sPos (Position r c)             = Just (r,c)

sMNB Nothing                    = Nothing
sMNB (Just (n,b))               = Just (sQName n,b)

sMN Nothing                     = Nothing
sMN (Just n)                    = Just (sName n)

sPrim s                         = case lookup s primMap of
                                    Just nm -> nm
                                    Nothing -> error ("sPrim: " ++ s)

primMap                           = [ (strip (strRep p), prim p) | p <- [minPrim..maxPrim] ]
  where strip ('p':'r':'i':'m':s) = s
        strip s                   = s

s2QN (Name s 0 Nothing _)       = qname2 s
s2QN (Name s 0 (Just m) _)      = Q (map name2 mods) (name2 s)
  where mods                    = words (map f m)
        f '.'                   = ' '
        f c                     = c
        