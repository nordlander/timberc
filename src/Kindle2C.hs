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

module Kindle2C(kindle2c) where


import Common
import Kindle
import PP
import Data.Char
import Depend

kindle2c m                      = return (render h, render c)
  where h                       = k2hModule m
        c                       = k2cModule m

-- ====================================================================================================
-- Generate .h file
-- ====================================================================================================

k2hModule (Module n ns es ds bs)= hHeader n ns $$$
                                  k2cDeclStubs True ds $$$
                                  k2cDecls True ds $$$
                                  k2cBindStubsH bs $$$
                                  k2cInitProcStub n <> text ";" $$$
                                  hFooter n (null es)
  
k2cImport n                     = text "#include \"" <> text (str n ++ ".h\"")


hHeader n ns                    = includeGuard n $$ text "#include \"timber.h\"" $$ vcat (map k2cImport ns) 

includeGuard n                  = text ("#ifndef " ++ g) $$
	                              text ("#define " ++ g)
  where g                       = map toUpper (name2str n) ++ "_H_"
                         
hFooter n b                     = (if b then empty else text "#include \"" <> text (str n) <> text ".extern.h\"") $$ text "#endif\n"

k2cDeclStubs isH ds             = vcat (map f ds)
  where f (n, _)
          | isH==isQualified n     = text "struct" <+> k2cName n <> text ";" $$
                                  text "typedef" <+> text "struct" <+> k2cName n <+> text "*" <> k2cName n <> text ";"
          | otherwise           = empty

k2cDecls isH ds                 = vcat (map f ds)
  where f (n, Struct [] te cs)
          | isH==isQualified n     = text "struct"  <+> k2cName n <+> text "{" $$
                                     nest 4 (k2cSigs n te) $$
                                  text "}" <> text ";"
          | otherwise           = empty
        k2cSigs n te            = vcat (map f te)
          where f (x,FunT [] ts t) 
                                = k2cType t <+> parens (text "*" <> k2cName x) <+> parens (commasep k2cType (tCon n : ts)) <> text";"
                f (x,ValT t)    = k2cType t <+> k2cName x <> text ";"


k2cFunParams te                 = parens (commasep f te)
  where f (x, t)                = k2cType t <+> k2cName x


k2cBindStubsH bs                 = vcat (map f bs)
  where f (x, _)
          | not(isQualified x)         = empty
        f (x, Fun [] t te c)    = k2cType t <+> k2cName x <+> k2cFunParams te <> text";"
        f (x, Val _ (ECall (Prim GCINFO _) _ _))
                                = text "extern" <+> text "WORD" <+> k2cGCInfoName x <> text "[];"
        f (x, Val t e)          = text "extern" <+> k2cType t <+> k2cName x <> text ";"


-- Generate types
k2cType (TCon (Prim TRUE _) _)  = k2cName (prim Bool)
k2cType (TCon (Prim FALSE _) _) = k2cName (prim Bool)
k2cType (TCon n _)              = k2cName n
k2cType (TVar _ _)              = k2cType tPOLY
k2cType (TClos _ _ _)		= k2cName (prim CLOS)



-- ====================================================================================================
-- Generate .c file
-- ====================================================================================================

{- 
  Here we need
  - stubs for private functions and values
  - bindings for all functions and for values with constant initializer
  - initialization procedure for values with non-constant initializer
  "Constant initializer" is presently interpreted as literals only for lack of better understanding.
-}

k2cModule (Module n ns es ds bs)= cHeader es n $$$
                                  k2cDeclStubs False ds $$$
                                  k2cDecls False ds $$$
                                  k2cBindStubsC bs $$$
                                  k2cTopBinds bs $$$
                                  k2cInitProc n ns es bs $$$
                                  cFooter n


k2cSize n                       = text "WORDS(sizeof(struct" <+> k2cName n <> text "))"


cHeader es n                    = text "#include \"" <> text (str n) <> text ".h\"" $$
                                  if null es then empty else text "#include \"" <> text (str n) <> text ".extern.c\""
cFooter n                       = text "\n"


k2cBindStubsC bs                = vcat (map f bs)
  where f (x, Fun [] t te c)
          | isQualified x          = empty
          | otherwise           = k2cStatic x <+> k2cType t <+> k2cName x <+> k2cFunParams te <> text";"
        f (x, Val _ (ECall (Prim GCINFO _) _ _))
          | isQualified x          = empty
          | otherwise           = k2cStatic x <+> text "WORD" <+> k2cGCInfoName x <> text "[];"
        f (x, Val t e)          = k2cStatic x <+> k2cType t <+> k2cName x <> text ";"


k2cStatic x
  | isQualified x                  = empty
  | otherwise                   = text "static"


k2cValBindStubsC bs             = vcat (map f bs)
  where f (x, Val t e)          = k2cType t <+> k2cName x <> text ";"


k2cInitProcStub n               = text "void _init_" <> text (name2str n) <+> text "()"

k2cInitImports ns               = vcat (map f ns)
  where f n                     = text "_init_" <> text (name2str n) <> text "();"

k2cOnce p                       = text "static int INITIALIZED = 0;" $$
                                  text "if (!INITIALIZED) {" $$
                                  nest 4 (p $$ text "INITIALIZED = 1;") $$
                                  text "}"


k2cInitProc n ns es bs          = k2cInitProcStub n <+> text "{" $$
	                          nest 4 (k2cOnce (k2cInitImports ns $$ 
	                                           vcat (map k2cValBinds' (groupMap bs)) $$
	                                           if null es then empty else text ("_init_external_" ++ name2str n ++ "();"))) $$
                                  text "}"
  where k2cValBinds' (r,bs)     = k2cValBinds (r, filter isInitVal bs)
        isInitVal (_,Val _ (ECall (Prim GCINFO _) _ _))
                                = False
        isInitVal (_,Val _ _)   = True
        isInitVal _             = False

k2cTopBinds bs                  = vcat (map f bs)
  where f (x, Fun [] t te c)    = k2cStatic x <+> k2cType t <+> k2cName x <+> k2cFunParams te <+> text "{" $$
                                    nest 4 (k2cCmd c) $$
                                  text "}"
        f (x, Val _ (ECall (Prim GCINFO _) [] es@(EVar n : _)))
                                = k2cStatic x <+> text "WORD" <+> k2cGCInfoName x <> text "[]" <+> text "=" <+> 
                                    braces (commasep (k2cGC n) es) <> text ";"
        f _                     = empty


k2cGC n (EVar x) | x == n       = k2cSize x
                 | otherwise    = text "WORDS(offsetof(struct" <+> k2cName n <> text "," <+> k2cName x <> text "))"
k2cGC n e                       = k2cExp e


k2cValBinds (rec,bs)
  | not rec || isSafe bs        = vcat (map f bs) $$
                                  vcat (map g bs)
  where f (x, Val _ (ENew (Prim Ref _) _ _))
                                = internalError0 "new Ref in k2cValBinds"
        f (x, Val t (ENew n [] bs)) 
                                = newCall t x [n]
        f (x, Val t (ECast _ (ENew n [] bs)))
                                = newCall t x [n]
        f (x, Val t (EClos [] _ _ _))
				= newCall t x [prim CLOS] 
        f (x, Val _ e)          = k2cName x <+> text "=" <+> k2cExp e <> text ";"
        f _                     = empty
        g (x, Val _ (ENew n [] bs)) 
                                = k2cStructBinds (EVar x) n bs
        g (x, Val _ (ECast _ (ENew n [] bs)))
                                = k2cStructBinds (ECast (tCon n) (EVar x)) n bs
        g (x, Val _ (EClos [] _ _ (CRet (ECall f [] _))))
				= k2cExp (ESel (EVar x) (prim GCINFO)) <+> text "=" <+> k2cGCInfoName (prim CLOS) <> text ";" $$
				  k2cExp (ESel (EVar x) (prim Code)) <+> text "=" <+> parens (parens (text "void(*)(void)") <> k2cName f) <> text ";"
        g _                     = empty
        vs                      = dom bs
        isSafe bs               = all isConst bs && strictBs bs `intersect` vs == []
        isConst (_, Fun _ _ _ _)                  = True
        isConst (_, Val _ (ENew _ _ _))           = True
        isConst (_, Val _ (ECast _ (ENew _ _ _))) = True
        isConst (_, Val _ (EClos _ _ _ _))        = True
        isConst _                                 = False
k2cValBinds (_,bs)              = text ("{   Array roots = CYCLIC_BEGIN(" ++ show size ++ "," ++ show n_upd ++ ");") $$
                                  nest 4 (vcat (zipWith f [0..] bs) $$
                                          vcat (zipWith3 g [0..] upd bs) $$
                                          text "CYCLIC_END(roots, hp);") $$
                                  text "}"
  where size                    = length bs
        upd                     = cyclicUpdateFlags [] bs
        n_upd                   = length (filter id upd)
        f i (x, Val t (ENew (Prim Ref _) _ _))
                                = internalError0 "new Ref in k2cValBinds"
        f i (x, Val t _)        = k2cName x <+> text "=" <+> k2cExp (rootInd' t i) <> text ";"
        g i u (x, Val t (ENew n [] bs'))
                                = update u i $$
                                  newCall t x [n] $$
                                  k2cExp (rootInd i) <+> text "=" <+> k2cExp (ECast tPOLY (EVar x)) <> text ";" $$
                                  k2cStructBinds (rootInd' t i) n bs'
        g i u (x, Val t (ECast _ (ENew n [] bs')))
                                = update u i $$
                                  newCall t x [n] $$
                                  k2cExp (rootInd i) <+> text "=" <+> k2cExp (ECast tPOLY (EVar x)) <> text ";" $$
                                  k2cStructBinds (ECast (tCon n) (rootInd' t i)) n bs'
	g i u (x, Val t (EClos [] _ _ (CRet (ECall f [] _))))
				= update u i $$
				  newCall t x [prim CLOS] $$
				  k2cExp (rootInd i) <+> text "=" <+> k2cExp (ECast tPOLY (EVar x)) <> text ";" $$
				  k2cExp (ESel (EVar x) (prim GCINFO)) <+> text "=" <+> k2cGCInfoName (prim CLOS) <> text ";" $$
				  k2cExp (ESel (EVar x) (prim Code)) <+> text "=" <+> parens (parens (text "void(*)(void)") <> k2cName f) <> text ";"
        g i u (x, Val t e)      = update u i $$
                                  k2cName x <+> text "=" <+> k2cExp e <> text ";" $$
                                  k2cExp (rootInd i) <+> text "=" <+> k2cExp (ECast tPOLY (EVar x)) <> text ";"
        update True i           = text ("CYCLIC_UPDATE(roots, " ++ show i ++ ", hp);")
        update False i          = empty
        rootInd i               = ECall (prim IndexArray) [] [ELit (lInt 0), EVar (name0 "roots"), ELit (lInt i)]
        rootInd' t i            = ECast t (rootInd i)


-- Forward and backward references within recursive bindings ------------------------------------------------

strictBs bs                     = concatMap strictB bs
strictB (_, Val _ e)		= strict e
strictB (_, Fun _ _ te c) 	= evars c \\ dom te

strict (ECast _ e)              = strict e
strict (ESel e l)               = evars e
strict (EEnter e l _ es)        = evars (e:es)
strict (ECall f _ es)           = evars es
strict (ENew _ _ bs)            = strictBs bs
strict _                        = []

strictRhs (EVar x)              = [x]
strictRhs (ECast _ e)           = strictRhs e
strictRhs e                     = strict e

cyclicUpdateFlags prev []       = []
cyclicUpdateFlags prev ((x,Val _ e):bs)
   				= mustUpdate : cyclicUpdateFlags ((x,fwrefs):prev') bs
  where computed                = dom prev
        bwrefs                  = filter (not . isPatTemp) (strictRhs e `intersect` computed)
        fragile                 = concat [ fws | (y,fws) <- prev, y `elem` bwrefs ]
        mustUpdate              = not (null (fragile `intersect` computed))
        fwrefs                  = evars e `intersect` (x:dom bs)
        prev' | mustUpdate      = [ (y,fws \\ computed) | (y,fws) <- prev ]
              | otherwise       = prev
cyclicUpdateFlags prev ((x,fn):bs)	
				= False : cyclicUpdateFlags ((x,evars fn `intersect` (x:dom bs)):prev) bs
--------------------------------------------------------------------------------------------------------------


newCall t x [n] | isBigTuple n  = text "NEW" <+> parens (k2cType t <> text "," <+> 
                                                         k2cName x <> text "," <+> 
                                                         k2cSize n <> text ("+" ++ show (widthInclTags n))) <> text ";"
newCall t x ns                  = text "NEW" <+> parens (k2cType t <> text "," <+> 
                                                         k2cName x <> text "," <+> 
                                                         sep (punctuate (text "+") (map k2cSize ns))) <> text ";"


k2cStructBinds e0 n bs
  | isBigTuple n                = k2cExp2 e0 <> text "->size = " <+> text (show (widthInclTags n)) <> text ";" $$
                                  vcat (map f bs)
  where f (Prim GCINFO _, _)    = k2cExp (ESel e0 (prim GCINFO)) <+> text "=" <+> k2cGCInfoName n <> text ";"
        f (x, Val t e)
          | t == tPOLY          = k2cBigSel e0 x <+> text "=" <+> k2cExp e <> text ";"
          | otherwise           = k2cBigSel e0 x <+> text "=" <+> k2cExp (ECast tPOLY e) <> text ";"
k2cStructBinds e0 n bs          = vcat (map f bs)
  where f (Prim GCINFO _, Val _ (ECall _ [] es))
                                = k2cExp (ESel e0 (prim GCINFO)) <+> text "=" <+> k2cGCInfoName n <> off <> text ";"
          where off | null es   = empty
                    | otherwise = text "+" <> parens (k2cExp (head es))
        f (x, Val t e)          = k2cExp (ESel e0 x) <+> text "=" <+> k2cExp e <> text ";"
        f (x, Fun [] t te (CRet (ECall f [] es)))
                                = k2cExp (ESel e0 x) <+> text "=" <+> k2cName f <> text ";"
        f (x, _)                = internalError0 "k2cSBind"



k2cCmd (CRet e)                 = text "return" <+> k2cExp e <> text ";"
k2cCmd (CRun e c)               = k2cExp e <> text ";" $$
                                  k2cCmd c
k2cCmd (CBind False [(x,Val t (ENew n [] bs))] (CBind False [(y,Val tref (ENew (Prim Ref _) [] bs'))] c))
  | st == ECast tPOLY (EVar x)  = k2cType tref <+> k2cName y <> text ";" $$
                                  newCall (tref) y [prim Ref, n] $$
                                  text "INITREF" <> parens (k2cName y) <> text ";" $$
                                  k2cStructBinds (ECast t (ESel (EVar y) (prim STATE))) n bs $$
                                  k2cCmd c
  where Val _ st                = lookup' bs' (prim STATE)
k2cCmd (CBind False bs c)       = k2cValBindStubsC bs $$
                                  k2cValBinds (False,bs) $$
                                  k2cCmd c
k2cCmd (CBind True bs c)        = k2cValBindStubsC bs $$
                                  vcat (map k2cValBinds (groupMap bs)) $$
                                  k2cCmd c
k2cCmd (CUpd x e c)             = k2cName x <+> text "=" <+> k2cExp e <> text ";" $$
                                  k2cCmd c
k2cCmd (CUpdS e x e' c)         = k2cExp (ESel e x) <+> text "=" <+> k2cExp e' <> text ";" $$
                                  k2cCmd c
k2cCmd (CUpdA e i e' c)         = k2cExp (ECall (prim IndexArray) [] [ELit (lInt 0),e,i]) <+> text "=" <+> k2cExp e' <> text ";" $$
                                  k2cCmd c
k2cCmd (CSwitch e alts)         = case litType (firstLit alts) of
                                    TCon (Prim LIST _) [TCon (Prim Char _) []] -> -- we know (from Prepare4C) that e is a variable
                                      k2cStringAlts False e alts
                                    TCon (Prim Float _) [] -> 
                                      k2cFloatAlts False e alts
                                    _ -> text "switch" <+> parens (k2cExp e) <+> text "{" $$
                                         nest 4 (vcat (map k2cAlt alts)) $$
                                         text "}"
k2cCmd (CSeq c c')              = k2cCmd c $$
                                  k2cCmd c'
k2cCmd (CBreak)                 = text "break;"
k2cCmd (CRaise e)               = text "RAISE" <> parens (k2cExp e) <> text ";"
k2cCmd (CWhile e c c')          = text "while" <+> parens (k2cExp e) <+> text "{" $$
                                  nest 4 (k2cCmd c) $$
                                  text "}" $$
                                  k2cCmd c'
k2cCmd (CCont)                  = text "continue;"

firstLit (ALit l _ : _) = l
firstLit (_ : as)       = firstLit as

k2cStringAlts b e (ALit l c : as)      
                                = (if b then text "else " else empty) <> text "if (strEq (" <> k2cExp e <>
                                    text "," <> k2cExp (ELit l) <> text ")) {" $$
                                     nest 4 (k2cNestIfCmd c) $$
                                     text "}" $$
                                     k2cStringAlts True e as
k2cStringAlts _ e [AWild c]       = text "else {" $$
                                     nest 4 (k2cNestIfCmd c) $$
                                     text "}"

k2cFloatAlts b e (ALit l c : as)      
                                  = (if b then text "else " else empty) <> text "if (" <> k2cExp e <>
                                    text "=="<> k2cExp (ELit l) <> text ") {" $$
                                     nest 4 (k2cNestIfCmd c) $$
                                     text "}" $$
                                     k2cFloatAlts True e as
k2cFloatAlts _ e [AWild c]       = text "else {" $$
                                     nest 4 (k2cNestIfCmd c) $$
                                     text "}"
         
k2cAlt (ACon n _ _ c)           = internalError "Constructor tag in Kindle2C" n
k2cAlt (ALit l c)               = text "case" <+> pr l <> text ":" <+> k2cNestCmd c
k2cAlt (AWild c)                = text "default:" <+> k2cNestCmd c


k2cNestCmd (CRet e)             = text "return" <+> k2cExp e <> text ";"
k2cNestCmd (CBreak)             = text "break;"
k2cNestCmd (CCont)              = text "continue;"
k2cNestCmd (CRaise e)           = text "RAISE" <> parens (k2cExp e) <> text ";"
k2cNestCmd c                    = text "{" <+> k2cCmd c $$
                                  text "}" $$
                                  text "break;"     -- important in case contains a switch that might break

k2cNestIfCmd (CRet e)             = text "return" <+> k2cExp e <> text ";"
k2cNestIfCmd (CBreak)             = empty
k2cNestIfCmd (CCont)              = text "continue;"
k2cNestIfCmd (CRaise e)           = text "RAISE" <> parens (k2cExp e) <> text ";"
k2cNestIfCmd c                    = text "{" <+> k2cCmd c $$
                                    text "}" 


k2cExp (ECall x [] [e1,e2])
  | isInfix x                   = parens (k2cExp e1 <+> k2cName x <+> k2cExp1 e2)
k2cExp e                        = k2cExp1 e


k2cExp1 (ECall x [] [e])
  | isUnaryOp x                 = k2cName x <> k2cExp1 e
k2cExp1 (ECast t e)             = parens (k2cType t) <> k2cExp1 e
k2cExp1 e                       = k2cExp2 e


k2cExp2 (EVar x)                = k2cName x
k2cExp2 (ELit (LRat _ r))       = text (show (fromRational r :: Double))
k2cExp2 (ELit (LStr _ str))     = text "getStr(\"" <> hcat (map cChar str) <> text "\")"
k2cExp2 (ELit (LChr _ c))       = text "\'" <> cChar c <> text "\'"
k2cExp2 (ELit l)                = pr l
k2cExp2 (ESel e (Prim STATE _)) = text "STATEOF" <> parens (k2cExp e)
k2cExp2 (ESel (ECast (TCon n _) e) l)
  | isBigTuple n                = k2cBigSel e l
k2cExp2 (ESel e l)              = k2cExp2 e <> text "->" <> k2cName l
k2cExp2 (EEnter (ECast (TClos [] ts t) (EVar x)) (Prim Code _) [] es)
                                = parens (parens ftype <> parens (k2cName x <> text "->" <> k2cName (prim Code)))
                                  <> parens (commasep k2cExp (EVar x : es))
  where ftype                   = k2cType t <> text "(*)" <> parens (hsep (punctuate comma (k2cName (prim CLOS) : map k2cType ts)))
k2cExp2 (EEnter (EVar x) f [] es)  
                                = k2cExp2 (ESel (EVar x) f) <> parens (commasep k2cExp (EVar x : es))
k2cExp2 (ECall (Prim IndexArray _) [] [_,e1,e2])
                                = k2cExp2 e1 <> text "->elems[" <> k2cExp e2 <> text "]"
k2cExp2 (ECall (Prim SizeArray _) [] [_,e])
                                = k2cExp2 e<> text "->size"
k2cExp2 e@(ECall x [] es)
  | not (isInfix x)             = k2cName x <> parens (commasep k2cExp es)
k2cExp2 EThis                   = internalError0 "k2cExp'"
k2cExp2 e                       = parens (k2cExp e)


isBigTuple n                    = isTuple n && width n > 4

k2cBigSel e l                   = k2cExp2 e <> text "->elems[" <> text (show (lookup' bigLabelDict l)) <> text "]"

bigLabelDict                    :: [(Name,Int)]
bigLabelDict                    = f _ABCSupply abcSupply `zip` [0..]
  where f (x:xs) ys             = let (ys1,ys2) = splitAt 32 ys in x : ys1 ++ f xs ys2

widthInclTags n                 = w + ((w+31) `div` 32)
  where w                       = width n


k2cName (Prim p _)              = k2cPrim p
k2cName n | isBigTuple n        = text "TUPLE"
          | otherwise           = prIdV3 n

k2cGCInfoName n                 = text gcinfoSym <> k2cName n


k2cPrim IntToFloat              = text "(Float)"
k2cPrim FloatToInt              = text "(Int)"

k2cPrim CharToInt               = text "(Int)"
k2cPrim IntToChar               = text "(Char)"

k2cPrim Raise                   = text "Raise"
k2cPrim Abort                   = text "ABORT"
k2cPrim CLOS			= text "CLOS"

k2cPrim p                       = cSym p