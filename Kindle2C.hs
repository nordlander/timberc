module Kindle2C(kindle2c) where


import Common
import Kindle
import PP
import Char

kindle2c is m                   = return (render h, render c)
  where h                       = k2hModule m
        c                       = k2cModule is m

-- ====================================================================================================
-- Generate .h file
-- ====================================================================================================

k2hModule (Module n ns ds bs)   = hHeader n ns $$$
                                  vcat (map k2cStructStub [ n | (n, Struct te _) <- ds ]) $$$
                                  vcat (map k2cDecl ds) $$$
                                  vcat (map k2cHBind bs) $$$
                                  k2cInitProcStub n <> text ";" $$$
                                  hFooter n
  
k2cImport n                     = text "#include \"" <> text (concat(intersperse "/" (splitString (str n)))) <> text ".h\""


hHeader n []                    = includeGuard n $$ text "#include \"rts.h\"" $$ text "#include \"timber.h\""
hHeader n ns                    = includeGuard n $$ vcat (map k2cImport ns)

includeGuard n                  = text ("#ifndef " ++ g) $$
	                              text ("#define " ++ g)
  where g                       = map toUpper (modToundSc (str n)) ++ "_H_"
                         
hFooter n                       = text "#endif\n"

k2cStructStub n                 = text "struct" <+> k2cName n <> text ";" $$
                                  text "typedef" <+> text "struct" <+> k2cName n <+> text "*" <> k2cName n <> text ";"

k2cDecl (n, Struct te cs)       = text "struct"  <+> k2cName n <+> text "{" $$
                                     nest 4 (k2cGCsig $$ vcat (map (k2cSig n) te)) $$
                                  text "}" <> text ";" $$
                                  k2cEnum cs $$
                                  k2cGCinfoStub n
                                  
k2cGCinfoStub n                 = text "extern" <+> text "WORD" <+> k2cGCinfoName n <> text "[]" <> text ";"

k2cGCsig                        = text "WORD *gcinfo;"

k2cEnum []                      = empty
k2cEnum cs                      = text "enum" <+> braces (commasep k2cTag cs) <> text ";"

k2cSig n (x, FunT ts t)         = k2cType t <+> parens (text "*" <> k2cName x) <+> parens (commasep k2cType (TId n : ts)) <> text";"
k2cSig _ (x, ValT t)            = k2cType t <+> k2cName x <> text ";"

k2cSig' (x, t)                  = k2cType t <+> k2cName x

k2cBindStub (x, Fun t te c)     = k2cType t <+> k2cName x <+> parens (commasep k2cSig' te) <> text";"
k2cBindStub (x, Val t e)        = text "extern" <+> k2cType t <+> k2cName x <> text ";"

k2cHBind b@(x,_) 
  | isQualified x               = k2cBindStub b
  | otherwise                   = empty

k2cIsSimple (_,Fun _ _ _)       = True
k2cIsSimple (x,Val _ (ELit _))  = True
k2cIsSimple _                   = False

-- Generate types
k2cType (TArray t)              = k2cName (prim Array)
k2cType (TId n)                 = k2cName n
k2cType (TWild)                 = text "POLY"


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

k2cModule is (Module n ns ds bs)= cHeader n $$$
                                  vcat (map k2cGCinfo ds) $$$
                                  vcat (map k2cBindStub [ b | b@(n,_) <- bs, not(isQualified n) ]) $$$
                                  vcat (map k2cBindStubActual bs2) $$$
                                  vcat (map k2cBind bs1) $$$
                                  k2cInitProc n ns bs2 $$$
                                  cFooter n
  where (bs1,bs2)               = partition k2cIsSimple bs

k2cGCinfo (n, Struct te cs)     = text "WORD" <+> k2cGCinfoName n <> text "[]" <+> text "=" <+> 
                                  braces (k2cSize n <> text "," <+> k2cOffsets n te) <> text ";"

k2cSize n                       = text "WORDS(sizeof(struct" <+> k2cName n <> text "))"

k2cOffsets n []                 = text "0"
k2cOffsets n ((x,FunT _ _):te)  = k2cOffsets n te
k2cOffsets n ((x,ValT t):te)
  | not (isPtr t)               = k2cOffsets n te
  | otherwise                   = text "WORDS(offsetof(struct" <+> k2cName n <> text "," <+> k2cName x <> text "))," <+>
                                  k2cOffsets n te

k2cGCinfoName n                 = text "__GC__" <> k2cName n

k2cGCinit n e                   = k2cExp e <> text "->gcinfo" <+> text "=" <+> k2cGCinfoName n <> text ";"

isPtr (TId (Prim p _))          = p `elem` ptrPrims
isPtr _                         = True

k2cBindStubLocal b@(x,_)
  | fromMod x /= Nothing        = empty
  | otherwise                   = k2cBindStub b

k2cBindStubActual (x, Val t _)  = k2cType t <+> k2cName x <> text ";"
k2cBindStubActual _             = empty


cHeader n                       = text "#include \"" <> text (last(splitString (str n))) <> text ".h\"" 
cFooter n                       = text "\n"

k2cInitProcStub n               = text "void _init_" <> text (modToundSc (str n)) <+> text "()"

k2cInitProc n ns bs             = k2cInitProcStub n <+> text "{" $$
	                              nest 4 (k2cOnce (vcat (map k2cInitImport ns ++ map k2cInit bs))) $$
                                  text "}"

k2cInitImport n                 = text "_init_" <> text (modToundSc (str n)) <> text "();"

k2cOnce p                       = text "static int INITIALIZED = 0;" $$
                                  text "if (!INITIALIZED) {" $$
                                  nest 4 (p $$ text "INITIALIZED = 1;") $$
                                  text "}"

                                  -- temporary, only works for non-recursive values
k2cInit (x, Val t (ENew n bs))  = newCall t e n $$
                                  k2cGCinit n e $$
                                  vcat (map (k2cSBind e) bs)
  where e                       = EVar x
k2cInit (x, Val t e)            = k2cName x <+> text "=" <+> k2cExp e <> text ";"


k2cBind p@(x,Val t (ENew _ _))  = k2cType t <+> k2cName x <> text ";" <+> k2cInit p
k2cBind p@(x,Val t e)           = k2cType t <+> k2cInit p
k2cBind (x, Fun t te c)         = k2cType t <+> k2cName x <+> parens (commasep k2cSig' te) <+> text "{" $$
                                    nest 4 (k2cCmd c) $$
                                  text "}"


newCall t e n                   = text "NEW" <+> parens (k2cType t <> text "," <+> 
                                                                 k2cExp e <> text "," <+> 
                                                                 k2cSize n) <> text ";"


k2cSBind e0 (x, Val t (ENew n bs))
                                = newCall t e n  <> text ";" $$
                                  k2cGCinit n e $$
                                  vcat (map (k2cSBind e) bs)
  where e                       = ESel e0 x
k2cSBind e0 (x, Val t e)        = k2cExp (ESel e0 x) <+> text "=" <+> k2cExp e <> text ";"
k2cSBind e0 (x, Fun t te (CRet (ECall f es)))
                                = k2cExp (ESel e0 x) <+> text "=" <+> k2cName f <> text ";"
k2cSBind e0 (x, _)              = internalError0 "k2cSBind"


k2cCmd (CRet e)                 = text "return" <+> k2cExp e <> text ";"
k2cCmd (CRun e c)               = k2cExp e <> text ";" $$
                                  k2cCmd c
k2cCmd (CBind False [(_,Val _ (ECall (Prim UpdateArray _) [e1,e2,e3,_]))] c)
                                = k2cExp2 (ECall (prim IndexArray) [e1,e2]) <+> text "=" <+> k2cExp e3 <> text ";" $$
                                  k2cCmd c
k2cCmd (CBind False bs c)       = vcat (map k2cBind bs) $$
                                  k2cCmd c
k2cCmd (CUpd x e c)             = k2cName x <+> text "=" <+> k2cExp e <> text ";" $$
                                  k2cCmd c
k2cCmd (CUpdS e x e' c)         = k2cExp (ESel e x) <+> text "=" <+> k2cExp e' <> text ";" $$
                                  k2cCmd c
k2cCmd (CUpdA e i e' c)         = k2cExp (ECall (prim IndexArray) [e,i]) <+> text "=" <+> k2cExp e' <> text ";" $$
                                  k2cCmd c
k2cCmd (CSwitch e alts)         = text "switch" <+> parens (k2cExp e) <+> text "{" $$
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


k2cAlt (ACon n c)               = text "case" <+> k2cTag n <> text ":" <+> k2cNestCmd c
k2cAlt (ALit l c)               = text "case" <+> pr l <> text ":" <+> k2cNestCmd c
k2cAlt (AWild c)                = text "default:" <+> k2cNestCmd c


k2cNestCmd (CRet e)             = text "return" <+> k2cExp e <> text ";"
k2cNestCmd (CBreak)             = text "break;"
k2cNestCmd (CCont)              = text "continue;"
k2cNestCmd (CRaise e)           = text "RAISE" <> parens (k2cExp e) <> text ";"
k2cNestCmd c                    = text "{" <+> k2cCmd c $$
                                  text "}" $$
                                  text "break;"     -- important in case contains a switch that might break


k2cExp (ECall x [e1,e2])
  | isInfix x                   = k2cExp e1 <+> k2cName x <+> k2cExp1 e2
k2cExp e                        = k2cExp1 e


k2cExp1 (ECall x [e])
  | isUnaryOp x                 = k2cName x <> k2cExp1 e
k2cExp1 (ECast t e)             = parens (k2cType t) <> k2cExp1 e
k2cExp1 e                       = k2cExp2 e


k2cExp2 (EVar x) | isCon x      = k2cTag x
                 | otherwise    = k2cName x
k2cExp2 (ELit (LRat _ r))       = text (show (fromRational r :: Double))
k2cExp2 (ELit l)                = pr l
k2cExp2 (ESel e l)              = k2cExp2 e <> text "->" <> k2cName l
k2cExp2 (EEnter (EVar x) f es)  = k2cExp2 (ESel (EVar x) f) <> parens (commasep k2cExp (EVar x : es))
k2cExp2 (ECall (Prim IndexArray _) [e1,e2])
                                = k2cExp2 e1 <> text "->elems[" <> k2cExp e2 <> text "]"
k2cExp2 (ECall (Prim SizeArray _) [e])
                                = k2cExp2 e<> text "->size"
k2cExp2 e@(ECall x es)
  | not (isInfix x)             = k2cName x <> parens (commasep k2cExp es)
k2cExp2 EThis                   = internalError0 "k2cExp'"
k2cExp2 e                       = parens (k2cExp e)


k2cName (Prim p _)              = k2cPrim p
k2cName n                       = prId3 n

k2cTag n                        = char '_' <> prId3 n


k2cPrim IntPlus                 = text "+"
k2cPrim IntMinus                = text "-"
k2cPrim IntTimes                = text "*"
k2cPrim IntDiv                  = text "/"
k2cPrim IntMod                  = text "%"
k2cPrim IntNeg                  = text "-"

k2cPrim IntEQ                   = text "=="
k2cPrim IntNE                   = text "!="
k2cPrim IntLT                   = text "<"
k2cPrim IntLE                   = text "<="
k2cPrim IntGE                   = text ">="
k2cPrim IntGT                   = text ">"
                                
k2cPrim FloatPlus               = text "+"
k2cPrim FloatMinus              = text "-"
k2cPrim FloatTimes              = text "*"
k2cPrim FloatDiv                = text "/"
k2cPrim FloatNeg                = text "-"
                                
k2cPrim FloatEQ                 = text "=="
k2cPrim FloatNE                 = text "!="
k2cPrim FloatLT                 = text "<"
k2cPrim FloatLE                 = text "<="
k2cPrim FloatGE                 = text ">="
k2cPrim FloatGT                 = text ">"

k2cPrim IntToFloat              = text "(Float)"
k2cPrim FloatToInt              = text "(Int)"

k2cPrim CharToInt               = text "(Int)"
k2cPrim IntToChar               = text "(Char)"

k2cPrim LazyOr                  = text "||"
k2cPrim LazyAnd                 = text "&&"
k2cPrim MsgEQ                   = text "=="
k2cPrim MsgNE                   = text "!="

k2cPrim PidEQ                   = text "=="
k2cPrim PidNE                   = text "!="
                                
k2cPrim Sec                     = text "SEC"
k2cPrim Millisec                = text "MILLISEC"
k2cPrim Microsec                = text "MICROSEC"
k2cPrim Nanosec                 = text "NANOSEC"
k2cPrim Infinity                = text "INFINITY"

k2cPrim Raise                   = text "RAISE"
k2cPrim Catch                   = text "CATCH"
                                
k2cPrim TimePlus                = text "TPLUS"
k2cPrim TimeMinus               = text "TMINUS"
k2cPrim TimeMin                 = text "TMIN"
                                
k2cPrim TimeEQ                  = text "=="
k2cPrim TimeNE                  = text "!="
k2cPrim TimeLT                  = text "<"
k2cPrim TimeLE                  = text "<="
k2cPrim TimeGE                  = text ">="
k2cPrim TimeGT                  = text ">"

k2cPrim p                       = text (strRep2 p)

