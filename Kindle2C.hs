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
                                  hFooter n
  
k2cImport n                     = text "#include \"" <> text (modToundSc (str n)) <> text ".h\""


hHeader n []                    = includeGuard n $$ text "#include \"timber.h\"" $$ text "#include \"rts.h\""
hHeader n ns                    = includeGuard n $$ vcat (map k2cImport ns)

includeGuard n                  = text ("#ifndef " ++ g) $$
	                              text ("#define " ++ g)
  where g                       = map toUpper (modToundSc (str n)) ++ "_H_"
                         
hFooter n                       = text "#endif"

k2cStructStub n                 = text "struct" <+> k2cName n <> text ";" $$
                                  text "typedef" <+> text "struct" <+> k2cName n <+> text "*" <> k2cName n <> text ";"

k2cDecl (n, Struct te cs)       = text "struct"  <+> k2cName n <+> text "{" $$
                                     nest 4 (vcat (map (k2cSig n) te)) $$
                                  text "}" <> text ";" $$
                                  k2cEnum cs

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
                                  vcat (map k2cBindStub [ b | b@(n,_) <- bs, not(isQualified n) ]) $$$
                                  vcat (map k2cBindStubActual bs2) $$$
                                  vcat (map k2cBind bs1) $$$
                                  k2cInitProc n ns bs2 $$$
                                  cFooter n
  where (bs1,bs2)               = partition k2cIsSimple bs

k2cBindStubLocal b@(x,_)
  | fromMod x /= Nothing        = empty
  | otherwise                   = k2cBindStub b

k2cBindStubActual (x, Val t _)  = k2cType t <+> k2cName x <> text ";"
k2cBindStubActual _             = empty


cHeader n                       = k2cImport n 
cFooter n                       = empty

k2cInitProc n ns bs             = text "void _init_" <> text (modToundSc (str n)) <+> text "() {" $$
	                              nest 4 (k2cOnce (vcat (map k2cInitImport ns ++ map k2cInit bs))) $$
                                  text "}"

k2cInitImport n                 = text "_init_" <> text (modToundSc (str n)) <> text "();"

k2cOnce p                       = text "static int INITIALIZED = 0;" $$
                                  text "if (!INITIALIZED) {" $$
                                  nest 4 (p $$ text "INITIALIZED = 1;") $$
                                  text "}"

                                  -- temporary, only works for non-recursive values
k2cInit (x, Val t (ENew n bs))  = newCall t e n $$
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
                                                                 text "sizeof" <> parens (text "struct" <+> k2cName n))<> text ";"

k2cSBind e0 (x, Val t (ENew n bs))
                                = newCall t e n  <> text ";" $$
                                  vcat (map (k2cSBind e) bs)
  where e                       = ESel e0 x
k2cSBind e0 (x, Val t e)        = k2cExp (ESel e0 x) <+> text "=" <+> k2cExp e <> text ";"
k2cSBind e0 (x, Fun t te (CRet (ECall f es)))
                                = k2cExp (ESel e0 x) <+> text "=" <+> k2cName f <> text ";"
k2cSBind e0 (x, _)              = error "Internal: k2cSBind"


k2cCmd (CRet e)                 = text "return" <+> k2cExp e <> text ";"
k2cCmd (CRun (ECall (Prim UpdateArray _) [e1,e2,e3,_]) c)
                                = k2cExp' (ECall (prim IndexArray) [e1,e2]) <+> text "=" <+> k2cExp e3 <> text ";" $$
                                  k2cCmd c
k2cCmd (CRun e c)               = k2cExp e <> text ";" $$
                                  k2cCmd c
k2cCmd (CBind False bs c)       = vcat (map k2cBind bs) $$
                                  k2cCmd c
k2cCmd (CAssign e x e' c)       = k2cExp (ESel e x) <+> text "=" <+> k2cExp e' <> text ";" $$
                                  k2cCmd c
k2cCmd (CSwitch e alts d)       = text "switch" <+> parens (k2cExp e) <+> text "{" $$
                                    nest 4 (vcat (map k2cAlt alts)) $$
                                    nest 4 (text "default:" <+> k2cNestCmd d) $$
                                  text "}"
k2cCmd (CSeq c c')              = k2cCmd c $$
                                  k2cCmd c'
k2cCmd (CBreak)                 = text "break;"


k2cAlt (ACon n c)               = text "case" <+> k2cTag n <> text ":" <+> k2cNestCmd c
k2cAlt (ALit l c)               = text "case" <+> pr l <> text ":" <+> k2cNestCmd c


k2cNestCmd (CRet e)             = text "return" <+> k2cExp e <> text ";"
k2cNestCmd (CBreak)             = text "break;"
k2cNestCmd c                    = text "{" <+> k2cCmd c $$
                                  text "}" $$
                                  text "break;"


k2cExp (ECall x [e1,e2])
  | isInfix x                   = k2cExp' e1 <+> k2cName x <+> k2cExp' e2
k2cExp (ECast t e)              = parens (k2cType t) <> k2cExp' e
k2cExp e                        = k2cExp' e


k2cExp' (EVar x) | isCon x      = k2cTag x
                 | otherwise    = k2cName x
k2cExp' (ELit (LRat r))         = text (show (fromRational r :: Double))
k2cExp' (ELit l)                = pr l
k2cExp' (ESel e l)              = k2cExp' e <> text "->" <> k2cName l
k2cExp' (EEnter (EVar x) f es)  = k2cExp' (ESel (EVar x) f) <> parens (commasep k2cExp (EVar x : es))
k2cExp' (ECall (Prim IndexArray _) [e1,e2])
                                = k2cExp' e1 <> text "->elems [" <+> k2cExp e2 <+> text "]"
k2cExp' (ECall (Prim SizeArray _) [e])
                                = k2cExp' e<> text "->size"
k2cExp' e@(ECall x es)
  | not (isInfix x)             = k2cName x <> parens (commasep k2cExp es)
k2cExp' e                       = parens (k2cExp e)


k2cName (Prim p _)              = k2cPrim p
k2cName n                       = prId3 n

k2cTag n                        = char '_' <> prId3 n

isInfix (Prim p _)              = p `elem` [IntPlus .. TimeGT] \\ 
                                  ([IntNeg, FloatNeg ,IntToFloat, FloatToInt, CharToInt, IntToChar] ++ [Sec .. Infinity])
isInfix _                       = False


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

k2cPrim IntToFloat              = text "(float)"
k2cPrim FloatToInt              = text "(int)"

k2cPrim CharToInt               = text "(int)"
k2cPrim IntToChar               = text "(char)"

k2cPrim LazyOr                  = text "||"
k2cPrim LazyAnd                 = text "&&"
k2cPrim MsgEQ                   = text "=="
k2cPrim MsgNE                   = text "!="

k2cPrim PidEQ                   = text "=="
k2cPrim PidNE                   = text "!="
                                
k2cPrim Sec                     = text "SEC"
k2cPrim MilliSec                = text "MILLISEC"
k2cPrim MicroSec                = text "MICROSEC"
k2cPrim NanoSec                 = text "NANOSEC"
k2cPrim Infinity                = text "INFINITY"

k2cPrim Raise                   = text "RAISE"
k2cPrim Catch                   = text "CATCH"
                                
k2cPrim TimePlus                = text "+"
k2cPrim TimeMinus               = text "-"
k2cPrim TimeMin                 = text "MIN"
                                
k2cPrim TimeEQ                  = text "=="
k2cPrim TimeNE                  = text "!="
k2cPrim TimeLT                  = text "<"
k2cPrim TimeLE                  = text "<="
k2cPrim TimeGE                  = text ">="
k2cPrim TimeGT                  = text ">"

k2cPrim p                       = text (strRep2 p)

