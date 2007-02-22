module Kindle2C(kindle2c) where


import Common
import Kindle
import PP
import Char

kindle2c m                      = return (render h, render c)
  where h                       = k2hModule m
        c                       = k2cModule m

-- ====================================================================================================
-- Generate .h file
-- ====================================================================================================

k2hModule (Module n ds bs)      = hHeader n $$$
                                  vcat (map k2cStructStub [ n | (n, Struct te) <- ds ]) $$$
                                  vcat (map k2cDecl ds) $$$
                                  vcat (map k2cBindStub bs) $$$
                                  hFooter n

hHeader n                       = empty
hFooter n                       = empty

k2cStructStub n                 = text "struct" <+> k2cName n <> text ";"

k2cDecl (n, Enum cs)            = text "enum" <+> braces (commasep k2cName cs) <> text ";"
k2cDecl (n, Struct te)          = text "struct" <+> text "{" $$
                                    nest 4 (vcat (map k2cSig te)) $$
                                  text "}" <+> k2cName n <> text ";" $$
                                  text "typedef" <+> text "struct" <+> k2cName n <+> text "*" <> k2cName n <> text ";"

k2cSig (x, FunT ts t)           = k2cType t <+> parens (text "*" <> k2cName x) <+> parens (commasep k2cType ts) <> text";"
k2cSig (x, ValT t)              = k2cType t <+> k2cName x <> text ";"

k2cSig' (x, t)                  = k2cType t <+> k2cName x

k2cBindStub (x, Fun t te c)     = k2cType t <+> k2cName x <+> parens (commasep k2cSig' te) <> text";"
k2cBindStub (x, Val t e)        = text "extern" <+> k2cType t <+> k2cName x <> text ";"


-- Generate types
k2cType (TId n)                 = k2cName n
k2cType (TWild)                 = text "void*"


-- ====================================================================================================
-- Generate .c file
-- ====================================================================================================

k2cModule (Module n ds bs)      = cHeader n $$$
                                  vcat (map k2cBind bs) $$$
                                  cFooter n

cHeader n                       = empty
cFooter n                       = empty


                                  -- temporary, only works for non-recursive values
k2cBind (x, Val t (ENew n bs))  = k2cType t <+> k2cName x <+> text "=" <+> newCall $$
                                  vcat (map (k2cSBind (EVar x)) bs)
  where newCall                 = k2cName (prim NEW) <+> parens (text "sizeof" <> parens (text "*" <> k2cName n))
k2cBind (x, Val t e)            = k2cType t <+> k2cName x <+> text "=" <+> k2cExp e <> text ";"
k2cBind (x, Fun t te c)         = k2cType t <+> k2cName x <+> parens (commasep k2cSig' te) <+> text "{" $$
                                    nest 4 (k2cCmd c) $$
                                  text "}"


k2cSBind e0 (x, Val t e)        = k2cExp (ESel e0 x) <+> text "=" <+> k2cExp e <> text ";"
k2cSBind e0 (x, Fun t te (CRet (ECall f es)))
                                = k2cExp (ESel e0 x) <+> text "=" <+> k2cName f <> text ";"
k2cSBind e0 (x, _)              = error "Internal: k2cSBind"


k2cCmd (CRet e)                 = text "return" <+> k2cExp e <> text ";"
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


k2cAlt (ACon n c)               = k2cName n <> text ":" <+> k2cNestCmd c
k2cAlt (ALit l c)               = pr l <> text ":" <+> k2cNestCmd c


k2cNestCmd (CRet e)             = text "return" <+> k2cExp e <> text ";"
k2cNestCmd (CBreak)             = text "break;"
k2cNestCmd c                    = text "{" <+> k2cCmd c $$
                                  text "}"


k2cExp (ECall x [e1,e2])
  | isInfix x                   = k2cExp' e1 <+> k2cName x <+> k2cExp' e2
k2cExp e                        = k2cExp' e


k2cExp' (EVar x)                = k2cName x
k2cExp' (ELit l)                = pr l
k2cExp' (ESel e l)              = k2cExp' e <> text "->" <> k2cName l
k2cExp' (EEnter (EVar x) f es)  = k2cExp' (ESel (EVar x) f) <> parens (commasep k2cExp (EVar x : es))
k2cExp' e@(ECall x es)
  | isInfix x                   = parens (k2cExp e)
  | otherwise                   = k2cName x <> parens (commasep k2cExp es)
k2cExp' (ECast t e)             = parens (k2cType t) <> k2cExp' e
k2cExp' _                       = error "Internal: k2cExp"


k2cName (Prim p _)              = k2cPrim p
k2cName n                       = prId3 n


isInfix (Prim p _)              = p `elem` [IntPlus .. TimeGT] \\ ([IntNeg, FloatNeg, CharToInt, IntToChar] ++ [Sec .. Infinity])
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

k2cPrim CharToInt               = text "(int)"
k2cPrim IntToChar               = text "(char)"

k2cPrim MsgEQ                   = text "=="
k2cPrim MsgNE                   = text "!="

k2cPrim PidEQ                   = text "=="
k2cPrim PidNE                   = text "!="
                                
k2cPrim Sec                     = text "SEC"
k2cPrim MilliSec                = text "MILLISEC"
k2cPrim MicroSec                = text "MICROSEC"
k2cPrim NanoSec                 = text "NANOSEC"
k2cPrim Infinity                = text "INFINITY"
                                
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


{-

kindle2c m                      = return ((render (k2cHeader m)), (render (k2cModule m)))


data Env                        = Env { cons  :: Map Id ConType }


data ConType                    = Ordinary | Unary | Trivial | Tagged | Abstract | Enum
                                deriving (Eq, Show)


-- XXX This duplicates k2cModule's "where". Should not be this way.
k2cHeader (Module m ds fs vs)   = text "#ifndef " <> text m <> text "_h_" $$
                                  text "#define " <> text m <> text "_h_" $$
                                  header m $$$
                                  vcat  (map (k2cTDef env) ds) $$$
                                  vcat2 (map (k2cCDef env) ds) $$$
                                  vcat2 (map k2cFDecl fs) $$$
                                  vcat2 (map k2cVDecl vs) $$
                                  vcat (k2cRTSDecl ds) $$$
                                  text "#endif\n" 
  where ds'                     = ds ++ primDS
        cts0                    = [ (c, f c ve) | (c,Class ve) <- ds' ]
        cts1                    = [ (c, g c cs) | (c,Union cs) <- ds' ]
        env                     = Env { cons = cts0 ++ cts1 }
        subs                    = concat [ cs' | (_,Union cs') <- ds' ]
        f c []                  = Trivial
        f c _
          | c `elem` subs       = Tagged
          | otherwise           = Ordinary
        g c cs  | all ((==Trivial) . lookup' cts0) cs
                                = Enum
                | otherwise     = Abstract


k2cModule (Module m ds fs vs)   = header m $$
                                  -- XXX This will break with module system.
--                                text "#include \"" <> text m <> text ".h\"" $$$
                                  text "#include \"rts.h\"" $$
                                  text "#include \"defs.h\"" $$$
                                  vcat (map (k2cCImpl env) ds) $$
                                  vcat2 (map (k2cFDef env) fs) $$$
                                  k2cMain env ds fs vs
  where ds'                     = ds ++ primDS
        cts0                    = [ (c, f c ve) | (c,Class ve) <- ds' ]
        cts1                    = [ (c, g c cs) | (c,Union cs) <- ds' ]
        env                     = Env { cons = cts0 ++ cts1 }
--      env                     = Env { cons = [ (c, f c ve) | (c,Class ve) <- ds' ] ++ 
--                                             [ (c,Abstract) | (c,Union _) <- ds' ] }
        subs                    = concat [ cs' | (_,Union cs') <- ds' ]
        f c []                  = Trivial
--      f c [_]                 = Unary
        f c _
          | c `elem` subs       = Tagged
          | otherwise           = Ordinary
        g c cs  | all ((==Trivial) . lookup' cts0) cs
                                = Enum
                | otherwise     = Abstract
        
        

tagid                           = "_tag"
discrid                         = "_discr"
tagSig                          = (tagid, TPrim "int")
tagVal c                        = (tagid, Val (TPrim "int") (EVar (mkTag c)))
mkTag c                         = "_" ++ c
tmpid                           = "_ptr"

consFun c                       = "_New_" ++ c
rtsFun  c                       = "RTS_" ++ c



header m                        = text "/*" $$
                                  text " *" <+> text "Module" <+> text m $$
                                  text " */"


gcInfoArr c te' =  "static size_t " ++ c ++ "_gcinfo[" ++ (show $ gcLength te') ++ "] = {sizeof(struct " ++
                                  c ++ ")," ++ prOffsets c te'
                                  
   where
         prOffsets _ []         = "0};"
         prOffsets c ((x,t):xs) = if (ptrType t) then "offsetof(struct " ++ c ++ "," ++ x ++ "),"
                                                                                                                                                                                                 ++ (prOffsets c xs)
                                                                                                                                                                                else prOffsets c xs
         gcLength []            = 2 -- Include sizeof and null termination.
         gcLength ((x,t):xs)    = if (ptrType t) then 1 + gcLength xs else gcLength xs
         ptrType (TId c)        = True
         ptrType (TPrim c)      = False
         ptrType TPoly          = True
         ptrType t              = False

k2cTDef env (c, Union _)
  | lookup' (cons env) c==Enum  = empty
k2cTDef env (c, _)              = text "typedef" <+> text "struct" <+> text c <+> text "*" <> text c <> text ";"


k2cCDef env (c, Class [])       = empty
k2cCDef env (c, Class te)       = text "struct" <+> text c <+> text "{" $$
                                  text "#ifdef ENABLE_GC" $$
                                  nest 4 (text "size_t *gcinfo;") $$
                                  text "#endif" $$
                                  nest 4 (vcat (map k2cTSig' te')) $$
                                  text "};" $$$
                                  text "#ifdef ENABLE_GC" $$
                                  text (gcInfoArr c te') $$
                                  text "#endif\n" $$
                                  if ct /= Abstract then k2cConsDecl ct c te else empty
  where ct                      = lookup' (cons env) c
        te'                     = if ct==Tagged then tagSig:te else te
                                  
k2cCDef env (c, Union cs)
  | lookup' (cons env) c==Enum  = text "enum {" <+> commasep text tags <+> text "};"
--  | isEnum                    = text "enum {" <+> commasep text tags <+> text "};"
  | otherwise                   = k2cCDef env (c, Class [tagSig]) $$$
                                  text "enum {" <+> commasep text tags <+> text "};"
  where --isEnum                        = all ((==Trivial) . lookup' (cons env)) cs
        tags                    = map mkTag cs

k2cConsDecl ct c te             = text c <+> text (consFun c) <> parens (commasep k2cTSig te) <> text ";\n"


k2cRTSDecl ds                   = map (k2cRTSDecl' ds) (lookup'' "Env" ds)

k2cRTSDecl' ds (x, t)           = vcat (map (pDecl x) (lookup'' (show (k2cType t)) ds))
    where
         pDecl x (_, TFun ts t) = k2cType t <+> text (rtsFun x) <> parens (commasep k2cType ts) <> text ";"
         -- XXX Subtyping of Env gives us TId with coercions to pDecl.
         pDecl _ _              = text (error "pDecl: (Subtyping?) Not yet implemented")



k2cCImpl env (c, Class [])      = empty
k2cCImpl env (c, Class te)      = if ct /= Abstract then k2cConsFun ct c te else empty
    where ct                    = lookup' (cons env) c
          te'                   = if ct==Tagged then tagSig:te else te
            
k2cConsFun ct c te              = text c <+> text (consFun c) <+> parens (commasep k2cTSig te) <+> text "{" $$
                                  nest 4 (text c <+> text tmpid <+> text "=" <+>
                                          text "malloc(sizeof(struct" <+> text c <> text "));") $$
                                  text "#ifdef ENABLE_GC" $$
                                  nest 4 (text (tmpid ++ "->gcinfo = " ++ c ++ "_gcinfo;")) $$
                                  text "#endif" $$
                                  nest 4 ((if ct==Tagged then assign tagid (text (mkTag c)) else empty) $$
                                          vcat (map f te) $$
                                          text "return" <+> text tmpid <> text ";") $$
                                  text "}"
  where f (x,t)                 = assign x (text x)
        assign x exp            = text tmpid <> text "->" <> text x <+> text "=" <+> exp <> text ";"


k2cTSig (x,TFun ts t)           = k2cType t <+> parens (text "*" <> text x) <+> parens (commasep k2cType ts)
k2cTSig (x,t)                   = k2cType t <+> text x

k2cTSig' ct                     = k2cTSig ct <> text ";"


k2cType (TId c)                 = text c
k2cType (TPrim c)               = text c
k2cType TPoly                   = text "void*"
k2cType t                       = error ("Internal: k2cType" ++ show t)


k2cFDecl (x, Fun te t b)        = k2cTSig (x,t) <+> parens (commasep k2cTSig te) <> text ";"


k2cVDecl (x, Val t e)           = k2cTSig (x,t) <> text ";"

k2cVDef env (x, Val t e)        = k2cTSig (x,t) <+> text "=" <+> k2cExp env e <> text ";"


k2cFDef env (x, Fun te t b)     = k2cTSig (x,t) <+> parens (commasep k2cTSig te) <+> text "{" $$
                                  nest 4 (k2cBody env b) $$
                                  text "}"


k2cBody env (BDef fs b)         = error "Value recursion not yet supported"
k2cBody env (BLet v b)          = k2cVDef env v $$
                                  k2cBody env b
k2cBody env (BCase t (EVar x0) alts d)
                                = k2cCase1 env t x0 alts1 alts2 d
  where (alts1,alts2)           = partition isTriv alts
        isTriv (Alt c x b)      = lookup' (cons env) c == Trivial
k2cBody env (BCase t e alts d)  = k2cVDef env (discrid, Val t e) $$
                                  k2cBody env (BCase t (EVar discrid) alts d)
k2cBody env (BCase' e alts d)   = text "switch" <+> parens (k2cExp env e) <+> text "{" $$
                                  nest 2 (vcat (map (k2cAlt' env) alts) $$
                                          text "default: " <+> newScop (k2cBody env) d) $$
                                  text "}"
k2cBody env (BBreak)            = text "break;"
k2cBody env (BSeq b1 b2)        = k2cBody env b1 $$
                                  k2cBody env b2
k2cBody env (BGen v b)          = k2cBody env (BLet v b)
k2cBody env (BAss x l e b)      = text x <> text "->" <> text l <+> text "=" <+> k2cExp env e <> text ";" $$
                                  k2cBody env b
k2cBody env (BExp e)            = text "return" <+> parens (k2cExp env e) <> text ";"


k2cCase1 env t x0 [] alts2 d    = k2cCase2 env t x0 alts2 d
k2cCase1 env t x0 alts1 alts2 d = text "switch" <+> parens (k2cExp env e) <+> text "{" $$
                                  nest 2 (vcat (map (k2cTrivAlt env) alts1) $$ 
                                          text "default: " <+> newScop (k2cCase2' env t x0 alts2) d) $$
                                  text "}"
  where e                       = ECast t (TPrim "int") (EVar x0) 


k2cTrivAlt env (Alt c x b)      = text "case" <+> text (mkTag c) <> text ": " <+> newScop (k2cBody env) b


k2cCase2' env t x0 [] d         = k2cBody env d
k2cCase2' env t x0 alts d       = k2cCase2 env t x0 alts d


newScop f (BBreak)              = f BBreak
newScop f (BExp e)              = f (BExp e)
newScop f b                     = text "{" <+> f b $$
                                  text "  break;" $$
                                  text "}"


k2cCase2 env t x0 alts d        = text "switch" <+> parens (k2cExp env e) <+> text "{" $$
                                  nest 2 (vcat (map (k2cAlt env t x0) alts) $$
                                          text "default: " <+> newScop (k2cBody env) d) $$
                                  text "}"
  where e                       = ESel (EVar x0) tagid


k2cAlt env t x0 (Alt c x b)     = text "case" <+> text (mkTag c) <> text ": " <+> newScop (k2cBody env) b'
  where b'                      = BLet (x,Val (TId c) (ECast t (TId c) (EVar x0))) b


k2cAlt' env (Alt' l b)          = text "case" <+> k2cLitCase l <> text ": " <+> newScop (k2cBody env) b


k2cLitCase (Int n)              = pr (Int n)
k2cLitCase (Chr c)              = pr (Chr c)
k2cLitCase (Rat r)              = error "Floating point case analysis not yet implemented"
k2cLitCase (Str s)              = error "String case analysis not yet implemented"


k2cExp env e                    = exp2 env e


exp2 env (ECall (EVar x) es)    = exp2' env x es
exp2 env e                      = exp8 env e

exp2' env x [a,b]
  | x `elem` syms2              = exp3 env a <+> text x <+> exp2 env b
exp2' env x es                  = exp3' env x es


exp3 env (ECall (EVar x) es)    = exp3' env x es
exp3 env e                      = exp8 env e

exp3' env x [a,b]
  | x `elem` syms3              = exp4 env a <+> text x <+> exp3 env b
exp3' env x es                  = exp4' env x es


exp4 env (ECall (EVar x) es)    = exp4' env x es
exp4 env e                      = exp8 env e

exp4' env x [a,b]
  | x `elem` syms4              = exp6 env a <+> text x <+> exp6 env b
exp4' env x [e]
  | x `elem` syms4'             = text x <> exp6 env e
exp4' env x es                  = exp6' env x es

exp6 env (ECall (EVar x) es)    = exp6' env x es
exp6 env e                      = exp8 env e

exp6' env x [a,b]
  | x `elem` syms6              = exp6 env a <+> text x <+> exp7 env b
exp6' env x [e]
  | x `elem` syms6'             = text x <> exp7 env e
exp6' env x es                  = exp7' env x es


exp7 env (ECall (EVar x) es)    = exp7' env x es
exp7 env e                      = exp8 env e

exp7' env x [a,b]
  | x `elem` syms7              = exp7 env a <+> text x <+> exp8 env b
exp7' env x es                  = exp8' env x es


exp8 env (ECall (EVar x) es)    = exp8' env x es
exp8 env (ECall e es)           = parens (text "*" <> parens (exp8 env e)) <> parens (commasep (exp2 env) es)
exp8 env (ESel e l)             = exp8 env e <> text "->" <> text l
exp8 env (EVar x)               = text x
exp8 env (ELit l)               = pr l
exp8 env (ENew c [])            = text (mkTag c)
exp8 env (ENew c vs)            = exp8 env (ECall (EVar (consFun c)) (map (expOfVal . snd) vs))
-- where vs'                    = if lookup' (cons env) c == Tagged then (tagVal c) : vs else vs
exp8 env (ECast t1 t2 e)
  | t1 == tChar || t2 == tChar  = parens (k2cType t2) <> text "(int)" <> exp8 env e
  | otherwise                   = parens (k2cType t2) <> exp8 env e

exp8 env e                      = parens (exp2 env e)

exp8' env x es
  | x `elem` syms               = parens (exp2' env x es)
  | otherwise                   = text x <+> parens (commasep (exp2 env) es)


k2cMain env ds fs vs            = text "int" <+> text "main" <+> parens (text "int argc, char *argv[]") <+> text "{" $$
--                                nest 4 (vcat (map printDecl (lookup'' "Env" ds)) $$
--                                        text "Env env =" <+> text (consFun "Env") <> parens (commasep printNames (lookup'' "Env" ds)) <> text ";" $$
--                                        vcat (map (k2cFDef0 env) (map val2fun vs ++ fs)) $$
--                                        text "return RTS_go((Prog)root(env,0));") $$
                                  text "}\n"
    where
     printDecl (x, t) = k2cType t <+> text x <+> text "=" <+> text (consFun (show (k2cType t))) <> parens (text (rtsFun x)) <> text ";"
     printNames (x, t) = text x


lookup'' s [] = []
lookup'' s ((c, Class te):xs) = if c == s then te else lookup'' s xs

-}
