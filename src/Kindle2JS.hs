module Kindle2JS where
        
import Data.Char
import Common
import Kindle
import PP
import Depend

kindle2js m                     = return (render (k2jModule m))

k2jModule (Module n ns es _ bs) = k2jExtern n es $$ vcat (map k2jBinds (groupMap bs))
        
k2jExtern n []			= empty
k2jExtern n _			= empty -- text "includeScript" <> parens (text ("'" ++ modToPath (str n) ++ ".extern.js'"))

k2jBinds (_, bs)            = vcat (map k2jBind bs)
k2jBinds (True, bs)             = vcat (map k2jInitBind bs) $$
				  vcat (map k2jUpdateBind bs)

k2jBind (x, Fun _ _ te c)       = text "function" <+> k2jName x <+> parens (commasep k2jName (dom te)) <+> text "{" $$
                                  nest 4 (k2jCmd c) $$
                                  text "}"
k2jBind (x, Val _ e)            = text "var" <+> k2jName x <+> text "=" <+> k2jExp e


k2jInitBind (x, Val _ _)	= text "var" <+> k2jName x <+> text "= {}"
k2jInitBind (x, Fun _ _ _ _)	= empty

k2jUpdateBind (x, Val _ e)	= k2jName x <+> text "= UPDATE" <> parens (k2jName x <> comma <+> k2jExp e)
k2jUpdateBind b			= k2jBind b


specialFuns 			= map prim [CharToInt, TimeMin, SecOf, MicrosecOf, MUTLISTINIT, MUTLISTEXTEND, MUTLISTEXTRACT]


k2jExp (ECall (Prim CharToInt _) _ [e])
				= k2jExp2 e <> text ".charCodeAt(0)"
k2jExp (ECall (Prim TimeMin _) _ [e1,e2])
				= k2jExp1 e1 <+> text "<=" <+> k2jExp1 e2 <> text "?" <+> k2jExp1 e1 <+> text ":" <+> k2jExp1 e2
k2jExp (ECall (Prim SecOf _) _ [e])
				= text "Math.floor" <> parens (k2jExp1 e <+> text "/ 1000")
k2jExp (ECall (Prim MicrosecOf _) _ [e])
				= parens (k2jExp1 e <+> text "% 1000") <+> text "* 1000"
k2jExp (ECall (Prim MUTLISTINIT _) _ _)
				= text "[]"
k2jExp (ECall (Prim MUTLISTEXTEND _) _ [e,e'])
				= k2jExp e <> text ".push" <> parens (k2jExp e')
k2jExp (ECall (Prim MUTLISTEXTRACT _) _ [e])
				= k2jExp e
k2jExp (ECall x _ [e1,e2])
  | isJsInfix x        		= k2jExp1 e1 <+> k2jName x <+> k2jExp1 e2
k2jExp (ELit l)                 = k2jLit l
k2jExp e                        = k2jExp1 e


k2jExp1 (ECall x _ [e])
  | isUnaryOp x && isSym x      = k2jName x <> k2jExp1 e
k2jExp1 (ECast t e)             = k2jExp1 e
k2jExp1 e                       = k2jExp2 e


k2jExp2 (EVar x)                = k2jName x
k2jExp2 (EThis)                 = text "this"
k2jExp2 (ELit l)                = k2jLit l
k2jExp2 (ECall x _ es)
  | not (isJsInfix x) && x `notElem` specialFuns
          			= k2jName x <> parens (commasep k2jExp es)
k2jExp2 (ESel e l)
  | l == selH			= k2jExp2 e <> text "[0]"
  | l == selT			= k2jExp2 e <> text ".slice(1)"
  | isCoerceLabel l		= k2jExp2 e
  | l == prim STATE		= k2jExp2 e
  | otherwise   	        = k2jExp2 e <> text "." <> k2jSelName l
k2jExp2 (EEnter (ECall (Prim ClassRefl _) _ _) (Prim Code _) _ [e])
				= k2jExp2 e
k2jExp2 (EEnter e (Prim Code _) _ es)
				= k2jExp2 e <> parens (commasep k2jExp es)
k2jExp2 (EEnter e x _ es)       = k2jExp2 e <> text "." <> k2jSelName x <> parens (commasep k2jExp es)
k2jExp2 (ENew (Prim NIL _) _ _) = text "[]"
k2jExp2 (ENew (Prim CONS _) _ bs)
				= k2jList [headExp bs] (tailExp bs)
k2jExp2 (ENew x _ [(Prim Code _,Fun _ _ te (CRet e))])
				= text "function" <+> parens (commasep k2jName (dom te)) <+> text "{" <+> text "return" <+> k2jExp e <+> text "}"
k2jExp2 (ENew x _ [(Prim Code _,Fun _ _ te c)])
				= text "function" <+> parens (commasep k2jName (dom te)) <+> text "{" $$
				  nest 4 (k2jCmd c) $$
				  text "}"
k2jExp2 (ENew x _ [(_,Val _ e)])
  | isCoerceConstr x		= k2jExp2 e
k2jExp2 (ENew (Prim Ref _) _ _)	= text "{ LOCKED: false }"
k2jExp2 (ENew x _ [])		= k2jConsStr x
k2jExp2 (ENew x _ bs)           = text "{" $$
                                  nest 4 (k2jName (prim Tag) <+> text ":" <+> k2jConsStr x <> text "," $$
					  k2jSBinds bs) $$
                                  text "}"
k2jExp2 e                       = parens (k2jExp e)


k2jList es (ENew (Prim CONS _) _ bs)
 				= k2jList (headExp bs : es) (tailExp bs)
k2jList es (ENew (Prim NIL _) _ _)
				= brackets (commasep k2jExp (reverse es))
k2jList es (ECast _ e)		= k2jList es e
k2jList es e			= brackets (commasep k2jExp (reverse es)) <> text ".concat" <> parens (k2jExp e)

	
headExp bs			= e
  where Val _ e			= lookup' bs selH

tailExp bs			= e
  where Val _ e			= lookup' bs selT


k2jSBinds bs			= foldr1 (\a b -> a <> comma $$ b) (map k2jSBind bs)


k2jSBind (x, Fun _ _ te c)      = k2jSelName x <+> text ":" <+> text "function" <+> parens (commasep k2jName (dom te)) <+> text "{" $$
                                  nest 4 (k2jCmd c) $$
                                  text "}"
k2jSBind (x, Val _ (ENew _ _ bs))
  | isCoerceLabel x		= k2jSBinds bs
k2jSBind (x, Val _ e)           = k2jSelName x <+> text ":" <+> k2jExp e


k2jCmd (CRet (ENew (Tuple 0 _) _ []))
				= empty
k2jCmd (CRet e)                 = text "return" <+> k2jExp e
k2jCmd (CRun e c)               = k2jExp e $$
                                  k2jCmd c
k2jCmd (CBind r bs c)           = vcat (map k2jBinds (groupMap bs)) $$
                                  k2jCmd c
k2jCmd (CUpd x e c)             = prId2 x <+> text "=" <+> k2jExp e $$
                                  k2jCmd c
k2jCmd (CUpdS e s e' c) 	= k2jUpd e s e' $$
				  k2jCmd c
k2jCmd (CUpdA e i e' c)         = k2jExp (ECall (prim IndexArray) [] [e,i]) <+> text "=" <+> k2jExp e' $$
                                  k2jCmd c
k2jCmd (CSwitch e [ACon k _ [(x,_)] c])
  | isCoerceConstr k		= text "var" <+> k2jName x <+> text "=" <+> k2jExp e $$
				  k2jCmd c
k2jCmd (CSwitch e alts)
  | listswitch			= k2jListSwitch0 e alts0 alts1
  | boolswitch			= k2jBoolSwitch0 e alts0 alts1
  | otherwise			= k2jSwitch0 e alts0 alts1
  where (alts0,alts1)		= partition nullary alts
	nullary (ACon _ _ [] _)	= True
	nullary (ALit _ _)	= True
	nullary _		= False
	ks			= consOf alts
	listswitch		= not (null ([prim NIL, prim CONS] `intersect` ks))
	boolswitch		= not (null ([prim TRUE, prim FALSE] `intersect` ks))
k2jCmd (CSeq c1 c2)
  | noBreak c1			= k2jCmd c1 
  | otherwise			= k2jCmd c1 $$
                                  k2jCmd c2
k2jCmd (CBreak)                 = text "break"
k2jCmd (CRaise e)               = text "RAISE" <> parens (k2jExp e)
k2jCmd (CWhile e c _)
  | isTrue e && noBreak c 	= text "while" <+> parens (k2jExp e) <+> text "{" $$
                                  nest 4 (k2jCmd c) $$
				  text "}"
k2jCmd (CWhile e c c')          = text "while" <+> parens (k2jExp e) <+> text "{" $$
                                  nest 4 (k2jCmd c) $$
                                  text "}" $$
                                  k2jCmd c'
k2jCmd (CCont)                  = text "continue"

isTrue (ELit (LInt _ n))	= n /= 0
isTrue (ENew (Prim TRUE _) _ _) = True
isTrue _			= False

noBreak (CBreak)		= False
noBreak (CRet _)		= True
noBreak (CCont)			= True
noBreak (CRaise _)		= True
noBreak (CRun _ c) 		= noBreak c
noBreak (CBind _ _ c) 		= noBreak c
noBreak (CUpd _ _ c) 		= noBreak c
noBreak (CUpdS _ _ _ c) 	= noBreak c
noBreak (CUpdA _ _ _ c) 	= noBreak c
noBreak (CWhile _ _ c) 		= noBreak c
noBreak (CSeq _ c) 		= noBreak c
noBreak (CSwitch _ alts)	= all noBreakAlt alts

noBreakAlt (ACon _ _ _ c) 	= noBreak c
noBreakAlt (ALit _ c) 		= noBreak c
noBreakAlt (AWild c) 		= noBreak c

k2jUpd e s e'
  | s == selH			= pre $$ k2jName x <> text "[0] =" <+> k2jExp e'
  | s == selT			= pre $$ k2jName x <> text ".push.apply" <> parens (k2jName x <> text "," <> k2jExp e')
  | otherwise			= k2jExp (ESel e s) <+> text "=" <+> k2jExp e'
  where (pre,x)			= k2jTemp e


k2jTemp (EVar x)		= (empty,x)
k2jTemp e			= (text "var" <+> k2jName x <+> text "=" <+> k2jExp e, x)
  where x			= name0 tempSym


k2jSwitch0 e [] alts1		= k2jSwitch1 e alts1
k2jSwitch0 e alts0 []		= text "switch" <+> parens (k2jExp e) <+> text "{" $$
				  nest 2 (vcat (map k2jAlt0 alts0)) $$
				  text "}"
k2jSwitch0 e alts0 alts1	= text "switch" <+> parens (k2jExp e) <+> text "{" $$
				  nest 2 (vcat (map k2jAlt0 alts0) $$
				          text "default:" <+> k2jSwitch1 e alts1) $$
				  text "}"

k2jSwitch1 e [AWild c]		= k2jCmd c
k2jSwitch1 e [ACon k _ te c]	= pre $$ k2jConBinds k x te $$
				  k2jCmd c
  where (pre,x)			= k2jTemp e
k2jSwitch1 e alts  		= pre $$ text "switch" <+> parens (k2jName x <> text "." <> k2jName (prim Tag)) <+> text "{" $$
                                  nest 2 (vcat (map (k2jAlt1 x) alts)) $$
                                  text "}"
  where (pre,x)			= k2jTemp e



k2jAlt0 (ACon k _ [] c) 	= text "case" <+> k2jConsStr k <> text ":" <+> text "{" $$
				  nest 2 (k2jCmd c $$ text "break") $$
				  text "}"
k2jAlt0 (ALit l c)              = text "case" <+> k2jLit l <> text ":" <+> text "{" $$
                                  nest 2 (k2jCmd c) $$
                                  text "}"

k2jAlt1 x (ACon k _ te c)       = text "case" <+> k2jConsStr k <> text ":" <+> text "{" $$
				                                  nest 2 (k2jConBinds k x te $$ 
									  k2jCmd c $$ text "break") $$
				                                  text "}"
k2jAlt1 x (AWild CBreak)	= empty
k2jAlt1 x (AWild c)             = text "default:" <+> text "{" $$
                                  nest 2 (k2jCmd c) $$
                                  text "}"

k2jConBinds k x te		= vcat (map (k2jConArg x) (dom te `zip` conSels k))

k2jConArg x (y,l)               = text "var" <+> k2jName y <+> text "=" <+> k2jName x <> text "." <> k2jSelName l


k2jListSwitch0 e (ACon _ _ _ c1 : _) (ACon _ _ te c2 : _)
				= k2jListSwitch1 e c1 te c2
k2jListSwitch0 e [] (ACon _ _ te c2 : AWild c1 : _)
				= k2jListSwitch1 e c1 te c2
k2jListSwitch0 e (ACon _ _ _ c1 : _) (AWild c2 : _)
				= k2jListSwitch1 e c1 [] c2
k2jListSwitch0 e alts []	= k2jListSwitch0 e alts [AWild CBreak]
k2jListSwitch0 e [] alts	= k2jListSwitch0 e alts (alts ++ [AWild CBreak])

k2jListSwitch1 e c1 [] c2	= text "if" <+> parens (k2jExp1 e <> text ".isNil()") <+> text "{" $$
				  nest 4 (k2jCmdT c1) $$
				  k2jCmdF c2 $$
				  text "}"
k2jListSwitch1 e c1 te c2
				= pre $$ text "if" <+> parens (k2jName x <> text ".isNil()") <+> text "{" $$
				  nest 4 (k2jCmdT c1) $$
				  text "} else {" $$
				  nest 4 (text "var" <+> k2jName hd <+> text "=" <+> k2jName x <> text "[0]" $$
				          text "var" <+> k2jName tl <+> text "=" <+> k2jName x <> text ".slice(1)" $$ 
				          k2jCmd c2) $$
				  text "}"
  where [hd,tl]			= dom te
	(pre,x)			= k2jTemp e


k2jBoolSwitch0 e (ACon (Prim TRUE _) _ _ ct : ACon (Prim FALSE _) _ _ cf : _) _
				= k2jBoolSwitch1 e ct cf
k2jBoolSwitch0 e (ACon (Prim FALSE _) _ _ cf : ACon (Prim TRUE _) _ _ ct : _) _
				= k2jBoolSwitch1 e ct cf
k2jBoolSwitch0 e (ACon (Prim TRUE _) _ _ ct : _) (AWild cf : _)
				= k2jBoolSwitch1 e ct cf
k2jBoolSwitch0 e (ACon (Prim FALSE _) _ _ cf : _) (AWild ct : _)
				= k2jBoolSwitch1 e ct cf
k2jBoolSwitch0 e alts []	= k2jBoolSwitch0 e alts [AWild CBreak]

k2jBoolSwitch1 e ct cf		= text "if" <+> parens (k2jExp e) <+> text "{" $$
				  nest 4 (k2jCmdT ct) $$
				  k2jCmdF cf $$
				  text "}"

k2jCmdT CBreak			= empty
k2jCmdT c			= k2jCmd c


k2jCmdF CBreak			= empty
k2jCmdF c			= text "} else {" $$
				  nest 4 (k2jCmd c)

				
k2jLit (LRat _ r)               = text (show (fromRational r :: Double))
k2jLit (LStr _ str)             = text "\"" <> hcat (map cChar str) <> text "\""
k2jLit (LChr _ c)               = text "\'" <> cChar c <> text "\'"
k2jLit l                        = pr l

k2jConsStr (Prim TRUE _)	= text "true"
k2jConsStr (Prim FALSE _)	= text "false"
k2jConsStr n			= text "'" <> k2jConsName n <> text "'"

k2jConsName (Prim p _)		= k2jPrim p
k2jConsName n@(Name s t m a)
  | all isOK s			= text (toJS s)
  | otherwise			= prId3 n
  where isOK c			= isAlpha c || c == '_'
	toJS ('_':s)		= toJS' s
	toJS s			= toJS' s
	toJS' s | all isUpper s	= s
	        | otherwise	= map f s
	f '_'			= '-'
	f c 			= toLower c
k2jConsName (Tuple n _)		= text ("tuple"++show n)

k2jSelName (Prim p _)		= k2jPrim p
k2jSelName n@(Name s t m a)
  | okForC s && s `notElem` jsKeys = text s
  | otherwise			= prId3 n

jsKeys				= ["break","catch","continue","debugger","finally","for","function","instanceof","return","switch",
				   "this","throw","try","typeof","var","void","with"]

isJsInfix x			= isInfix x || timeOp x
  where timeOp (Prim p _)	= p `elem` [TimePlus,TimeMinus,TimeEQ,TimeNE,TimeLT,TimeLE,TimeGE,TimeGT]
	timeOp _		= False
				  

k2jName (Prim p _)              = k2jPrim p
k2jName n                       = prId3 n


k2jPrim Tag			= text "tagName"

k2jPrim IntToFloat              = empty
k2jPrim FloatToInt              = text "Math.floor"

k2jPrim IntToChar               = text "String.fromCharCode"

k2jPrim Sec			= text "1000*"
k2jPrim Millisec		= empty
k2jPrim Microsec		= text "1E-3*"
k2jPrim Nanosec			= text "1E-6*"
k2jPrim Infinity                = text "Infinity"

k2jPrim TimePlus		= text "+"
k2jPrim TimeMinus		= text "-"

k2jPrim TimeEQ			= text "=="
k2jPrim TimeNE			= text "!="
k2jPrim TimeLT			= text "<"
k2jPrim TimeLE			= text "<="
k2jPrim TimeGE			= text ">"
k2jPrim TimeGT			= text ">="


k2jPrim Raise                   = text "Raise"
k2jPrim Abort                   = text "ABORT"

k2jPrim p                       = cSym p
