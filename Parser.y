{

module Parser (parser) where

import Common
import Token
import Lexer
import ParseMonad
import Syntax
import Fixity

parser     :: String -> M Module
parser str = runPM2 parse str

}

%token
        VARID 	 { VarId $$ }
	CONID	 { ConId $$ }
        SELID    { SelId $$ }
	'-'	 { VarSym "-" }
        '<'      { VarSym "<" }
        '>'      { VarSym ">" }
        '*'	 { VarSym "*" }
	VARSYM	 { VarSym $$ }
	CONSYM	 { ConSym $$ }
	INT	 { IntTok $$ }
	RATIONAL { FloatTok $$ }
 	CHAR	 { Character $$ }
	STRING   { StringTok $$ }
{-

Symbols
-}
	'('	{ LeftParen }
	')'	{ RightParen }
	';'	{ SemiColon }
	'{'	{ LeftCurly }
	'}'	{ RightCurly }
	vccurly { VRightCurly }	      -- a virtual close brace
	'['	{ LeftSquare }
	']'	{ RightSquare }
  	','	{ Comma }
	'`'	{ BackQuote }
        '_'     { Wildcard }

{-

Reserved operators

-}
	'..'	{ DotDot }
	'::'	{ DoubleColon }
        ':='    { Assign }
	'='	{ Equals }
	'\\'	{ Backslash }
	'|'	{ Bar }
	'<-'	{ LeftArrow }
	'->'	{ RightArrow }
        '\\\\'  { Backslash2 }
{-

Reserved Ids

-}
        'action'        { KW_Action }
        'after'         { KW_After }
        'before'        { KW_Before }
        'case'		{ KW_Case }
        'class'		{ KW_Class }
        'data'		{ KW_Data }
        'do'		{ KW_Do }
        'else'		{ KW_Else }
        'elsif'		{ KW_Elsif }
        'forall'	{ KW_Forall }
        'if'		{ KW_If }
        'in'		{ KW_In }
        'instance'	{ KW_Instance }
        'let'		{ KW_Let }
        'module'	{ KW_Module }
        'of'		{ KW_Of }
        'record'        { KW_Record }
        'return'        { KW_Return }
        'request'       { KW_Request }
        'template'      { KW_Template }
        'then'		{ KW_Then }
        'type'		{ KW_Type }
        'where'		{ KW_Where }
        'while'		{ KW_While }

%monad { PM } { thenPM } { returnPM }
%lexer { lexer } { EOF }
%name parse
%tokentype { Token }
%%

-- Module Header ------------------------------------------------------------

module  :: { Module }
        : 'module' conid 'where' body		{ Module $2 $4 }

body    :: { [Decl] }
        : '{' layout_off topdecls optsemi '}'	{ reverse $3 }
        |     layout_on  topdecls optsemi close	{ reverse $2 }

optsemi :: { () }
        : ';'					{ () }
        | {- empty -}				{ () }


-- Top-level declarations ---------------------------------------------------

topdecls :: { [Decl] }
        : topdecls ';' topdecl		        { $3 : $1 }
        | topdecl				{ [$1] }

topdecl :: { Decl }
        : conid '::' kind		        	{ DKSig $1 $3 }
        | 'type' conid tyvars '=' type	                { DType $2 (reverse $3) $5 }
        | 'data' conid tyvars optsubs optcs             { DData $2 (reverse $3) $4 $5 }
        | 'record' conid tyvars optsups optsigs         { DRec False $2 (reverse $3) $4 $5 }
	| 'class' conid tyvars optsups optsigs	        { DRec True $2 (reverse $3) $4 $5 }
	| 'instance' type '=' bindlist			{ DInst $2 $4 }
        | 'instance' varid '::' type                    { DPSig $2 $4 }  
        | vars '::' type		                { DBind (BSig (reverse $1) $3) }
        | lhs rhs 					{ DBind (BEqn $1 $2) }

optsups :: { [Type] }
        : '<' types				{ reverse $2 }
        | '<' type				{ [$2] }
        | {- empty -}				{ [] }

optsubs :: { [Type] }
        : '>' types				{ reverse $2 }
        | '>' type				{ [$2] }
        | {- empty -}				{ [] }

tyvars  :: { [Name] }
        : tyvars varid				{ $2 : $1 }
        | {- empty -}				{ [] }


-- Datatype declarations ---------------------------------------------------

optcs   :: { [Constr] }
        : '=' constrs				{ reverse $2 }
        | {- empty -}				{ [] }
        
constrs :: { [Constr] }
	: constrs '|' type	                { type2cons $3 : $1 }
	| type			                { [type2cons $1] }


-- Signatures --------------------------------------------------------------

optsigs :: { [Sig] }
        : '=' siglist				{ $2 }
        | {- empty -}				{ [] }
        
siglist :: { [Sig] }
        : '{' layout_off sigs '}'		{ reverse $3 }
        |     layout_on  sigs close		{ reverse $2 }

sigs	:: { [Sig] }
        : sigs ';' sig				{ $3 : $1 }
        | sig					{ [$1] }

sig	:: { Sig }
        : selvars '::' type	                { Sig (reverse $1) $3 }


-- Bindings ----------------------------------------------------------------

bindlist :: { [Bind] }
	: '{' layout_off binds '}'		{ reverse $3 }
	|     layout_on  binds close		{ reverse $2 }

binds   :: { [Bind] }
        : binds ';' bind			{ $3 : $1 }
        | bind					{ [$1] }
        
bind    :: { Bind }
	: vars '::' type	                { BSig (reverse $1) $3 }
        | lhs rhs 				{ BEqn $1 $2 }


recbinds   :: { [Field] }
        : recbinds ',' recbind			{ $3 : $1 }
        | recbind				{ [$1] }
        
recbind    :: { Field }
        : selvar exp 				{ Field $1  $2 }

vars	:: { [Name] }
	: vars ',' var				{ $3 : $1 }
        | var					{ [$1] }

selvars	:: { [Name] }
	: selvars ',' selvar			{ $3 : $1 }
        | selvar				{ [$1] }

lhs     :: { Lhs }
        : exp0s                                 { exp2lhs $1 }

rhs	:: { Rhs Exp }
	: '=' exp		    		{ RExp $2 }
	| gdrhss		    		{ RGrd (reverse $1) }
        | rhs 'where' bindlist			{ RWhere $1 $3 }

gdrhss  :: { [GExp Exp] }
	: gdrhss gdrhs		    		{ $2 : $1 }
	| gdrhs			    		{ [$1] }

gdrhs   :: { GExp Exp }
	: '|' quals '=' exp    			{ GExp $2 $4 }


-- Types ---------------------------------------------------------------------

type    :: { Type }
        : ftype '\\\\' preds                    { TQual $1 $3 }
        | ftype                                 { $1 }

ftype   :: { Type }
        : btypes                                { tFun (reverse (tail $1)) (head $1) }
        | btype '<' btype                       { TSub $1 $3}

btypes  :: { [Type] }
        : btypes '->' btype                     { $3 : $1 }
        | btype                                 { [$1] }

btype   :: { Type }
        : btype  atype                          { TAp $1 $2 }
        | atype                                 { $1 }

atype   :: { Type }
	: con					{ TCon $1 }
	| varid					{ TVar $1 }
        | '_'                                   { TWild }
        | '[' ']'				{ TCon (prim LIST) }
--	| '(' '->' ')'	                	{ TCon (prim ARROW) }
	| '(' commas ')'			{ TCon (tuple ($2+1)) }
        | '(' ')'				{ TCon (prim UNIT) }
        | '(' type ')'                          { $2 }
	| '(' types ')'				{ TTup (reverse $2) }
	| '[' type ']'				{ TList $2 }

types   :: { [Type] }
	: types ',' ftype			{ $3 : $1 }
	| ftype  ',' ftype			{ [$3, $1] }

commas  :: { Int }
	: commas ','		     		{ $1 + 1 }
	| ','			     		{ 1 }

-- Predicates -----------------------------------------------------------------

preds	:: { [Pred] }
        : preds ',' pred			{ $3 : $1 }
        | pred					{ [$1] }
        
pred	:: { Pred }
        : ftype                                 { PType $1 }
        | varid '::' kind			{ PKind $1 $3 }

kind	:: { Kind }
        : kind1 '->' kind			{ KFun $1 $3 }
        | kind1					{ $1 }

kind1	:: { Kind }
        : '*'					{ Star }
        | '_'                                   { KWild }
        | '(' kind ')'				{ $2 }
        

-- Expressions -------------------------------------------------------------

exp     :: { Exp }
        : exp0a '::' type                       { ESig $1 $3 }
        | exp0                                  { $1}

exp0    :: { Exp }
        : exp0a                                 { $1 }
        | exp0b                                 { $1 }

exp0a	:: { Exp }
        : opExpa			        { transFix $1 }
	| exp10a			        { $1 }
	  
exp0b	:: { Exp }
        : opExpb			        { transFix $1 }
	| exp10b			        { $1 }
	  
opExpa  :: { OpExp }
        : opExpa op '-' exp10a	                { Cons $1 $2 (ENeg $4) }
        | opExpa op exp10a	                { Cons $1 $2 $3 }
        | '-' exp10a			        { Nil (ENeg $2) }
        | exp10a op '-' exp10a	                { Cons (Nil $1) $2 (ENeg $4) }
        | exp10a op exp10a		        { Cons (Nil $1) $2 $3 }
	  
opExpb  :: { OpExp }
        : opExpa op '-' exp10b	                { Cons $1 $2 (ENeg $4) }
	| opExpa op exp10b	                { Cons $1 $2 $3 }
	| '-' exp10b		                { Nil (ENeg $2) }
	| exp10a op '-' exp10b	                { Cons (Nil $1) $2 (ENeg $4) }
	| exp10a op exp10b	                { Cons (Nil $1) $2 $3 }
	  
exp10a  :: { Exp }
        : 'case' exp 'of' altslist              { ECase $2 $4 }
        | exp10as                               { $1 }

exp10as :: { Exp }
        : 'do' stmtlist                         { EDo $2  }
        | loc 'action' stmtlist                 { EAct (name $1 "self") $3 }
        | loc 'request' stmtlist                { EReq (name $1 "self") $3 }
        | loc 'template' stmtlist               { ETempl (name $1 "self") $3 }
        | '{'  layout_off recbinds '}'          { ERec Nothing (reverse $3) } 
        | con '{'  layout_off recbinds '}'      { ERec (Just ($1,True)) (reverse $4) } 
        | con '{'  layout_off recbinds '..' '}' { ERec (Just ($1,False)) (reverse $4) } 
        | con '{'  layout_off '..' '}'          { ERec (Just ($1,False)) [] } 
        | fexp                                  { $1 }

exp10b :: { Exp }
        : 'if' exp 'then' exp 'else' exp        { EIf $2 $4 $6 }
        | exp10bs                               { $1 }

exp10bs :: { Exp }
        : '\\' apats '->' exp                   { ELam (reverse $2) $4 }
        | 'let' bindlist 'in' exp               { ELet $2 $4 }
        | 'after' aexp exp                      { EAfter $2 $3 }
        | 'before' aexp exp                     { EBefore $2 $3 }

fexp    :: { Exp }
        : fexp aexp                             { EAp $1 $2 }
        | aexp                                  { $1 }

aexp    :: { Exp }
        : aexp selid                            { ESelect $1 $2 }
        | bexp                                  { $1 }

bexp    :: { Exp }
        : var                                   { EVar $1 }
        | '_'                                   { EWild }
        | con                                   { ECon $1 }
        | lit                                   { ELit $1 }
        | '(' ')'                               { ECon (prim UNIT) }
        | '(' selid ')'                         { ESel $2 }
        | '(' exp ')'                           { $2 }
        | '(' exps ')'                          { ETup (reverse $2) } 
        | '[' list ']'                          { $2 }
        | '(' exp10a op ')'                     { ESectR $2 $3 }
        | '(' op0 fexp ')'                      { ESectL $2 $3 }
        | '(' commas ')'                        { ECon (tuple ($2+1)) }

lit     :: { Lit }
        : INT                                   { LInt (readInteger $1) }
        | RATIONAL                              { LRat (readRational $1) }
        | CHAR                                  { LChr $1 }
        | STRING                                { LStr $1 }

-- List expressions -------------------------------------------------------------

list    :: { Exp }
        : {- empty -}                           { EList [] }
        | exp                                   { EList [$1] }
        | exps                                  { EList (reverse $1) }
        | exp '..' exp                          { ESeq $1 Nothing $3 }
        | exp ',' exp '..' exp                  { ESeq $1 (Just $3) $5 }
        | exp '|' quals                         { EComp $1 (reverse $3) }

exps    :: { [Exp] }
        : exps ',' exp                          { $3 : $1 }
        | exp ',' exp                           { [$3,$1] }
 

-- List comprehensions ---------------------------------------------------------

quals   :: { [Qual] }
        : quals ',' qual                        { $3 : $1 }
        | qual                                  { [$1] }

qual    :: { Qual }
        : pat '<-' exp0s                        { QGen $1 $3 }
        | exp0s                                 { QExp $1 }
        | 'let' bindlist                        { QLet $2 }


-- Case alternatives ------------------------------------------------------------

altslist :: { [Alt Exp] }
        : '{' layout_off alts optsemi '}'       { reverse $3 }
        |     layout_on  alts optsemi close     { reverse $2 }


alts    :: { [Alt Exp] }
        : alts ';' alt                          { $3 : $1 }
        | alt                                   { [$1] }

alt     :: { Alt Exp }
        : pat rhscasealts                       { Alt $1 $2 }

rhscasealts :: { Rhs Exp }
        : '->' exp                              { RExp $2 }
        | gdcaserhss                            { RGrd (reverse $1) }
        | rhscasealts 'where' bindlist          { RWhere $1 $3 }

gdcaserhss :: { [GExp Exp] }
        : gdcaserhss gdcaserhs                  { $2 : $1 }
        | gdcaserhs                             { [$1] }

gdcaserhs :: { GExp Exp }
        : '|' quals  '->' exp                   { GExp $2 $4 }


-- Case statement alternatives ------------------------------------------------------------

saltslist :: { [Alt [Stmt]] }
        : '{' layout_off salts optsemi '}'      { reverse $3 }
        |     layout_on  salts optsemi close    { reverse $2 }


salts   :: { [Alt [Stmt]] }
        : salts ';' salt                        { $3 : $1 }
        | salt                                  { [$1] }

salt    :: { Alt [Stmt] }
        : pat srhscasealts                      { Alt $1 $2 }

srhscasealts :: { Rhs [Stmt] }
        : '->' stmtlist                         { RExp $2 }
        | sgdcaserhss                           { RGrd (reverse $1) }
        | srhscasealts 'where' bindlist         { RWhere $1 $3 }

sgdcaserhss :: { [GExp [Stmt]] }
        : sgdcaserhss sgdcaserhs                { $2 : $1 }
        | sgdcaserhs                            { [$1] }

sgdcaserhs :: { GExp [Stmt] }
        : '|' quals  '->' stmtlist              { GExp $2 $4 }


-- Statement sequences -----------------------------------------------------------

stmtlist :: { [Stmt] }
        : '{' layout_off stmts '}'              { reverse $3 }
        | layout_on stmts close                 { reverse $2 }

stmts   :: { [Stmt] }
        : stmts ';' stmt                        { $3 : $1 }
        | stmt                                  { [$1] }

stmt    :: { Stmt }
        : pat '<-' exp                          { SGen $1 $3 }
        | mexp                                  { SExp $1 }
        | vars '::' type                        { SBind (BSig $1 $3) }
        | lhs rhs                               { SBind (BEqn $1 $2) }
        | pat ':=' exp                          { SAss $1 $3 }
        | 'return' exp                          { SRet $2 }
        | 'forall' quals 'do' stmtlist          { SForall (reverse $2) $4 }
        | 'if' exp 'then' stmtlist              { SIf $2 $4 }
        | 'elsif' exp 'then' stmtlist           { SElsif $2 $4 }
        | 'else' stmtlist                       { SElse $2 }
        | 'while' exp 'do' stmtlist             { SWhile $2 $4 }
        | 'case' exp 'of' saltslist             { SCase $2 $4 }
     

mexp    :: { Exp }
--      : exp0as '::' type                      { ESig $1 $3 }
        : exp0s                                 { $1}

exp0s    :: { Exp }
        : exp0as                                { $1 }
        | exp0bs                                { $1 }

exp0as	:: { Exp }
        : opExpas			        { transFix $1 }
	| exp10as			        { $1 }
	  
exp0bs	:: { Exp }
        : opExpbs			        { transFix $1 }
	| exp10bs			        { $1 }
	  
opExpas  :: { OpExp }
        : opExpas op '-' exp10as                { Cons $1 $2 (ENeg $4) }
        | opExpas op exp10as	                { Cons $1 $2 $3 }
        | '-' exp10as			        { Nil (ENeg $2) }
        | exp10as op '-' exp10as                { Cons (Nil $1) $2 (ENeg $4) }
        | exp10as op exp10as		        { Cons (Nil $1) $2 $3 }
	  
opExpbs  :: { OpExp }
        : opExpas op '-' exp10bs                { Cons $1 $2 (ENeg $4) }
	| opExpas op exp10bs	                { Cons $1 $2 $3 }
	| '-' exp10bs		                { Nil (ENeg $2) }
	| exp10as op '-' exp10bs                { Cons (Nil $1) $2 (ENeg $4) }
	| exp10as op exp10bs	                { Cons (Nil $1) $2 $3 }



-- Patterns ----------------------------------------------------------------

pat     :: { Pat }
        : exp0s                                 { $1 }

apats   :: { [Pat] }
        : apats apat                            { $2 : $1 }
        | apat                                  { [$1] }

apat    :: { Pat }
        : aexp                                  { $1 }


-- Variables, Constructors and Operators ------------------------------------

var     :: { Name }
        : varid                                 { $1 }
        | '(' varsym ')'                        { $2 }

con     :: { Name }
        : conid                                 { $1 }
        | '(' consym ')'                        { $2 }

varop   :: { Name }
        : varsym                                { $1 }
        | '`' varid '`'                         { $2 }

conop   :: { Name }
        : consym                                { $1 }
        | '`' conid '`'                         { $2 }

op      :: { Name }
        : varop                                 { $1 }
        | conop                                 { $1 }

op0     :: { Name }
        : VARSYM0                               {% do l <- getSrcLoc; return (name l $1) }
        | '`' varid '`'                         { $2 }
        | conop                                 { $1 }

selvar	:: { Name }
	: selid					{ $1 }
	| varid					{ mkSel $1 }


selid   :: { Name }
        : loc SELID                             { name $1 $2 }

varid   :: { Name }
        : loc VARID                             { name $1 $2 }

conid   :: { Name }
        : loc CONID                             { name $1 $2 }

varsym  :: { Name }
        : VARSYM1                               {% do l <- getSrcLoc; return (name l $1) }

consym  :: { Name }
        : loc CONSYM                            { name $1 $2 }



VARSYM1 :: { String }
	: VARSYM0				{ $1 }
	| '-'					{ "-" }

VARSYM0 :: { String }
        : VARSYM                                { $1 }
        | '<'                                   { "<" }
        | '>'                                   { ">" }
        | '*'                                   { "*" }
        | '\\\\'				{ "\\\\" }
        

-- Layout ---------------------------------------------------------------------

close :: { () }
        : vccurly                               { () } -- context popped in lexer.
        | error                                 {% popContext }

layout_off :: { () }    :                       {% pushContext NoLayout }
layout_on  :: { () }    :                       {% do { (_,c) <- getSrcLoc ;
                                                        pushContext (Layout c)
                                                      }
                                                }

loc     :: { (Int,Int) }
         : {- empty -}                          {% getSrcLoc }

{-
    layout_onR  :: { () }   :                       {% do { (_,c) <- getSrcLoc ;
                                                        pushContext (RecLayout c)
                                                      }
                                                }
-}

-- Error -----------------------------------------------------------------------

{
happyError = parseError "parse error"
}
