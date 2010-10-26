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

{

module Parser2 (parser) where

import Common
import Token
import Lexer
import ParseMonad
import Syntax2



}

%token
        VARID 	 { VarId $$ }
	CONID	 { ConId $$ }
        '-'	 { VarSym ("","-") }
        '<'      { VarSym ("","<") }
        '>'      { VarSym ("",">") }
        '*'	 { VarSym ("","*") }
        '@'      { VarSym ("","@") }
	VARSYM	 { VarSym $$ }
	CONSYM	 { ConSym $$ }
	INT	 { IntTok $$ }
	RATIONAL { FloatTok $$ }
 	CHAR	 { Character $$ }
	STRING   { StringTok $$ }
{-

Symbols
-}
        '~'     { Tilde }
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
        '.'     { Dot }
	'..'	{ DotDot }
	'::'	{ DoubleColon }
        ':='    { Assign }
	'='	{ Equals }
	'\\'	{ Backslash }
	'|'	{ Bar }
	'<-'	{ LeftArrow }
	'->'	{ RightArrow }
        '\\\\'  { Backslash2 }
        '||'    { Or }
        '&&'    { And }
{-

Reserved Ids

-}
        'action'        { KW_Action }
        'after'         { KW_After }
        'before'        { KW_Before }
        'case'		{ KW_Case }
        'class'		{ KW_Class }
        'data'		{ KW_Data }
        'default'       { KW_Default }
        'deriving'      { KW_Deriving }
        'do'		{ KW_Do }
        'else'		{ KW_Else }
        'elsif'		{ KW_Elsif }
        'extern'        { KW_Extern }
        'forall'	{ KW_Forall }
        'if'		{ KW_If }
        'import'        { KW_Import }
        'instance'      { KW_Instance }
        'in'		{ KW_In }
        'let'		{ KW_Let }
        'module'	{ KW_Module }
        'new'           { KW_New }
        'of'		{ KW_Of }
        'private'       { KW_Private }
        'request'       { KW_Request }
        'result'        { KW_Result }
        'send'          { KW_Send }
        'struct'        { KW_Struct }
        'then'		{ KW_Then }
        'type'		{ KW_Type }
        'typeclass'	{ KW_Typeclass }
        'use'           { KW_Use }
        'where'		{ KW_Where }

%monad { PM } { thenPM } { returnPM }
%lexer { lexer } { EOF }
%name parse
%tokentype { Token }
%%

-- Module Header ------------------------------------------------------------

module  :: { Module }
        : 'module' conid 'where' body		{ mkModule $2 $4 }

body    :: { ([Import],[Decl],[Decl]) }
        : '{' layout_off imports topdecls '}' private	{ (reverse $3,reverse $4, $6) }
        |     layout_on  imports topdecls close	private { (reverse $2, reverse $3, $5) }

private :: { [Decl] }
        : 'private' pbody			{ $2 }
        | {- empty -}			        { [] }

pbody    :: { [Decl] }
        : '{' layout_off topdecls '}'	        { reverse $3 }
        |     layout_on  topdecls close	        { reverse $2 }

imports :: { [Import] }
        : imports import ';'                    { $2 : $1 }
        | {- empty -}                           { [] }

import  :: { Import }
        : 'import' conid                        { Import (modId $2) }
        | 'use' conid                           { Use (modId $2) }


-- Top-level declarations ---------------------------------------------------

topdecls :: { [Decl] }
        : topdecls ';' topdecl		        { $3 : $1 }
        | topdecl				{ [$1] }

topdecl :: { Decl }
        : conid '::' kind		        	   { DKSig $1 $3 }
        | 'type' conid tyvars '=' type	                   { DType $2 (reverse $3) $5 }
        | 'data' conid tyvars optsubs optcs                { DData $2 (reverse $3) $4 $5 }
        | 'struct' conid tyvars optsups optsigs            { DStruct $2 (reverse $3) $4 $5 }
        | 'typeclass' conid tyvars sups optsigs            { DTypeClass $2 (reverse $3) $4 $5 }
        | 'typeclass' id                                   { DTClass $2 }
        | 'instance' var '::' type 'where' bindlist        { DInstance (Just $2) $4 $6 }
        | 'instance' type 'where' bindlist                 { DInstance Nothing $2 $4 }
        | 'instance' id                                    { DInst $2 }
        | 'deriving' 'instance' var '::' type              { DDerive (Just $3) $5 }
        | 'deriving' 'instance' type                       { DDerive Nothing $3 }
        | 'default' defaults                               { DDefault (reverse $2) }
        | 'extern' vars                                    { DExtern (reverse $2) }
        | vars '::' type		                   { DSig (reverse $1) $3 }
        | lhs rhs 					   { DEqn $1 $2 }

sups :: { [Type] }
        : '<' types				{ reverse $2 }
        | '<' type				{ [$2] }

optsups :: { [Type] }
        : sups                                  { $1 }
        | {- empty -}				{ [] }

optsubs :: { [Type] }
        : '>' types				{ reverse $2 }
        | '>' type				{ [$2] }
        | {- empty -}				{ [] }

tyvars  :: { [Name] }
        : tyvars varid				{ $2 : $1 }
        | {- empty -}				{ [] }

defaults ::  { [(Name,Name)] }
        : defaults ',' var '<' var              { ($3,$5) : $1 }
        | var '<' var                           { [($1,$3)] }
 

-- Datatype declarations ---------------------------------------------------

optcs   :: { [Constr] }
        : '=' constrs				{ reverse $2 }
        | {- empty -}				{ [] }
        
constrs :: { [Constr] }
	: constrs '|' constr	                { $3 : $1 }
	| constr			        { [$1] }

constr  :: { Constr }
        : type consym type                      { CInfix $1 $2 $3 [] [] }
        | type                                  { type2cons $1 }


-- Signatures --------------------------------------------------------------

optsigs :: { [Sig] }
        : 'where' siglist	      		{ $2 }
        | {- empty -}				{ [] }
        
siglist :: { [Sig] }
        : '{' layout_off sigs '}'		{ reverse $3 }
        |     layout_on  sigs close		{ reverse $2 }

sigs	:: { [Sig] }
        : sigs ';' sig				{ $3 : $1 }
        | sig					{ [$1] }

sig	:: { Sig }
        : vars '::' type	                { Sig (reverse $1) $3 }


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

vars	:: { [Name] }
	: vars ',' var				{ $3 : $1 }
        | var					{ [$1] }

lhs     :: { Lhs }
        : exp0s                                 { exp2lhs $1 }

rhs	:: { Rhs Exp }
	: '=' exp optbinds		    	{ RExp $2 $3 }
	| gdrhss optbinds		    	{ RGrd (reverse $1) $2 }

gdrhss  :: { [GExp Exp] }
	: gdrhss gdrhs		    		{ $2 : $1 }
	| gdrhs			    		{ [$1] }

gdrhs   :: { GExp Exp }
        : '|' quals '=' exp    			{ GExp (reverse $2) $4 }

optbinds :: { [Bind] }
        : 'where' bindlist                      { $2 }
        | {- empty -}                           { [] }


-- Types ---------------------------------------------------------------------

type    :: { Type }
        : ftype '\\\\' preds                    { TQual $1 (reverse $3) [] }
        | ftype '\\\\' quants                   { TQual $1 [] (reverse $3) }
        | ftype '\\\\' preds ',' quants         { TQual $1 (reverse $3) (reverse $5) }
        | ftype                                 { $1 }

ftype   :: { Type }
        : btypes                                { tFun (reverse (tail $1)) (head $1) }

btypes  :: { [Type] }
        : btypes '->' btype                     { $3 : $1 }
        | btype                                 { [$1] }

btype   :: { Type }
        : btype  atype                          { TAp $1 $2 }
        | atype                                 { $1 }

atype   :: { Type }
	: varid					{ TVar $1 }
	| con                                   { TCon $1 }
	| atype0                                { $1 }
	
atype0  :: { Type }
	: '_'                                   { TWild }
        | '[' ']'				{ TCon (prim LIST) }
--	| '(' '->' ')'	                	{ TCon (prim ARROW) }
	| '(' commas ')'			{ TCon (tuple ($2+1)) }
        | '(' ')'				{ TCon (tuple 0) }
        | '(' type ')'                          { TParen $2 }
	| '(' types ')'				{ TTup (reverse $2) }
	| '[' type ']'				{ TList $2 }

atypes  :: { [Type] }
        : atypes atype                          { $2 : $1 }
        | {- empty -}                           { [] }

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
        : con atypes                            { PClass $1 $2 }
        | con atypes '<' btype                  { PSub (foldl TAp (TCon $1) $2) $4 }
        | varid atypes '<' btype                { PSub (foldl TAp (TVar $1) $2) $4 }
        | atype0 atypes '<' btype               { PSub (foldl TAp $1 $2) $4}

quants  :: { [Quant] }
        : quants ',' quant                      { $3 : $1 }
        | quant                                 { [$1] }

quant   :: { Quant }
        : varid '::' kind			{ QVarSig $1 $3 }
        | varid                                 { QVar $1 }

kind	:: { Kind }
        : kind1 '->' kind			{ KFun $1 $3 }
        | kind1					{ $1 }

kind1	:: { Kind }
        : '*'					{ Star }
        | '_'                                   { KWild }
        | '(' kind ')'				{ $2 }
        

-- Expressions -------------------------------------------------------------

exp     :: { Exp }
        : exp0a '::' btype                                 { ESig $1 $3 }
        | exp0                                             { $1}
        | 'struct' layout_on binds close                   { EStruct Nothing (reverse $3) }
        | con 'struct' layout_on binds close               { EStruct (Just ($1,False)) (reverse $4) }
        | con 'struct' layout_on binds ';' '..' close      { EStruct (Just ($1,True)) (reverse $4) }
        | con 'struct' layout_on '..' close                { EStruct (Just ($1,True)) [] }

exp0    :: { Exp }
        : exp000a                               { $1 }
        | exp000b                               { $1 }

exp000a :: { Exp }
        : exp000a '||' exp00a                   { EOr $1 $3 }
        | exp00a                                { $1 }

exp00a  :: { Exp }
        : exp00a '&&' exp0a                     { EAnd $1 $3 }
        | exp0a                                 { $1 }

exp0a	:: { Exp }
        : opExpa			        { transFix $1 }
	| exp10a			        { $1 }
	  
exp000b :: { Exp }
        : exp000a '||' exp00b                   { EOr $1 $3 }
        | exp00b                                { $1 }

exp00b  :: { Exp }
        : exp00a '&&' exp0b                     { EAnd $1 $3 }
        | exp0b                                 { $1 }

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

op      :: { Exp }
        : varsym                                { EVar $1 }
        | consym                                { ECon $1 }
	| '`' fexp '`'                          { $2 }

op0     :: { Exp }
        : varsym0                               { EVar $1 }
        | consym                                { ECon $1 }
	| '`' fexp '`'                          { $2 }

exp10a  :: { Exp }
        : 'case' exp 'of' altslist              { ECase $2 $4 }
        | '{' layout_off binds '}'              { EStruct Nothing (reverse $3) }
        | exp10as                               { $1 }

exp10as :: { Exp }
        : fexp                                     { $1 }
        | forall 'do' stmtlist                     { EDo $1 Nothing Nothing $3 }
        | forall 'do' '@' var stmtlist             { EDo $1 (Just $4) Nothing $5 }
        | forall 'do' '@' con stmtlist             { EDo $1 Nothing (Just $4) $5 }
        | forall 'do' '@' var '@' con stmtlist     { EDo $1 (Just $4) (Just $6) $7 }
        | forall 'class' stmtlist                  { EClass $1 Nothing Nothing $3 }
        | forall 'class' '@' var stmtlist          { EClass $1 (Just $4) Nothing $5 }
        | forall 'class' '@' con stmtlist          { EClass $1 Nothing (Just $4) $5 }
        | forall 'class' '@' var '@' con stmtlist  { EClass $1 (Just $4) (Just $6) $7 }
        | before 'action' stmtlist                 { EAct $1 Nothing $3 }
        | before 'action' '@' var stmtlist         { EAct $1 (Just $4) $5 }
        | 'request' stmtlist                       { EReq Nothing $2 }
        | 'request' '@' var stmtlist               { EReq (Just $3) $4 }
      
  	| con '{' layout_off binds '}'             { EStruct (Just ($1,True)) (reverse $4) } 
        | con '{' layout_off binds '..' '}'        { EStruct (Just ($1,False)) (reverse $4) } 
        | con '{' layout_off binds ';' '..' '}'    { EStruct (Just ($1,False)) (reverse $4) } 
        | con '{' layout_off '..' '}'              { EStruct (Just ($1,False)) [] } 
	| con '{' layout_off  '}'		   { EStruct (Just ($1,True)) [] }

	| after 'send' exp10a                      { ESend $1 $3 }
	| forall 'new' exp10a                      { ENew $1 $3 }
        | '<-' exp10a                              { EGen $2 }


forall  :: { [Quals] }
        : 'forall' qualss                       { (reverse $2) }
        | {- empty -}                           { [] }

before  :: { Maybe Exp }
        : 'before' exp                          { Just $2 }
        | {- empty -}                           { Nothing }

after   :: { Maybe Exp }
        : 'after' exp                           { Just $2 }
        | {- empty -}                           { Nothing }

exp10b :: { Exp }
        : 'if' exp 'then' exp 'else' exp        { EIf $2 $4 $6 }
        | exp10bs                               { $1 }

exp10bs :: { Exp }
        : '\\' apats '->' exp                   { ELam (reverse $2) $4 }
        | 'let' bindlist 'in' exp               { ELet $2 $4 }

fexp    :: { Exp }
        : fexp aexp                             { EAp $1 $2 }
        | aexp                                  { $1 }

aexp    :: { Exp }
        : aexp '.' var                          { ESel $1 $3 }
        | bexp                                  { $1 }

bexp    :: { Exp }
        : var                                   { EVar $1 }
        | '_'                                   { EVar (name0 "_") }
        | con                                   { ECon $1 }
        | lit                                   { ELit $1 }
        | '(' '.' var ')'                       { ESelector $3 }
        | '(' exp ')'                           { $2 }
        | '(' exps ')'                          { ETup (reverse $2) } 
        | '[' list ']'                          { $2 }
        | '(' exp10a op ')'                     { ESectR $2 $3 }
        | '(' op0 fexp ')'                      { ESectL $2 $3 }
        | '(' commas ')'                        { ECon (tuple ($2+1)) }
        | '(' ')'                               { ECon (tuple 0) }

lit     :: { Lit }
        : INT                                   {% do l <- getSrcLoc; return (LInt (Just l) (readInteger $1)) }
        | RATIONAL                              {% do l <- getSrcLoc; return (LRat (Just l) (readRational $1)) }
        | CHAR                                  {% do l <- getSrcLoc; return (LChr (Just l) $1) }
        | STRING                                {% do l <- getSrcLoc; return (LStr (Just l) $1) }


-- List expressions -------------------------------------------------------------

list    :: { Exp }
        : {- empty -}                           { EList [] }
        | exp                                   { EList [$1] }
        | exps                                  { EList (reverse $1) }
        | exp '..' exp                          { ESeq $1 Nothing $3 }
        | exp ',' exp '..' exp                  { ESeq $1 (Just $3) $5 }
        | exp '|' qualss                        { EComp $1 (reverse $3) }

exps    :: { [Exp] }
        : exps ',' exp                          { $3 : $1 }
        | exp ',' exp                           { [$3,$1] }
 

-- List comprehensions ---------------------------------------------------------

qualss  :: { [Quals] }
        : qualss '|' quals                      { reverse $3 : $1 }
        | quals                                 { [reverse $1] }

quals   :: { Quals }
        : quals ',' qual                        { $3 : $1 }
        | qual                                  { [$1] }

qual    :: { Qual }
        : pat '<-' exp0s                        { QGen $1 $3 }
        | exp0s                                 { QExp $1 }
        | 'let' bindlist                        { QLet $2 }


-- Case alternatives ------------------------------------------------------------

altslist :: { [Alt Exp] }
        : '{' layout_off alts '}'               { reverse $3 }
        |     layout_on  alts close             { reverse $2 }


alts    :: { [Alt Exp] }
        : alts ';' alt                          { $3 : $1 }
        | alt                                   { [$1] }

alt     :: { Alt Exp }
        : pat rhscasealts                       { Alt $1 $2 }

rhscasealts :: { Rhs Exp }
        : '->' exp optbinds                     { RExp $2 $3 }
        | gdcaserhss optbinds                   { RGrd (reverse $1) $2 }

gdcaserhss :: { [GExp Exp] }
        : gdcaserhss gdcaserhs                  { $2 : $1 }
        | gdcaserhs                             { [$1] }

gdcaserhs :: { GExp Exp }
        : '|' quals  '->' exp                   { GExp (reverse $2) $4 }


-- Case statement alternatives ------------------------------------------------------------

saltslist :: { [Alt Stmts] }
        : '{' layout_off salts  '}'             { reverse $3 }
        |     layout_on  salts close            { reverse $2 }


salts   :: { [Alt Stmts] }
        : salts ';' salt                        { $3 : $1 }
        | salt                                  { [$1] }

salt    :: { Alt Stmts }
        : pat srhscasealts                      { Alt $1 $2 }

srhscasealts :: { Rhs Stmts }
        : '->' stmtlist optbinds                { RExp $2 $3 }
        | sgdcaserhss optbinds                  { RGrd (reverse $1) $2 }

sgdcaserhss :: { [GExp Stmts] }
        : sgdcaserhss sgdcaserhs                { $2 : $1 }
        | sgdcaserhs                            { [$1] }

sgdcaserhs :: { GExp Stmts }
        : '|' quals  '->' stmtlist              { GExp (reverse $2) $4 }


-- Statement sequences -----------------------------------------------------------

stmtlist :: { Stmts }
        : '{' layout_off stmts '}'              { $3 }
        | layout_on stmts close                 { $2 }

stmts   :: { Stmts }
        : stmt ';' stmts                        { $1 : $3 }
	| stmt					{ [$1] }
	| {- empty -}				{ [] }


stmt    :: { Stmt }
        : pat '<-' exp                          { SGen $1 $3 }
        | mexp                                  { SExp $1 }
        | vars '::' type                        { SSig $1 $3 }
        | lhs rhs                               { SEqn $1 $2 }
        | pat ':=' exp                          { SAss $1 $3 }
        | 'result' exp                          { SRes $2 }
        | 'if' exp 'then' stmtlist elsif else   { SIf $2 $4 $5 $6 }
        | 'case' exp 'of' saltslist             { SCase $2 $4 }

elsif   :: { [(Exp,Stmts)] }
        : 'elsif' exp 'then' stmtlist elsif     { ($2,$4) : $5 }
        | {- empty -}                           { [] }

else    :: { Maybe Stmts }
        : 'else' stmtlist                       { Just $2 }
        | {- empty -}                           { Nothing }

mexp    :: { Exp }
--      : exp0as '::' type                      { ESig $1 $3 }
        : exp0s                                 { $1}

exp0s    :: { Exp }
        : exp000as                              { $1 }
        | exp000bs                              { $1 }

exp000as :: { Exp }
        : exp000as '||' exp00as                 { EOr $1 $3 }
        | exp00as                               { $1 }

exp00as :: { Exp }
        : exp00as '&&' exp0as                   { EAnd $1 $3 }
        | exp0as                                { $1 }

exp0as	:: { Exp }
        : opExpas			        { transFix $1 }
	| exp10as			        { $1 }
	  
exp000bs :: { Exp }
        : exp000as '||' exp00bs                 { EOr $1 $3 }
        | exp00bs                               { $1 }

exp00bs :: { Exp }
        : exp00as '&&' exp0bs                   { EAnd $1 $3 }
        | exp0bs                                { $1 }

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
        : exp0s                                 { exp2pat $1 }

apats   :: { [Pat] }
        : apats apat                            { $2 : $1 }
        | apat                                  { [$1] }

apat    :: { Pat }
        : aexp                                  { exp2pat $1 }


-- Variables, Constructors and Operators ------------------------------------

var     :: { Name }
        : varid                                 { $1 }
        | '(' varsym ')'                        { $2 }
        | '~' varid                             { annotExplicit $2 }
        | '~' '(' varsym ')'                    { annotExplicit $3 }

con     :: { Name }
        : conid                                 { $1 }
        | '(' consym ')'                        { $2 }
        | '~' conid                             { annotExplicit $2 }
        | '~' '(' consym ')'                    { annotExplicit $3 }

id      :: { Name }
        : varid                                 { $1 }
        | conid                                 { $1 }

varid   :: { Name }
        : VARID                                 {% do l <- getSrcLoc; return (name l $1) }

conid   :: { Name }
        : CONID                                 {% do l <- getSrcLoc; return (name l $1) }

varsym  :: { Name }
        : VARSYM1                               {% do l <- getSrcLoc; return (name l $1) }

varsym0 :: { Name }
        : VARSYM0                               {% do l <- getSrcLoc; return (name l $1) }

consym  :: { Name }
        : CONSYM                                {% do l <- getSrcLoc; return (name l $1) }



VARSYM1 :: { (String,String) }
	: VARSYM0				{ $1 }
	| '-'					{ ("","-") }

VARSYM0 :: { (String,String) }
        : VARSYM                                { $1 }
        | '<'                                   { ("","<") }
        | '>'                                   { ("",">") }
        | '*'                                   { ("","*") }
        | '@'                                   { ("","@") }
        | '\\\\'				{ ("","\\\\") }
        

-- Layout ---------------------------------------------------------------------

close :: { () }
        : vccurly                               { () } -- context popped in lexer.
        | error                                 {% popContext }

layout_off :: { () }    :                       {% pushContext NoLayout }

layout_on  :: { () }    :                       {% do { (r,c) <- getSrcLoc;
                                                        pushContext (Layout c)
                                                      }
                                                }


-- Error -----------------------------------------------------------------------

{
parser     :: String -> M s Module
parser str = runPM2 parse str

happyError = parseError "parse error"
}
