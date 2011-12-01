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
import Prim



}

%token
        QVARID 	 { QVarId $$ }
		QCONID	 { QConId $$ }
		QVARSYM	 { QVarSym $$ }
		QCONSYM	 { QConSym $$ }

        '-'      { VarSym "-" }
        '<'      { VarSym "<" }
        '>'      { VarSym ">" }
        '*'      { VarSym "*" }
        '@'      { VarSym "@" }
		VARID 	 { VarId $$ }
		CONID	 { ConId $$ }
		VARSYM	 { VarSym $$ }
		CONSYM	 { ConSym $$ }

		INT	 	 { IntTok $$ }
		RATIONAL { FloatTok $$ }
 		CHAR	 { Character $$ }
		STRING   { StringTok $$ }
{-

Symbols
-}
		'('		{ LeftParen }
		')'		{ RightParen }
		';'		{ SemiColon }
		'{'		{ LeftCurly }
		'}'		{ RightCurly }
		vccurly { VRightCurly }	      -- a virtual close brace
		'['		{ LeftSquare }
		']'		{ RightSquare }
  		','		{ Comma }
		'`'		{ BackQuote }
        '_'     { Wildcard }

{-

Reserved operators

-}
        '.'     { Dot }
		'..'	{ DotDot }
		'::'	{ DoubleColon }
        ':='    { Assign }
		'='		{ Equals }
		'\\'	{ Backslash }
		'|'		{ Bar }
		'<-'	{ LeftArrow }
		'->'	{ RightArrow }
        '\\\\'  { Backslash2 }
        '||'    { Or }
        '&&'    { And }
        '!'     { Index }
{-

Reserved Ids

-}
        'action'    { KW_Action }
        'after'  	{ KW_After }
        'before'    { KW_Before }
        'case'		{ KW_Case }
        'class'		{ KW_Class }
        'data'		{ KW_Data }
        'default'   { KW_Default }
        'deriving'  { KW_Deriving }
        'do'		{ KW_Do }
        'else'		{ KW_Else }
        'elsif'		{ KW_Elsif }
        'extern'    { KW_Extern }
        'forall'	{ KW_Forall }
        'if'		{ KW_If }
        'import'    { KW_Import }
        'instance'  { KW_Instance }
        'in'		{ KW_In }
        'let'		{ KW_Let }
        'module'	{ KW_Module }
        'new'       { KW_New }
        'of'		{ KW_Of }
        'private'   { KW_Private }
        'request'   { KW_Request }
        'result'    { KW_Result }
        'send'      { KW_Send }
        'struct'    { KW_Struct }
        'then'		{ KW_Then }
        'type'		{ KW_Type }
        'typeclass'	{ KW_Typeclass }
        'use'       { KW_Use }
        'where'		{ KW_Where }

%monad { PM } { thenPM } { returnPM }
%lexer { lexer } { EOF }
%name parse
%tokentype { Token }
%%

-- Module Header ------------------------------------------------------------

module  :: { Module }
        : 'module' conid 'where' body           			{ let (is,ds,ps) = $4 in Module $2 is ds ps }

body    :: { ([Import],[Decl],[Decl]) }
        : '{' layout_off imports topdecls '}' private		{ (reverse $3,reverse $4, $6) }
        |     layout_on  imports topdecls close	private 	{ (reverse $2, reverse $3, $5) }

private :: { [Decl] }
        : 'private' pbody									{ $2 }
        | {- empty -}			        					{ [] }

pbody    :: { [Decl] }
        : '{' layout_off topdecls '}'	        			{ reverse $3 }
        |     layout_on  topdecls close	        			{ reverse $2 }

imports :: { [Import] }
        : imports import ';'                    			{ $2 : $1 }
        | {- empty -}                           			{ [] }

import  :: { Import }
        : 'import' anyconid                     			{ Import $2 }
        | 'use' anyconid                        			{ Use Nothing $2 }
        | 'use' conid '=' anyconid              			{ Use (Just $2) $4 }


-- Top-level declarations ---------------------------------------------------

topdecls :: { [Decl] }
        : topdecls ';' topdecl		        				{ $3 : $1 }
        | topdecl											{ [$1] }

topdecl :: { Decl }
        : conid '::' kind		        	   				{ DKSig $1 $3 }
        | 'type' conid tyvars '=' type	                   	{ DType $2 (reverse $3) $5 }
        | 'data' conid tyvars subs '=' constrs             	{ DData $2 (reverse $3) $4 (reverse $6) }
        | 'data' conid tyvars '=' constrs                  	{ DData $2 (reverse $3) [] (reverse $5) }
        | 'data' conid tyvars subs                         	{ DData $2 (reverse $3) $4 [] }
        | 'data' conid tyvars                              	{ DData $2 (reverse $3) [] [] }
        | 'struct' conid tyvars sups 'where' siglist       	{ DStruct $2 (reverse $3) $4 $6 }
        | 'struct' conid tyvars 'where' siglist            	{ DStruct $2 (reverse $3) [] $5 }
        | 'struct' conid tyvars sups                       	{ DStruct $2 (reverse $3) $4 [] }
        | 'struct' conid tyvars                            	{ DStruct $2 (reverse $3) [] [] }
        | 'typeclass' conid tyvars sups 'where' siglist    	{ DTypeClass $2 (reverse $3) $4 $6 }
        | 'typeclass' conid tyvars 'where' siglist         	{ DTypeClass $2 (reverse $3) [] $5 }
        | 'typeclass' conid tyvars sups                    	{ DTypeClass $2 (reverse $3) $4 [] }
        | 'typeclass' anyconid                             	{ DTClass $2 }
        | 'instance' varid '::' type 'where' bindlist      	{ DInstance (Just $2) $4 $6 }
        | 'instance' type 'where' bindlist                 	{ DInstance Nothing $2 $4 }
        | 'instance' anyvarid                              	{ DInst $2 }
        | 'deriving' 'instance' varid '::' type            	{ DDerive (Just $3) $5 }
        | 'deriving' 'instance' type                       	{ DDerive Nothing $3 }
        | 'default' defaults                               	{ DDefault (reverse $2) }
        | 'extern' varlist                                 	{ DExtern (reverse $2) }
        | 'extern' varlist '::' type                       	{ DExternSig (reverse $2) $4 }
        | varlist '::' type		                   			{ DSig (reverse $1) $3 }
        | pat rhs 					   						{ DEqn $1 $2 }
        | module                                           	{ DModule $1 }

sups :: { [Type] }
        : '<' types											{ reverse $2 }
        | '<' type											{ [$2] }

subs :: { [Type] }
        : '>' types											{ reverse $2 }
        | '>' type											{ [$2] }

tyvars  :: { [Quant] }
        : tyvars varid										{ QVar $2 : $1 }
        | tyvars '(' varid '::' kind ')'        			{ QVarSig $3 $5 : $1 }
        | {- empty -}										{ [] }

defaults ::  { [(QName2,QName2)] }
        : defaults ',' anyvarid '<' anyvarid    			{ ($3,$5) : $1 }
        | defaults ',' cpred                    			{ $1 }
        | anyvarid '<' anyvarid                 			{ [($1,$3)] }
        | cpred                                 			{ [] }
 

-- Datatype declarations ---------------------------------------------------

constrs :: { [Constr] }
		: constrs '|' constr	               				{ $3 : $1 }
		| constr			       							{ [$1] }

constr  :: { Constr }
        : ftype consym ftype '\\\\' preds             		{ CInfix $1 $2 $3 [] $5 }
        | ftype consym ftype '\\\\' quants            		{ CInfix $1 $2 $3 $5 [] }
        | ftype consym ftype '\\\\' quants ',' preds  		{ CInfix $1 $2 $3 $5 $7 }
        | type                                        		{ type2cons $1 }


-- Signatures --------------------------------------------------------------

siglist :: { [Sig] }
        : '{' layout_off sigs '}'				{ reverse $3 }
        |     layout_on  sigs close				{ reverse $2 }

sigs	:: { [Sig] }
        : sigs ';' sig							{ $3 : $1 }
        | sig									{ [$1] }

sig	:: { Sig }
        : varlist '::' type	                	{ Sig (reverse $1) $3 }


-- Bindings ----------------------------------------------------------------

bindlist :: { [Bind] }
		: '{' layout_off binds '}'				{ reverse $3 }
		|     layout_on  binds close			{ reverse $2 }

binds   :: { [Bind] }
        : binds ';' bind						{ $3 : $1 }
        | bind									{ [$1] }

bind    :: { Bind }
		: varlist '::' type	                	{ BSig (reverse $1) $3 }
        | pat rhs 								{ BEqn $1 $2 }

varlist	:: { [Name2] }
		: varlist ',' varid						{ $3 : $1 }
		| varlist ',' '(' varsym ')'            { $4 : $1 }
        | varid									{ [$1] }
        | '(' varsym ')'                        { [$2] }

rhs	:: { Rhs Exp }
		: '=' exp optbinds		    			{ RExp $2 $3 }
		| gdrhss optbinds		    			{ RGrd (reverse $1) $2 }

gdrhss  :: { [GExp Exp] }
		: gdrhss gdrhs		    				{ $2 : $1 }
		| gdrhs			    					{ [$1] }

gdrhs   :: { GExp Exp }
        : '|' quals '=' exp    					{ GExp (reverse $2) $4 }

optbinds :: { [Bind] }
        : 'where' bindlist                      { $2 }
        | {- empty -}                           { [] }


-- Types ---------------------------------------------------------------------

type00  :: { Type }
        : type                                  { $1 }
        | btype '::' kind                       { TSig $1 $3 }

type    :: { Type }
        : ftype '\\\\' preds                    { TQual $1 [] (reverse $3) }
        | ftype '\\\\' quants                   { TQual $1 (reverse $3) [] }
        | ftype '\\\\' quants ',' preds         { TQual $1 (reverse $3) (reverse $5) }
        | ftype                                 { $1 }

ftype   :: { Type }
        : btypes                                { tFun (reverse (tail $1)) (head $1) }

btypes  :: { [Type] }
        : btypes '->' btype                     { $3 : $1 }
        | btype                                 { [$1] }

btype   :: { Type }
        : btype atype                           { TAp $1 $2 }
        | atype                                 { $1 }

atype   :: { Type }
		: varid                                 { TVar $1 }
		| anyconid                              { TCon $1 }
		| '_'                                   { TWild }
        | '[' ']'								{ TCon Prim.nList }
--		| '(' '->' ')'	                		{ TCon Prim.tArrow }
		| '(' commas ')'						{ TTupC ($2+1) }
        | '(' ')'								{ TTupC 0 }
        | '(' type00 ')'                        { TParen $2 }
		| '(' types ')'							{ TTup (reverse $2) }
		| '[' type ']'							{ TList $2 }

atypes  :: { [Type] }
        : atypes atype                          { $2 : $1 }
        | {- empty -}                           { [] }

types   :: { [Type] }
		: types ',' ftype						{ $3 : $1 }
		| ftype  ',' ftype						{ [$3, $1] }

commas  :: { Int }
		: commas ','		     				{ $1 + 1 }
		| ','			     					{ 1 }

-- Predicates -----------------------------------------------------------------

preds	:: { [Pred] }
        : preds ',' pred						{ $3 : $1 }
        | pred									{ [$1] }
        
pred	:: { Pred }
		: cpred                                 { $1 }
        | anyconid atypes '<' btype             { PSub (foldl TAp (TCon $1) $2) $4 }
        | varid atypes '<' btype                { PSub (foldl TAp (TVar $1) $2) $4 }
        | '[' type ']' '<' btype                { PSub (TList $2) $5 }
        | '[' ']' atypes '<' btype              { PSub (foldl TAp (TCon Prim.nList) $3) $5 }
--      | '(' type ')' atypes '<' btype         { PSub (foldl TAp $2 $4) $6 }
--      | '(' types ')' '<' btype               { PSub (TTup (reverse $2)) $5 }
		| '(' commas ')' atypes '<' btype       { PSub (foldl TAp (TTupC ($2+1)) $4) $6 }
		| '(' ')' atypes '<' btype              { PSub (foldl TAp (TTupC 0) $3) $5 }
        | '(' qpred ')'                         { $2 }

cpred   :: { Pred }
		: anyconid atypes                       { PClass $1 $2 }

qpred    :: { Pred }
        : pred '\\\\' preds                     { PQual $1 [] (reverse $3) }
        | pred '\\\\' quants                    { PQual $1 (reverse $3) [] }
        | pred '\\\\' quants ',' preds          { PQual $1 (reverse $3) (reverse $5) }
        | pred                                  { $1 }

quants  :: { [Quant] }
        : quants ',' quant                      { $3 : $1 }
        | quant                                 { [$1] }

quant   :: { Quant }
        : varid '::' kind						{ QVarSig $1 $3 }
        | varid                                 { QVar $1 }

kind	:: { Kind }
        : kind1 '->' kind						{ KFun $1 $3 }
        | kind1									{ $1 }

kind1	:: { Kind }
        : '*'									{ Star }
        | '_'                                   { KWild }
        | '(' kind ')'							{ $2 }
        

-- Expressions -------------------------------------------------------------

exp     :: { Exp }
        : exp0a '::' btype                                   	{ ESig $1 $3 }
        | exp0                                               	{ $1}

        | 'struct' layout_on binds close                     	{ EStruct Nothing (reverse $3) }
        | anyconid 'struct' layout_on binds close            	{ EStruct (Just ($1,False)) (reverse $4) }
        | anyconid 'struct' layout_on binds ';' '..' close   	{ EStruct (Just ($1,True)) (reverse $4) }
        | anyconid 'struct' layout_on '..' close             	{ EStruct (Just ($1,True)) [] }
        | 'struct' layout_on binds ';' '_' close             	{ EStructUpdate Nothing (reverse $3) }
        | anyconid 'struct' layout_on binds ';' '_' close    	{ EStructUpdate (Just $1) (reverse $4) }

        | 'struct' '{' layout_off binds '}'                     { EStruct Nothing (reverse $4) }
        | anyconid 'struct' '{' layout_off binds '}'         	{ EStruct (Just ($1,False)) (reverse $5) }
        | anyconid 'struct' '{' layout_off binds ';' '..' '}'	{ EStruct (Just ($1,True)) (reverse $5) }
        | anyconid 'struct' '{' layout_off '..' '}'             { EStruct (Just ($1,True)) [] }
        | 'struct' '{' layout_off binds ';' '_' '}'             { EStructUpdate Nothing (reverse $4) }
        | anyconid 'struct' '{' layout_off binds ';' '_' '}'    { EStructUpdate (Just $1) (reverse $5) }

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
        : opExpa			        			{ transFix $1 }
		| exp10a			        			{ $1 }
	  
exp000b :: { Exp }
        : exp000a '||' exp00b                   { EOr $1 $3 }
        | exp00b                                { $1 }

exp00b  :: { Exp }
        : exp00a '&&' exp0b                     { EAnd $1 $3 }
        | exp0b                                 { $1 }

exp0b	:: { Exp }
        : opExpb			        			{ transFix $1 }
		| exp10b			        			{ $1 }
	  
opExpa  :: { OpExp }
        : opExpa op MINUS exp10a                { Cons $1 $2 (ENeg $4) }
        | opExpa op exp10a	                	{ Cons $1 $2 $3 }
        | MINUS exp10a			        		{ Nil (ENeg $2) }
        | exp10a op MINUS exp10a                { Cons (Nil $1) $2 (ENeg $4) }
        | exp10a op exp10a		        		{ Cons (Nil $1) $2 $3 }
	  
opExpb  :: { OpExp }
        : opExpa op MINUS exp10b                { Cons $1 $2 (ENeg $4) }
		| opExpa op exp10b	                	{ Cons $1 $2 $3 }
		| MINUS exp10b		                	{ Nil (ENeg $2) }
		| exp10a op MINUS exp10b                { Cons (Nil $1) $2 (ENeg $4) }
		| exp10a op exp10b	                	{ Cons (Nil $1) $2 $3 }

op      :: { Exp }
        : anyvarsym                             { EVar $1 }
        | anyconsym                             { ECon $1 }
		| '`' fexp '`'                          { $2 }

op0     :: { Exp }
        : anyvarsym0                            { EVar $1 }
        | anyconsym                             { ECon $1 }
		| '`' fexp '`'                          { $2 }

exp10a  :: { Exp }
        : 'case' exp 'of' altslist              { ECase $2 $4 }
        | '{' layout_off binds '}'              { EStruct Nothing (reverse $3) }
        | exp10as                               { $1 }

        | '{' layout_off binds ';' '_' '}'      { EStructUpdate Nothing (reverse $3) }

exp10as :: { Exp }											
        : fexp                                             	{ $1 }
        | forall 'do' stmtlist                             	{ EDo $1 Nothing Nothing $3 }
        | forall 'do' '@' varid stmtlist                   	{ EDo $1 (Just $4) Nothing $5 }
        | forall 'do' '@' anyconid stmtlist                	{ EDo $1 Nothing (Just $4) $5 }
        | forall 'do' '@' varid '@' anyconid stmtlist      	{ EDo $1 (Just $4) (Just $6) $7 }
        | forall 'class' stmtlist                          	{ EClass $1 Nothing Nothing $3 }
        | forall 'class' '@' varid stmtlist                	{ EClass $1 (Just $4) Nothing $5 }
        | forall 'class' '@' anyconid stmtlist             	{ EClass $1 Nothing (Just $4) $5 }
        | forall 'class' '@' varid '@' anyconid stmtlist   	{ EClass $1 (Just $4) (Just $6) $7 }
        | before 'action' stmtlist                         	{ EAct $1 Nothing Nothing $3 }
        | before 'action' '@' varid stmtlist               	{ EAct $1 (Just $4) Nothing $5 }
        | before 'action' '@' anyconid stmtlist            	{ EAct $1 Nothing (Just $4) $5 }
        | before 'action' '@' varid '@' anyconid stmtlist  	{ EAct $1 (Just $4) (Just $6) $7 }
        | 'request' stmtlist                               	{ EReq Nothing Nothing $2 }
        | 'request' '@' varid stmtlist                     	{ EReq (Just $3) Nothing $4 }
        | 'request' '@' anyconid stmtlist                  	{ EReq Nothing (Just $3) $4 }
        | 'request' '@' varid '@' anyconid stmtlist        	{ EReq (Just $3) (Just $5) $6 }
      
        | anyconid '{' layout_off binds '}'           		{ EStruct (Just ($1,True)) (reverse $4) } 
        | anyconid '{' layout_off binds '..' '}'      		{ EStruct (Just ($1,False)) (reverse $4) } 
        | anyconid '{' layout_off binds ';' '..' '}'  		{ EStruct (Just ($1,False)) (reverse $4) } 
        | anyconid '{' layout_off '..' '}'                 	{ EStruct (Just ($1,False)) [] } 
        | anyconid '{' layout_off  '}'	                   	{ EStruct (Just ($1,True)) [] }

        | anyconid '{' layout_off binds ';' '_' '}'        	{ EStructUpdate (Just $1) (reverse $4) } 

		| forall after 'send' exp10a                       	{ ESend $1 $2 $4 }
		| forall 'new' exp10a                              	{ ENew $1 $3 }
        | '<-' exp10a                                      	{ EGen $2 }

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
        | '\\\\' tpats apats '->' exp           { EBigLam (reverse $2) (reverse $3) $5 }
        | '\\\\' apats '->' exp                 { EBigLam [] (reverse $2) $4 }
        | '\\\\' tpats '->' exp                 { EBigLam (reverse $2) [] $4 }
        | 'let' bindlist 'in' exp               { ELet $2 $4 }

tpats :: { [Quant] }
        : tpats '{' quant '}'                   { $3 : $1 }
        | '{' quant '}'                         { [$2] }

fexp    :: { Exp }
        : fexp aexp                             { EAp $1 $2 }
        | fexp '@' aexp                         { EPAp $1 $3 }
        | fexp '@' '{' type00 '}'               { ETAp $1 $4 }
        | aexp                                  { $1 }

aexp    :: { Exp }
        : bexp                                  { $1 }
        | conref                                { ECon $1 }

bexp    :: { Exp }
        : bexp '.' varref                       { ESel $1 $3 }
        | bexp '!' cexp                         { EIndex $1 $3 }
        | cexp                                  { $1 }

cexp    :: { Exp }
        : varref                                { EVar $1 }
        | '_'                                   { EVar (qname2 "_") }
        | lit                                   { ELit $1 }
        | '(' '.' varref ')'                    { ESelector $3 }
        | '(' exp ')'                           { EParen $2 }
        | '(' exps ')'                          { ETup (reverse $2) } 
        | '[' list ']'                          { $2 }
        | '(' exp10a op ')'                     { ESectR $2 $3 }
        | '(' op0 fexp ')'                      { ESectL $2 $3 }
        | '(' commas ')'                        { ETupC ($2+1) }
        | '(' ')'                               { ETupC 0 }

lit     :: { Lit }
        : loc INT                               { LInt (Just $1) (readInteger $2) }
        | loc RATIONAL                          { LRat (Just $1) (readRational $2) }
        | loc CHAR                              { LChr (Just $1) $2 }
        | loc STRING                            { LStr (Just $1) $2 }


-- List expressions -------------------------------------------------------------

list    :: { Exp }
        : {- empty -}                           { EList [] }
        | exp                                   { EList [$1] }
        | exps                                  { EList (reverse $1) }
        | exp '..' exp                          { ESeq $1 Nothing $3 }
        | exp ',' exp '..' exp                  { ESeq $1 (Just $3) $5 }
        | exp '|' qualss                        { EComp $1 (reverse $3) }
        | maps                                  { EListUpdate (reverse $1) }

exps    :: { [Exp] }
        : exps ',' exp                          { $3 : $1 }
        | exp ',' exp                           { [$3,$1] }

maps    :: { [(Exp,Exp)] }
        : maps ',' map                          { $3 : $1 }
        | map                                   { [$1] }

map     :: { (Exp,Exp) }
        : exp '->' exp                          { ($1, $3) }


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
		| stmt									{ [$1] }
		| {- empty -}							{ [] }


stmt    :: { Stmt }
        : pat '<-' exp                          { SGen $1 $3 }
        | mexp                                  { SExp $1 }
        | varlist '::' type                     { SSig $1 $3 }
        | 'let' bindlist                        { SLet $2 }
        | pat rhs                               { SEqn $1 $2 }
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
        : opExpas			        			{ transFix $1 }
	| exp10as			        				{ $1 }
	  
exp000bs :: { Exp }
        : exp000as '||' exp00bs                 { EOr $1 $3 }
        | exp00bs                               { $1 }

exp00bs :: { Exp }
        : exp00as '&&' exp0bs                   { EAnd $1 $3 }
        | exp0bs                                { $1 }

exp0bs	:: { Exp }
        : opExpbs			        			{ transFix $1 }
		| exp10bs			        			{ $1 }
	  
opExpas  :: { OpExp }
        : opExpas op MINUS exp10as              { Cons $1 $2 (ENeg $4) }
        | opExpas op exp10as	                { Cons $1 $2 $3 }
        | MINUS exp10as	                        { Nil (ENeg $2) }
        | exp10as op MINUS exp10as              { Cons (Nil $1) $2 (ENeg $4) }
        | exp10as op exp10as		        	{ Cons (Nil $1) $2 $3 }
	  
opExpbs  :: { OpExp }
        : opExpas op MINUS exp10bs              { Cons $1 $2 (ENeg $4) }
		| opExpas op exp10bs	                { Cons $1 $2 $3 }
		| MINUS exp10bs	                        { Nil (ENeg $2) }
		| exp10as op MINUS exp10bs              { Cons (Nil $1) $2 (ENeg $4) }
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

conref  :: { QName2 }
        : anyconid                              { $1 }
        | '(' anyconsym ')'                     { $2 }

varref  :: { QName2 }
		: anyvarid                              { $1 }
        | '(' varsym ')'                        { noqual $2 }
        | '(' qvarsym ')'                       { $2 }

varid  :: { Name2 }
        : loc VARID                             { namePos $1 $2 }

anyvarid :: { QName2 }
		: varid                                 { noqual $1 }
        | loc QVARID                            { qnamePos $1 $2 }

varsym :: { Name2 }
        : varsym0                               { $1 }
        | MINUS									{ $1 }

varsym0 :: { Name2 }
        : loc VARSYM                            { namePos $1 $2 }
        | loc '<'                               { namePos $1 "<" }
        | loc '>'                               { namePos $1 ">" }
        | loc '*'                               { namePos $1 "*" }

MINUS   :: { Name2 }
		: loc '-'                               { namePos $1 "-" }
	
qvarsym :: { QName2 }
        : loc QVARSYM                           { qnamePos $1 $2 }

anyvarsym :: { QName2 }
        : varsym                                { noqual $1 }
        | qvarsym                               { $1 }

anyvarsym0 :: { QName2 }
        : varsym0                               { noqual $1 }
        | qvarsym                               { $1 }

conid  :: { Name2 }
        : loc CONID                             { namePos $1 $2 }

anyconid :: { QName2 }
        : conid                                 { noqual $1 }
        | loc QCONID                            { qnamePos $1 $2 }

consym :: { Name2 }
        : loc CONSYM                            { namePos $1 $2 }

anyconsym :: { QName2 }
        : consym                                { noqual $1 }
        | loc QCONSYM                           { qnamePos $1 $2 }
        

-- Layout ---------------------------------------------------------------------

loc    :: { (Int,Int) }
        : {- empty -}                           {% getSrcLoc }

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
