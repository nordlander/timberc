-- $Id: Token.hs,v 1.3 2005/08/17 06:58:52 sydow Exp $

module Token where

import Char


data Token 
    = VarId      String
    | ConId      String
    | VarSym     String
    | ConSym     String
    | IntTok     String
    | FloatTok   String
    | Character  Char
    | StringTok  String
{-

Symbols

-}
    | LeftParen
    | RightParen
    | SemiColon
    | LeftCurly
    | RightCurly
    | VRightCurly                       -- a virtual close brace
    | LeftSquare
    | RightSquare
    | Comma
    | BackQuote
{-

Reserved operators

-}
    | Assign
    | Dot
    | DotDot
    | DoubleColon
    | Equals
    | Backslash
    | Bar
    | LeftArrow
    | RightArrow
    | At
    | Tilde
    | Wildcard
    | Backslash2
{-

Reserved Ids

-}
    | KW_Action
    | KW_After
    | KW_Before
    | KW_Case     
    | KW_Class    
    | KW_Data     
    | KW_Default  
    | KW_Do       
    | KW_Else   
    | KW_Elsif
    | KW_Forall
    | KW_If    
    | KW_Import   
    | KW_In       
    | KW_Instance 
    | KW_Let
    | KW_Module   
    | KW_Of    
    | KW_Private   
    | KW_Record
    | KW_Request
    | KW_Return
    | KW_Template
    | KW_Then     
    | KW_Type  
    | KW_Use   
    | KW_Where
    | KW_While
    | EOF
      deriving (Eq, Show)


reserved_ops :: [(String, Token)]
reserved_ops
    = [
        ( ".",  Dot ),    
        ( "..", DotDot ),    
        ( "::", DoubleColon ),
        ( ":=", Assign ),
        ( "=",  Equals ),    
        ( "\\", Backslash ), 
        ( "|",  Bar ),       
        ( "<-", LeftArrow ), 
        ( "->", RightArrow ),
        ( "@",  At ),
        ( "_",  Wildcard ),
        ( "\\\\", Backslash2 )
      ]


reserved_ids :: [(String, Token)]
reserved_ids
    = [
        ( "action",    KW_Action ),
        ( "after",     KW_After ),
        ( "before",    KW_Before ),
        ( "case",      KW_Case ),     
        ( "class",     KW_Class ),    
        ( "data",      KW_Data ),
        ( "default",   KW_Default),   
        ( "do",        KW_Do ),       
        ( "else",      KW_Else ),
        ( "elsif",     KW_Elsif ),     
        ( "forall",    KW_Forall ),
        ( "if",        KW_If ),
        ( "import",    KW_Import ),       
        ( "in",        KW_In ),       
        ( "instance",  KW_Instance ), 
        ( "let",       KW_Let ),      
        ( "module",    KW_Module ),   
        ( "of",        KW_Of ),
        ( "private",   KW_Private ),
        ( "record",    KW_Record ),
        ( "return",    KW_Return ),
        ( "request",   KW_Request ),
        ( "template",  KW_Template ),   
        ( "then",      KW_Then ),     
        ( "type",      KW_Type ),  
        ( "use",       KW_Use ),   
        ( "where",     KW_Where ),
        ( "while",     KW_While )
      ]



tab_length = 8 :: Int

isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
isSymbol c = elem c ":!#$%&*+./<=>?@\\^|-~"

data LexInt =
      Decimal     (String,String)
    | Octal       (String,String)
    | Hexadecimal (String,String)
