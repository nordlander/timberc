-- $Id: Token.hs,v 1.3 2005/08/17 06:58:52 sydow Exp $

module Token where

import Char


data Token 
    = VarId      (String,String)
    | ConId      (String,String)
    | VarSym     (String,String)
    | ConSym     (String,String)
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
    | KW_Implicit 
    | KW_Import   
    | KW_In       
    | KW_Let
    | KW_Module
    | KW_New   
    | KW_Of    
    | KW_Private   
    | KW_Request
    | KW_Result
    | KW_Struct
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
        ( "implicit",  KW_Implicit ),     
        ( "in",        KW_In ),       
        ( "let",       KW_Let ),      
        ( "module",    KW_Module ),   
        ( "new",       KW_New ),
        ( "of",        KW_Of ),
        ( "private",   KW_Private ),
        ( "request",   KW_Request ),
        ( "result",    KW_Result ),
        ( "struct",    KW_Struct ),
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
