
module Lexer (Token(..), lexer, readInteger, readNumber, readRational) where


import ParseMonad
import Char
import Ratio
import Token
import Common


{-

The source location, (y,x), is the coordinates of the previous token.
col is the current column in the source file.  If col is 0, we are
somewhere at the beginning of the line before the first token.

Setting col to 0 is used in two places: just after emitting a virtual
close brace due to layout, so that next time through we check whether
we also need to emit a semi-colon, and at the beginning of the file,
to kick off the lexer.

-}
lexer :: (Token -> PM a) -> PM a
lexer cont =
    PM $ \input (y,x) col ->
    if col == 0 then tab y x   True  input  col
                else tab y col False input  col -- throw away old x
    where
    -- move past whitespace and comments
    tab y x bol []          col = (unPM $ cont EOF) [] (y,x) col
    tab y x bol ('\t':s)    col = tab y (nextTab x) bol s col
    tab y x bol ('\n':s)    col = newLine s y col
    tab y x bol ('-':'-':s) col = newLine (drop 1 (dropWhile (/= '\n') s))
                                      y col
    tab y x bol ('{':'-':s) col = nestedComment tab y x bol s col
    tab y x bol (c:s)       col
                 | isSpace c = tab y (x + 1) bol s col
                 | otherwise =
                     if bol then
                         (unPM $ lexBOL cont)   (c:s) (y,x) x
                     else
                         (unPM $ lexToken cont) (c:s) (y,x) x

    newLine s y col = tab (y + 1) 1 True s col


nextTab x = x + (tab_length - (x - 1) `mod` tab_length)

{-

When we are lexing the first token of a line, check whether we need to
insert virtual semicolons or close braces due to layout.

-}
lexBOL :: (Token -> PM a) -> PM a
lexBOL cont =
    PM $ \ s loc@(y,x) col ctx ->
    if need_close_curly x ctx then 
        --trace "layout: inserting '}'\n" $
        -- Set col to 0, indicating that we're still at the
        -- beginning of the line, in case we need a semi-colon too.
        -- Also pop the context here, so that we don't insert
        -- another close brace before the parser can pop it.
        (unPM $ cont VRightCurly) s loc 0 (tail ctx)
    else if need_semi_colon x ctx then
        --trace "layout: inserting ';'\n" $
        (unPM $ cont SemiColon) s loc col ctx
    else
        (unPM $ lexToken cont)  s loc col ctx
    where
        need_close_curly x []    = False
        need_close_curly x (i:_) = case i of
                                   NoLayout -> False
                                   Layout n -> x < n
                                   RecLayout n -> False

        need_semi_colon x []     = False
        need_semi_colon x (i:_)  = case i of
                                   NoLayout -> False
                                   Layout n -> x == n
                                   RecLayout n -> x == n


lexToken :: (Token -> PM a) -> PM a
lexToken cont =
    PM lexToken'
    where
    lexToken' (c:s) loc@(y,x') x =
        -- trace ("lexer: y = " ++ show y ++ " x = " ++ show x ++ "\n") $ 
        case c of
        -- First the special symbols
        '(' -> special LeftParen
        ')' -> special RightParen
        ',' -> special Comma
        ';' -> special SemiColon
        '[' -> special LeftSquare
        ']' -> special RightSquare
        '`' -> special BackQuote
        '{' -> special LeftCurly
        '}' -> \state ->
               case state of
               (_:ctxt) ->
                   special RightCurly ctxt -- pop context on }
               []       ->
                   (unPM $ parseError "parse error (possibly incorrect indentation)")
                   s loc x []

        '\'' -> (unPM $ lexChar cont)   s loc (x + 1)
        '\"' -> (unPM $ lexString cont) s loc (x + 1)

        '_' | null s || not (isIdent (head s)) -> special Wildcard
        
        c | isDigit c ->
              case lexInt (c:s) of
              Decimal (n, rest) ->
                  case rest of
                  ('.':c2:rest2) | isDigit c2 ->
                                     case lexFloatRest (c2:rest2) of
                                     Nothing -> (unPM $
                                                 parseError "illegal float.")
                                                s loc x
                                     Just (n2,rest3) ->
                                         let f = n ++ ('.':n2) in
                                         forward (length f) (FloatTok f) rest3
                  _ -> forward (length n) (IntTok n) rest
              Octal       (n,rest) -> forward (length n) (IntTok n) rest
              Hexadecimal (n,rest) -> forward (length n) (IntTok n) rest

          | isLower c ->
              let (vidtail, rest) = span isIdent s
                  vid             = c:vidtail
                  l_vid           = 1 + length vidtail
              in
                  case lookup vid reserved_ids of
                  Just keyword -> forward l_vid keyword rest
                  Nothing      -> forward l_vid (VarId vid) rest

          | isUpper c ->
              let (contail, rest) = span isIdent s
                  l_con           = 1 + length contail
                  con             = c:contail
              in
                  case lookup con reserved_ids of
                  Just keyword -> forward l_con keyword rest
                  Nothing      -> -- lexQual l_con con rest
                                      (forward l_con (ConId con) rest)
                  
          | isSymbol c ->
              let (symtail, rest) = span isSymbol s
                  sym             = c : symtail
                  l_sym           = 1 + length symtail
              in
                  case rest of
                  '\'' : _ -> let (qualtail,rest') = span isIdent rest
                              in  case c of 
                                   ':' -> forward (l_sym + length qualtail) (ConSym (sym ++ qualtail)) rest'
                                   _  ->  forward (l_sym + length qualtail) (VarSym (sym ++ qualtail)) rest' 
 
                  _        -> case lookup sym reserved_ops of
                                Just t  -> forward l_sym t rest
                                Nothing -> case c of
                                 ':' -> forward l_sym (ConSym sym) rest
                                 _   -> forward l_sym (VarSym sym) rest

          | otherwise ->
              (unPM $
               parseError ("illegal character \'" ++
                           showLitChar c "" ++ "\'\n"))
              s loc x
                  
      where
      special t = forward 1 t s
      forward n t s = (unPM $ cont t) s loc (x + n)
 
      lexFloatRest r = case span isDigit r of
                       (r2, 'e':r3) -> lexFloatExp (r2 ++ "e") r3
                       (r2, 'E':r3) -> lexFloatExp (r2 ++ "e") r3
                       f@(r2,   r3) -> Just f
 
      lexFloatExp r1 ('-':r2) = lexFloatExp2 (r1 ++ "-") r2
      lexFloatExp r1 ('+':r2) = lexFloatExp2 (r1 ++ "+") r2
      lexFloatExp r1      r2  = lexFloatExp2 r1          r2

      lexFloatExp2 r1 r2 = case span isDigit r2 of
                           ("", _ ) -> Nothing
                           (ds, r3) -> Just (r1++ds,r3)
    lexToken' _  _ _ =
        error "Lexer.lexToken: Internal error: empty input stream."


lexInt ('0':o:d:r) | toLower o == 'o' && isOctDigit d
    = let (ds, rs) = span isOctDigit r
      in
           Octal       ('0':'o':d:ds, rs)
lexInt ('0':x:d:r) | toLower x == 'x' && isHexDigit d
    = let (ds, rs) = span isHexDigit r
      in 
           Hexadecimal ('0':'x':d:ds, rs)
lexInt r = Decimal     (span isDigit r)


lexChar :: (Token -> PM a) -> PM a
lexChar cont = PM lexChar'
    where
    lexChar' s loc@(y,_) x =
        case s of
        '\\':s ->
            let (e, s2, i) =
                  runPM (escapeChar s) "" loc x []
            in
                charEnd e s2 loc (x + i)
        c:s  -> charEnd c s  loc (x + 1)
        []   -> error "Lexer.lexChar: Internal error: empty list."

    charEnd c ('\'':s)   =
        \loc x -> (unPM $ cont (Character c)) s loc (x + 1)
    charEnd c s         =
        (unPM $ parseError "improperly terminated character constant.") s 


lexString :: (Token -> PM a) -> PM a
lexString cont = PM lexString'
    where
    lexString' s loc@(y',_) x = loop "" s x y'
        where
        loop e s x y =
            case s of
            '\\':'&':s  -> loop e s (x+2) y
            '\\':c:s | isSpace c -> stringGap e s (x + 2) y
                     | otherwise ->
                         let (e', sr, i) =
                               runPM (escapeChar (c:s)) ""  loc x [] 
                         in
                             loop (e':e) sr (x+i) y
            '\"':s{-"-} -> (unPM $ cont (StringTok (reverse e))) s  loc (x + 1)
            c:s       -> loop (c:e) s (x + 1) y
            []          -> (unPM $ parseError "improperly terminated string.")
                                    s  loc x

        stringGap e s x y =
            case s of
                '\n':s -> stringGap e s 1 (y + 1)
                '\\':s -> loop e s (x + 1) y
                c:s' | isSpace c -> stringGap e s' (x + 1) y
                     | otherwise ->
                         (unPM $ parseError "illegal character in string gap.")
                         s  loc x
                []     -> error "Lexer.stringGap: Internal error: empty list."


escapeChar :: String -> PM (Char, String, Int)
escapeChar s = case s of
    -- Production charesc from section B.2 (Note: \& is handled by caller)
   'a':s         -> return ('\a', s, 2)
   'b':s         -> return ('\b', s, 2)
   'f':s         -> return ('\f', s, 2)
   'n':s         -> return ('\n', s, 2)
   'r':s         -> return ('\r', s, 2)
   't':s         -> return ('\t', s, 2)
   'v':s         -> return ('\v', s, 2)
   '\\':s        -> return ('\\', s, 2)
   '"':s         -> return ('\"', s, 2)      -- "
   '\'':s        -> return ('\'', s, 2)

    -- Production ascii from section B.2
   '^':x@(c:s)   -> cntrl x
   'N':'U':'L':s -> return ('\NUL', s, 4)
   'S':'O':'H':s -> return ('\SOH', s, 4)
   'S':'T':'X':s -> return ('\STX', s, 4)
   'E':'T':'X':s -> return ('\ETX', s, 4)
   'E':'O':'T':s -> return ('\EOT', s, 4)
   'E':'N':'Q':s -> return ('\ENQ', s, 4)
   'A':'C':'K':s -> return ('\ACK', s, 4)
   'B':'E':'L':s -> return ('\BEL', s, 4)
   'B':'S':s     -> return ('\BS',  s, 3)
   'H':'T':s     -> return ('\HT',  s, 3)
   'L':'F':s     -> return ('\LF',  s, 3)
   'V':'T':s     -> return ('\VT',  s, 3)
   'F':'F':s     -> return ('\FF',  s, 3)
   'C':'R':s     -> return ('\CR',  s, 3)
   'S':'O':s     -> return ('\SO',  s, 3)
   'S':'I':s     -> return ('\SI',  s, 3)
   'D':'L':'E':s -> return ('\DLE', s, 4)
   'D':'C':'1':s -> return ('\DC1', s, 4)
   'D':'C':'2':s -> return ('\DC2', s, 4)
   'D':'C':'3':s -> return ('\DC3', s, 4)
   'D':'C':'4':s -> return ('\DC4', s, 4)
   'N':'A':'K':s -> return ('\NAK', s, 4)
   'S':'Y':'N':s -> return ('\SYN', s, 4)
   'E':'T':'B':s -> return ('\ETB', s, 4)
   'C':'A':'N':s -> return ('\CAN', s, 4)
   'E':'M':s     -> return ('\EM',  s, 3)
   'S':'U':'B':s -> return ('\SUB', s, 4)
   'E':'S':'C':s -> return ('\ESC', s, 4)
   'F':'S':s     -> return ('\FS',  s, 3)
   'G':'S':s     -> return ('\GS',  s, 3)
   'R':'S':s     -> return ('\RS',  s, 3)
   'U':'S':s     -> return ('\US',  s, 3)
   'S':'P':s     -> return ('\SP',  s, 3)
   'D':'E':'L':s -> return ('\DEL', s, 4)


   -- Depending upon the compiler/interpreter's Char type, these yield either
   -- just 8-bit ISO-8859-1 or 2^16 UniCode.  The report says it should be the
   -- latter.

   -- Octal representation of a character
   'o':s           -> let (ds, s') = span isOctDigit s
                          n        = readNumber 8 ds
                      in 
                          numberToChar n s' (length ds + 1)

   -- Hexadecimal representation of a character
   'x':s           -> let (ds, s') = span isHexDigit s
                          n        = readNumber 16 ds
                      in
                          numberToChar n s' (length ds + 1)
 
   -- Base 10 representation of a charactef
   d:s | isDigit d -> let (ds, s') = span isDigit s
                          n        = readNumber 10 (d:ds)
                      in 
                          numberToChar n s' (length ds + 1)

   _               -> parseError "illegal escape sequence."

   where numberToChar n s l_n =
             if n < (toInteger $ fromEnum (minBound :: Char)) ||
                n > (toInteger $ fromEnum (maxBound :: Char)) then
                 parseError "illegal character literal (number out of range)."
             else
                 return (chr $ fromInteger n, s, l_n)
            
{-

Production cntrl from section B.2

-}
cntrl :: String -> PM (Char, String, Int)
cntrl (c   :s) | isUpper c = return (chr (ord c - ord 'A'), s, 2)
cntrl ('@' :s)             = return ('\^@', s, 2)
cntrl ('[' :s)             = return ('\^[', s, 2)
cntrl ('\\':s)             = return ('\^\', s, 2)
cntrl (']' :s)             = return ('\^]', s, 2)
cntrl ('^' :s)             = return ('\^^', s, 2)
cntrl ('_' :s)             = return ('\^_', s, 2)
cntrl _                    = parseError "illegal control character"


nestedComment cont y x bol s  col =
    case s of
    '-':'}':s -> cont y (x + 2) bol s  col
    '{':'-':s -> nestedComment (nestedComment cont) y (x + 2) bol s  col
    '\t':s    -> nestedComment cont y (nextTab x) bol s  col
    '\n':s    -> nestedComment cont (y + 1) 1 True s  col
    c:s       -> nestedComment cont y (x + 1) bol s  col
    []        -> error "Open comment at end of file"



readInteger :: String -> Integer
readInteger ('0':'o':ds) = readInteger2  8 isOctDigit ds
readInteger ('0':'O':ds) = readInteger2  8 isOctDigit ds
readInteger ('0':'x':ds) = readInteger2 16 isHexDigit ds
readInteger ('0':'X':ds) = readInteger2 16 isHexDigit ds
readInteger          ds  = readInteger2 10 isDigit    ds

readNumber :: Integer -> String -> Integer
readNumber radix ds = readInteger2 radix (const True) ds

readInteger2 :: Integer -> (Char -> Bool) -> String -> Integer
readInteger2 radix isDig ds 
  = foldl1 (\n d -> n * radix + d) (map (fromIntegral . digitToInt) 
                                    (takeWhile isDig ds))

readRational :: String -> Rational
readRational xs
    = (readInteger (i ++ m))%1 * 10^^((case e of
                                       ""       -> 0
                                       ('+':e2) -> read e2
                                       _        -> read e) - length m)
      where (i, r1) = span isDigit xs
            (m, r2) = span isDigit (dropWhile (== '.') r1)
            e       = dropWhile (== 'e') r2

