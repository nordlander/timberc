-----------------------------------------------------------------------------
$Id: ParseMonad.lhs,v 1.1.1.1 2005/02/15 20:03:28 nordland Exp $

Haskell Parser Monad
-----------------------------------------------------------------------------

\begin{code}
module ParseMonad where

import Common

data ParseResult a
    = Ok ParseState a
    | Failed String
      deriving Show

type ParseState = [LexContext]

data LexContext
    = NoLayout
    | Layout Int
    | RecLayout Int
      deriving (Eq, Ord, Show)

newtype PM a 
    =  PM (String		-- input string
          -> (Int,Int)		-- location of last token read (row,col)
	  -> Int		-- current column
	  -> ParseState   	-- layout info
	  -> ParseResult a)

unPM (PM p) = p

instance Monad PM where
    (>>=)	= thenPM
    return 	= returnPM
    fail  	= failPM

m `thenPM` k	= PM $ \i l c s -> 
                    case (unPM m) i l c s of 
                        Failed s -> Failed s
                        Ok s' a  -> case k a of PM k' -> k' i l c s'
returnPM a	= PM $ \i l c s -> Ok s a
failPM a 	= PM $ \i l c s -> Failed a

runPM (PM p) i l c s =
    case p i l c s of
        Ok _ a -> a
	Failed err -> error err

runPM2 (PM p) input =
    case p input (1,1) 0 [] of
        Ok _ result -> return result
        Failed msg  -> fail msg

getSrcLoc :: PM (Int,Int)
getSrcLoc = PM $ \i l c s -> Ok s l

pushContext :: LexContext -> PM ()
pushContext ctxt =
    PM $ \i l c s -> Ok (ctxt:s) ()


popContext :: PM ()
popContext = PM $ \i loc c stk ->
    case stk of
    (_:s) -> Ok s ()
    []    -> Failed $ show loc ++
                      ": parse error (possibly incorrect indentation)"


parseError :: String -> PM a
parseError err =
    PM $ \r (l,c) -> (unPM $ fail $ "Syntax error at line "++show l++", column "++show c++ "\n") r (l,c)

\end{code}
