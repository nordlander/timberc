module PP (module PP, module Text.PrettyPrint) where

import Text.PrettyPrint hiding (TextDetails(..))
import Char(showLitChar)


class Pr a where
    pr   :: a -> Doc
    pr x = prn 0 x

    prn  :: Int -> a -> Doc
    prn n x = pr x

    vpr  :: [a] -> Doc
    vpr xs = vcat (map pr xs)

    hpr  :: Char -> [a] -> Doc
    hpr c xs = hsep (punctuate (char c) (map pr xs))

    dump :: a -> IO ()
    dump x = putStr (render (pr x))


instance Pr Int where 
  pr = int

-- XXX Is this correct?
-- AJG It is needed by Main, but it looks wrong.

instance Pr (String, String) where
    pr (a, b) = text a <> text b

infixl 4 $$$
a $$$ b       = a $$ text " " $$ b

vcat2 xs      = vcat (map ($$ text " ") xs)

backQuotes p  = char '`'  <> p <> char '`'
litChar       = charQuotes . text . lit
litString     = doubleQuotes . text . concat . map lit
lit c         = showLitChar c ""
charQuotes p  = char '\'' <> p <> char '\''
curlies       = braces

commasep f xs                   = hsep (punctuate comma (map f xs))


show' :: Pr a => a -> String
show' = render . pr

vshow :: Pr a => [a] -> String
vshow = render . vpr

showlist :: Pr a => [a] -> String
showlist xs = render (text "(" <> hpr ',' xs <> text ")")