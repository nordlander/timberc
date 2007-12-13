module MasterMind where

import Data'Functional'List
import RandomGenerator
import POSIX

{-
Mastermind where the program does the guessing. Answers are given as two integers (separated by space(s) on 
one row, indicating number of bulls and cows. Command line argument acts as seed for random number generation.
-}

data Colour = Red | Blue | Green | Yellow | Black | White

instance Eq Colour =
  Red    == Red    = True
  Blue   == Blue   = True
  Green  == Green  = True
  Yellow == Yellow = True
  Black  == Black  = True
  White  == White  = True
  _      == _      = False

  x /= y = not (x==y)

instance Show Colour =
  show Red    = "Red"
  show Blue   = "Blue"
  show Green  = "Green"
  show Yellow = "Yellow"
  show Black  = "Black"
  show White  = "White"

type Guess = [Colour]

record Answer =
  exact :: Int
  near  :: Int

instance Eq Answer =
  a1 == a2 = a1.exact == a2.exact && a1.near == a2.near

  a1 /= a2 = not (a1 == a2)

type Board = [(Guess,Answer)]

allColours = [Red, Blue, Green, Yellow, Black, White]

allCodes :: Int -> [Guess]
allCodes 0 = [[]]
allCodes n = concat [[c:cs | c <- allColours] | cs <- allCodes (n-1)]

mkAnswer :: String -> Answer
mkAnswer cs = Answer{ exact = e, near = n }
    where [e,n] = map read (words cs)

answer :: Guess -> Guess -> Answer
answer guess code = Answer {exact=e, near = n}
   where e = equals guess code

         n  = sum [min (count c guess) (count c code) | c <- allColours] - e

         count c xs = length [x | x <- xs, x==c]

         equals [] [] = 0
         equals (x:xs) (y:ys)
           |x==y      = 1 + equals xs ys
           |otherwise = equals xs ys


contradictions :: Board -> Guess -> Board
contradictions board c = [(g,r) | (g,r) <- board, answer g c /= r]

consistent :: Board -> [Guess] -> [Guess]
consistent board cs = [ c | c <- cs, null (contradictions board c)]

read str = r (reverse str)
 where r (c:cs) = ord c - ord '0' + 10 * r cs
       r [] = 0


data State = Idle | JustGuessed | GameOver
           
root env = template
  gen <- baseGen (read (head (tail env.argv)))
  board := []
  cs := [] 
  state := Idle

  shift = do
     r <- gen.next
     n = r `mod` (length cs)
     ys = take n cs
     zs = drop n cs
     cs := zs ++ ys
  
  start = action
    env.stdout.write "Welcome to Mastermind!\n"
    startGame
   
  startGame = do
    board := []
    cs := []
    state := Idle
    env.stdout.write "Choose your secret. Press return when ready.\n"

  mkGuess = do
     case cs of
        [] -> do env.stdout.write "Contradictory answers!\n"
                 state := GameOver
                 checkQuit
        _  -> do shift
                 env.stdout.write ("My guess: "++show(head cs)++"\n")

  checkQuit = do
     env.stdout.write "Do you want to play again? (y/n) "

  io = action
    inp <- env.stdin.read
    case state of
      Idle ->        do cs := allCodes 4
                        state := JustGuessed
                        mkGuess
      JustGuessed -> do ans = mkAnswer inp
                        if ans.exact == 4 then
                           env.stdout.write "Yippee!\n"
                           state := GameOver
                           checkQuit
                        else
                           c:cs' = cs
                           board := (c,ans) : board
                           -- cs := [c | c <- cs', consistent board c]
                           cs := consistent board cs'
                           mkGuess
      
      GameOver ->    do if head inp == 'y' then
                           startGame
                        else
                           env.exit 0
                           return ""

  return Prog {..}


