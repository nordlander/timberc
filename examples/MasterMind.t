module MasterMind where

import Data'Functional'List
import RandomGenerator
import POSIX

{-
Standard Mastermind, where the program does the guessing. Answers are given as two integers (separated by space(s) on 
one row, indicating number of bulls and cows. Command line argument acts as seed for random number generation.
-}

data Colour = Red | Blue | Green | Yellow | Black | White

default eqColour    :: Eq Colour
        showColour  :: Show Colour 
        parseColour :: Parse Colour
        enumColour  :: Enum Colour

type Guess = [Colour]

data Answer = Answer Int Int

default eqAnswer :: Eq Answer

showAnswer (Answer e n) = show e ++ ' ' : show n

exact (Answer e _) = e

type Board = [(Guess,Answer)]

allColours = [Red .. White]

allCodes :: Int -> [Guess]
allCodes 0 = [[]]
allCodes n = concat [[c:cs | c <- allColours] | cs <- allCodes (n-1)]

mkAnswer :: String -> Answer
mkAnswer cs = Answer e n
    where [e,n] = map parse (words cs)

answer :: Guess -> Guess -> Answer
answer guess code = Answer e n
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

showGuess [g]    = show g
showGuess (g:gs) = show g ++ ' ' : showGuess gs


data State = Idle | JustGuessed | Contradiction | GameOver 
           
root env = class
  
  gen = new baseGen (parse (head (tail env.argv)))

  board := []
  cs := [] 
  state := Idle

  shift = do
     r <- gen.next
     n = r `mod` (length cs)
     ys = take n cs
     zs = drop n cs
     cs := zs ++ ys
  
  startGame = do
    board := []
    cs := allCodes 4
    state := Idle
    env.stdout.write "Choose your secret. Press return when ready.\n"

  mkGuess = do
     case cs of
        [] -> do state := Contradiction
                 env.stdout.write "Contradictory answers!\nTell me your secret: "

        _  -> do shift
                 env.stdout.write ("My guess: "++showGuess (head cs)++"\nAnswer (two integers): ")

  checkQuit = do
     env.stdout.write "Do you want to play again? (y/n) "

  start = action
    env.stdout.write "Welcome to Mastermind!\n"
    startGame
   
  io = action
    inp <- env.stdin.read
    case state of
      Idle ->         do state := JustGuessed
                         mkGuess

      JustGuessed ->  do ans = mkAnswer inp
                         if exact ans == 4 then
                            env.stdout.write "Yippee!\n"
                            state := GameOver
                            checkQuit
                         else
                            c:cs' = cs
                            board := (c,ans) : board
                            cs := consistent board cs'
                            mkGuess

      Contradiction -> do ss = map parse (words inp)
                          (gs,a):_ = contradictions board ss
                          env.stdout.write ("When I guessed "++showGuess gs++", you answered "++showAnswer a++".\n")
                          env.stdout.write ("Correct answer should have been "++showAnswer (answer gs ss)++".\n")
                          state := GameOver
                          checkQuit

      GameOver ->     do if head inp == 'y' then
                            startGame
                         else
                            env.exit 0

  result Prog {..}


