module Prog where
 
import ARM
import MyString
import Graphic -- extends Print
import Debug -- extends Print

import WORM
import Buttons

struct Counterme where
	startme :: Action

counter :: (String -> Int -> Request ()) -> Time -> Class Counterme
counter printfunc interval = class
  loop tal = action
    printfunc "tal" tal
    after interval loop (tal+1)
  startme = before (millisec 100) loop 0
  result Counterme {..}

struct Test where
  print_test :: Request ()

test :: Debug -> Class Test
test d =
  class
    print_test = request
      d.printstr "testclass"
      result ()
    result Test {..}

root :: Env -> TFT -> Class Prog
root env tft =
  class
    io1dir = 0xe0028018
    io1pin = 0xe0028010

    g1 = new graphic tft
    d1 = new debug env.debug
    d2 = new debug g1.print
    t1 = new test d2
    c1 = new counter (d1.printvardec) (millisec 200)
    c2 = new counter (d2.printvardec) (sec 1)
    buttons1 = new buttons env
    
    msg = Just "Hello World!\n"
  
    worm1 = new worm env tft buttons1

--    blink2 = action
--      env.membitclear io1pin (bset 0 18)
--      after (sec 1) blink1

--    blink1 = action
--      env.membitset io1pin (bset 0 18)
--      after (sec 1) blink2

    start2 = action
      -- c1.startme
      c2.startme

      buttons1.init

--      env.membitset io1dir 262144
--      after (sec 1) blink1

--      worm1

    start = action
      g1.clear
      d2.printstr (fromJust msg)
      start2
    result start

{-
root :: Env -> TFT -> Class Action
root env tft = class
     g = new graphic tft
     tick = action
              g.printstr "Hullo!\n"
              after (sec 1) tick
     result action
         g.clear
         tick
-}