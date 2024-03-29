module Prog where
 
import ARM
--import MyString
import Graphic
import Debug

import Buttons

struct Ticker where
	startme :: Action

counter :: (String -> Int -> Request ()) -> Time -> Class Ticker
counter printfunc interval = class
    loop tal = before millisec 100 action
                        printfunc "tal" tal
                        after interval send loop (tal+1)
    startme = loop 0
    result Ticker {..}


root world =
    class
        env = new arm world
        t = new tft world
        g = new graphic t
        d1 = new debug env.debug
        d2 = new debug g.print
        c1 = new counter d1.printvardec (millisec 200)
        c2 = new counter d2.printvardec (sec 1)
        btns = new buttons env
    
        result action
            g.clear
            d2.printstr "Hello World!\n"
            -- c1.startme
            c2.startme
            btns.init
