module WORM where

import ARM
import Debug
import Buttons

struct Window where 
    wFillRectangle :: Color -> Int -> Int -> Int -> Int -> Request ()

windowObj env tft = class
    wFillRectangle Red x y xs ys = request 
        tft.drawbox 0 x y xs ys 
        result ()
    wFillRectangle Blue x y xs ys = request 
        tft.drawbox 4 x y xs ys 
        result ()
    wFillRectangle Green x y xs ys = request 
        tft.drawbox 2 x y xs ys 
        result ()
    wFillRectangle Black x y xs ys = request 
        tft.drawbox 12 x y xs ys 
        result ()
    wFillRectangle White x y xs ys = request 
        tft.drawbox 22 x y xs ys 
        result ()
    result Window {..}

struct Memory a where 
    mInsert :: a -> Request ()
    mRemove :: Request a
    mCheck  :: a -> Request Bool
    mLength :: Request Int

memoryObj i = class
    m := [i]

    mInsert a = request
        m :=  m ++ (a:[]) 
        result ()

    mRemove   = request
        l:ls = m 
        m := ls
        result l

    mCheck  a = request 
        result elem a m

    mLength = request
        result length m

    result Memory {..}


data Direction = North | West | South | East
data Color = Red | Blue | Green | Black | White


left North  = West
left West   = South
left South  = East
left East   = North

right North  = East
right West   = North
right South  = West
right East   = South

size = 4
grow = 2

struct Worm where
    wTick   :: Action
    wLeft   :: Action
    wRight  :: Action

wormObj  env win mem mem2 xstart ystart dirstart color = class
    x := xstart 
    y := ystart 
    dir := dirstart
    nextdir := dir
    count := 0

    wTick = action
        dir := nextdir
        case dir of
           North    -> y := y - 1
           West     -> x := x - 1
           South    -> y := y + 1
           East     -> x := x + 1

        check1 <- mem.mCheck (x, y)
        check2 <- mem2.mCheck (x, y)

        if (check1 || check2) then
            env.debug "You died!!\n"
        else       
          t <- win.wFillRectangle color (x*size) (y*size) (size -1) (size -1) 

          mem.mInsert (x, y)
          if ((count `mod` grow) /= 0) then
            (x1, y1) <- mem.mRemove 
            t <- win.wFillRectangle White (x1*size) (y1*size) (size -1) (size -1)

          count := count + 1   
          after (millisec 100) wTick

    wLeft = action
        nextdir := left dir

    wRight = action
        nextdir := right dir

    result Worm {..}

--worm :: Debug -> TFT -> Buttons -> (Int -> Action) -> Class Prog
worm env tft but = class
  win = new windowObj env tft
  m1 = new memoryObj (0::Int, 30::Int) -- (x, y)
  m2 = new memoryObj (80::Int, 40::Int)

  result action
    env.debug "Killerworm started!\n"

    w1 = new wormObj env win m1 m2 (0::Int) (30::Int) East Green
    w2 = new wormObj env win m2 m1 (80::Int) (40::Int) West Blue
    w1.wTick
    w2.wTick
    but.handle 3 w1.wRight
    but.handle 4 w1.wLeft
    but.handle 5 w2.wRight
    but.handle 6 w2.wLeft
