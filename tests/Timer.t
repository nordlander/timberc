module Timer where

struct Timer where
  start  :: Action
  stop   :: Action
  reset  :: Action
  read   :: Request Int

timer = class

  time := 0
  running := False

  tick = action
     if running then
        time := time+1
        after (millisec 10) tick

  result struct
     start = action
        running := True
        after (millisec 10) tick

     stop = action
        running := False

     reset = action
        time := 0
 
     read = request
        result time

