module Timer where

record Timer =
  startT :: Action
  stop   :: Action
  reset  :: Action
  readT  :: Request Int

timer = template

  time := 0
  running := False

  tick = action
     if running then
        time := time+1
        after (millisec 10) tick
   
  startT = action
     running := True
     after (millisec 10) tick

  stop = action
     running := False

  reset = action
     time := 0
     running := False
 
  readT = request
     return time

  return Timer{..}
