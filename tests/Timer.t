module Timer where

record Timer =
  start  :: Action
  stop   :: Action
  reset  :: Action
  read   :: Request Int

timer = template

  time := 0
  running := False

  tick = action
     if running then
        time := time+1
        after (millisec 10) tick

  start = action
     running := True
     after (millisec 10) tick

  stop = action
     running := False

  reset = action
     time := 0
 
  read = request
     return time

  return Timer{..}
