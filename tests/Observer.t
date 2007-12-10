module Observer where

import Data'Functional'List

sequence :: [Action] -> Cmd a ()
sequence [] = do return ()
sequence (s : ss) = do {s; sequence ss}

record Object =
  self :: PID

record Observer a < Object =
  update :: a -> Action

instance Eq (Observer a) =
  o1 == o2 = o1.self == o2.self
  o1 /= o2 = o1.self /= o2.self

record Observable a =
  subscribe   :: Observer a -> Action
  unsubscribe :: Observer a -> Action
  
record Notifier a =
  setChanged :: Action
  notify :: a -> Action

record Publisher a < Observable a, Notifier a

publisher :: Template (Publisher a)
publisher = template

   olst := []
   changed := False

   subscribe obs = action
      olst := nub (obs : olst)

   unsubscribe obs = action
      olst := delete obs olst

   setChanged = action 
      changed := True

   notify a = action
      if changed then
         sequence [obs.update a | obs <- olst]
         changed := False

   return Publisher {..}


record Counter =
   incr  :: Action
   read  :: Request Int
   reset :: Action

record ObservableCounter < Observable Int, Counter

obsCounter :: Template ObservableCounter
obsCounter = template

   val := 0
   Publisher {..} <- publisher

   set v  = do 
      val := v
      setChanged
      notify v

   incr  = action set (val + 1)

   reset = action set 0

   read = request
      return val

   return ObservableCounter {..}
