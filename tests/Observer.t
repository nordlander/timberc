module Observer where

import Data'Functional'List

sequence :: [Action] -> Cmd a ()
sequence [] = do result ()
sequence (s : ss) = do {s; sequence ss}

struct Object where
  self :: PID

struct Observer a < Object where
  update :: a -> Action

implicit eqObserver :: Eq (Observer a) 
eqObserver = struct
  o1 == o2 = o1.self == o2.self
  o1 /= o2 = o1.self /= o2.self

struct Observable a where
  subscribe   :: Observer a -> Action
  unsubscribe :: Observer a -> Action
  
struct Notifier a where
  setChanged :: Action
  notify :: a -> Action

struct Publisher a < Observable a, Notifier a

publisher :: Class (Publisher a)
publisher = class

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

   result Publisher {..}


struct Counter where
   incr  :: Action
   read  :: Request Int
   reset :: Action

struct ObservableCounter < Observable Int, Counter

obsCounter :: Class ObservableCounter
obsCounter = class

   val := 0
   Publisher {..} = new publisher

   set v  = do 
      val := v
      setChanged
      notify v

   incr  = action set (val + 1)

   reset = action set 0

   read = request
      result val

   result ObservableCounter {..}
