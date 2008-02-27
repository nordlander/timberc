module Binary where

data Bin = B0 | B1

implicit struct Binary a where
  put :: a -> [Bin]
  get :: [Bin] -> (a,[Bin])

implicit binaryUnit :: Binary ()
binaryUnit = struct
  put _ = []
  get bs = ((),bs)

implicit binaryPair :: Binary (a,b) \\ Binary a, Binary b
binaryPair = struct
  put (x,y) = put x ++ put y
  get bs    = ((x,y),bs'')
              where (x,bs')  = get bs
                    (y,bs'') = get bs'

implicit binaryEither :: Binary (Either a b) \\ Binary a, Binary b
binaryEither = struct
  put (Left x)  = B0 : put x
  put (Right y) = B1 : put y
  get (B0 : bs) = (Left x,bs')  where (x,bs') = get bs
  get (B1 : bs) = (Right y,bs') where (y,bs') = get bs

implicit binaryBin :: Binary Bin
binaryBin = struct
              put x  = put (fromBin x)
              get bs = case get bs of (x,bs') -> (toBin x,bs')

   where      fromBin B0 = Left ()
              fromBin B1 = Right ()
              toBin (Left a)  = B0
              toBin (Right a) = B1
