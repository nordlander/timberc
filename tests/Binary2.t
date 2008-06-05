module Binary2 where

data Bin = B0 | B1

implicit struct Binary a where
  get :: [Bin] -> (a,[Bin])


implicit binaryUnit :: Binary ()
binaryUnit = struct
  get bs = ((),bs)


implicit binaryEither :: Binary (Either a b) \\ Binary a, Binary b
binaryEither = struct
  get (B0 : bs) = (Left x,bs')  where (x,bs') = get bs
  get (B1 : bs) = (Right y,bs') where (y,bs') = get bs


implicit binaryBin :: Binary Bin
binaryBin = struct
              get bs = case get bs of (x,bs') -> (toBin x,bs')

toBin :: Either () () -> Bin
toBin (Left a)  = B0
toBin (Right a) = B1
