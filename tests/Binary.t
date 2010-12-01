module Binary where

data Bin = B0 | B1

typeclass Binary a where
  put :: a -> [Bin]
  get :: [Bin] -> (a,[Bin])

instance binaryUnit :: Binary () where
  put _  = []
  get bs = ((),bs)

instance binaryPair :: Binary (a,b) \\ Binary a, Binary b where
  put (x,y) = put x ++ put y
  get bs    = ((x,y),bs'')
              where (x,bs')  = get bs
                    (y,bs'') = get bs'

instance binaryEither :: Binary (Either a b) \\ Binary a, Binary b where
  put (Left x)  = B0 : put x
  put (Right y) = B1 : put y
  get (B0 : bs) = (Left x,bs')  where (x,bs') = get bs
  get (B1 : bs) = (Right y,bs') where (y,bs') = get bs


deriving instance binaryMaybe :: Binary (Maybe a) \\ Binary a



data A = C1 | C2 

deriving instance binaryA :: Binary A
