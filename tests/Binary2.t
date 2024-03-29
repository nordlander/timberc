module Binary2 where

data Bin = B0 | B1

typeclass Binary a where
  get :: [Bin] -> (a,[Bin])


instance binaryUnit :: Binary () where
  get bs = ((),bs)


instance binaryEither :: Binary (Either a b) \\ Binary a, Binary b where
  get (B0 : bs) = (Left x,bs')  where (x,bs') = get bs
  get (B1 : bs) = (Right y,bs') where (y,bs') = get bs


instance binaryBin :: Binary Bin where
  get bs = case get bs of (x,bs') -> (toBin x,bs')

toBin :: Either () () -> Bin
toBin (Left a)  = B0
toBin (Right a) = B1

relyOnMonomorphismRestriction (B0 : bs) = class
    (x,bs') = get bs
    result (Left x, bs')
relyOnMonomorphismRestriction (B1 : bs) = class
    (x,bs') = get bs
    result (Right x, bs')
