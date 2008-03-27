module PreludeExtra where

-- prelude additions

toList :: Array a -> [a]
toList a = [a!i | i <- [0..size a]]

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just a) = False

implicit ordChar :: Ord Char
ordChar = struct
            a <  b = ord a <  ord b
            a <= b = ord a <= ord b
            a >  b = ord a >  ord b
            a >= b = ord a >= ord b
            (==) = eqChar.(==)
            (/=) = eqChar.(/=)

implicit struct Functor m where
  ($^)   :: (a -> b) -> m a -> m b

implicit struct Applicative m < Functor m where
  ($*)   :: m (a -> b) -> m a -> m b
  return :: a -> m a

implicit struct Monad m < Applicative m where
  (>>=)  :: m a -> (a -> m b) -> m b


(>>) :: m a -> m b -> m b \\ Monad m
ma >> mb = ma >>= \_ -> mb

implicit struct Monoid a where
  mempty :: a
  mappend :: a -> a -> a


implicit functorMaybe :: Functor Maybe = struct
  f $^ Nothing = Nothing
  f $^ Just a  = Just (f a)

implicit applicativeMaybe :: Applicative Maybe = Applicative {..}
  where Functor {..} = functorMaybe
        Just f $* Just a = Just (f a)
        _      $* _      = Nothing

implicit monadMaybe :: Monad Maybe = Monad {..}
  where Applicative {..} = applicativeMaybe
        Just a  >>= f = f a
        Nothing >>= _ = Nothing

implicit monoidMaybe :: Monoid (Maybe a) = struct
  mempty = Nothing
  mappend (Just a) _ = Just a
  mappend Nothing  a = a

implicit functorArray :: Functor Array = struct
  f $^ a = array [f (a!i) | i <- [0..size a-1]]

($) :: (a -> b) -> a -> b
f $ a = f a

join :: m (m a) -> m a \\ Monad m
join m = m >>= id

const :: a -> b -> a
const a _ = a

id :: a -> a
id a = a

implicit showTuple :: Show (a,b) \\ Show a, Show b = struct
  show (a,b) = "("++show a++","++show b++")"

implicit showUnit :: Show () = struct
  show () = "()"
