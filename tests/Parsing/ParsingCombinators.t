module ParsingCombinators where

import POSIX
import PreludeExtra
import State

type Token = Char
type Bounds = (Token,Token)

struct TArray t e where
  ar :: Array e
  t0 :: t

implicit functorTArray :: Functor (TArray t) = struct
  f $^ a = TArray {..} where t0 = a.t0 --TArray{..} = a
                             ar = f $^ a.ar

tarray :: t -> [e] -> TArray t e
tarray t es = struct
  ar = array es 
  t0 = t

tsize :: TArray t e -> Int
tsize t = size t.ar

bounds :: TArray t e -> (t,t) \\ Enum t
bounds a = (a.t0,toEnum (fromEnum a.t0 + tsize a - 1))

mindex :: Array e -> Int -> Maybe e
mindex a o | o >= 0 && o < size a = Just (a!o)
           | otherwise            = Nothing

tindex :: TArray t e -> t -> Maybe e \\ Enum t
tindex a o = mindex a.ar (fromEnum o - fromEnum a.t0)

struct P a where
  arr :: TArray Token (Maybe (State [Token] (Maybe a)))
  emptyParse :: Maybe a


parse' :: P a -> State [Token] (Maybe a)
parse' p =  
  get >>= \ts ->
  case ts of 
    t:ts' -> case tindex p.arr t of
               Just (Just f) -> 
                 set ts' >>
                 f >>= \ma ->
                 case ma of
                   Just a -> return (Just a)
                   Nothing -> set ts >>
                              return p.emptyParse
               _ -> return p.emptyParse
    [] -> return p.emptyParse

parseP :: P a -> [Token] -> ([Token],Maybe a)
parseP = runState @ parse'

token :: Token -> P ()
token t = struct
  arr = tarray t [Just (return (Just ()))]
  emptyParse = Nothing

implicit functorP :: Functor P =
  struct
    g $^ a = struct
      arr = (((g $^) $^) $^) $^ a.arr
      emptyParse = g $^ a.emptyParse

implicit applicativeP :: Applicative P = struct
  ($^) = functorP.($^)
  f $* a = struct
    arr = combineArraySeq f a
    emptyParse = f.emptyParse $* a.emptyParse
  return a = struct
    arr = tarray ' ' []
    emptyParse = Just a

combineArraySeq :: P (a -> b) -> P a -> 
                   TArray Token (Maybe (State [Token] (Maybe b)))
combineArraySeq fp ap = 
  case fp.emptyParse of
    Nothing -> ((\ft -> 
                  ft            >>= \mf ->
                  case mf of 
                    Nothing -> return Nothing
                    Just f -> 
                      parse' ap >>= \ma ->
                      return (f $^ ma))$^) $^ fp.arr
    Just f -> combineTArray cf fp.arr ap.arr where
      cf Nothing    (Just at) = Just ((f$^) $^ at)
      cf Nothing    Nothing   = Nothing
      cf (Just ft') Nothing   = (\a -> (($a) $^) $^ ft') $^ ap.emptyParse
      cf (Just ft') (Just at) = Just ((ft' >>= \mf ->
                                      case mf of 
                                        Nothing -> return Nothing
                                        Just f -> (f$^) $^ at)
                                    `altState`
                                      ((f$^) $^ at))

combineTArray :: (Maybe a -> Maybe b -> Maybe c) -> 
                 TArray t (Maybe a) -> 
                 TArray t (Maybe b) -> 
                 TArray t (Maybe c)                 \\ Enum t, Ord t
combineTArray f aa ba = 
  tarray c1 [ f (join (tindex aa t))
                (join (tindex ba t))
            | t <- [c1..c2] ]
  where (c1,c2) = (min a1 b1, max a2 b2)
        (a1,a2) = bounds aa
        (b1,b2) = bounds ba


altState :: State s (Maybe a) -> State s (Maybe a) -> State s (Maybe a)
altState m1 m2 = 
  get >>= \s ->
  m1 >>= \ma ->
  case ma of
    Just a -> return (Just a)
    Nothing -> set s >> m2
