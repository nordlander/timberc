module StructPat where

struct Point where
  x,y :: Int

struct Point3D where
  base :: Point
  height :: Int 

f (Point {x=0..}) = 0
f (Point {y=0..}) = 1
f p = 2

basex (Point3D {base=Point{..}..}) = x

h p = case p of
       Point3D{..} -> height

