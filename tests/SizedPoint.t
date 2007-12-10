module SizedPoint where

  record SizedPoint < Point =
    size :: Int

  p = Point {x=1, y=2}

  q = SizedPoint {size=9, x=8, y=7}

