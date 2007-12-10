module UnionFind where

record EqRel =
  equal   :: Int -> Int -> Request Bool
  mkEqual :: Int -> Int -> Action

unionFind :: Int -> Template EqRel
unionFind n = template 
   a := primConstArray n (-1)

   find k = do 
     if a!k < 0 then
        return k
     else
        s <- find(a!k)
        a!k := s
        return s

   equal i j = request
     si <- find i
     sj <- find j
     return (si==sj)

   mkEqual i j = action
     si <- find i 
     sj <- find j
     if si/=sj then
        asi = a!si
        asj = a!sj
        if asj < asi then
           a!si := sj
        else 
           a!sj := si
           if asi==asj then
              a!si := asi-1

   return EqRel{..}
