module Depend where

import Common
import Core

type Graph a                    = Map a [a]

graph nbors ps                  = map f ps
  where f (i,d)                 = (i,[v | v <- nbors d, v/=i, v `elem` domain])
        domain                  = dom ps
 
scc                             :: Eq a => Graph a -> [[a]]
scc g                           = dfs g' (concat (dfs g (dom g)))
  where g'                      = [(i,[x | (x,ys) <- g, i `elem` ys]) | (i,_) <- g]
 
topSort g
  | all (null . tail) ns        = concat ns
  | otherwise                   = error "Cyclic graph"
  where ns                      = scc g

topSort1 nbors ms               = order ms (topSort (graph nbors ms)) 

dfs g is                        = snd (dfs' ([],[]) is)
   where dfs' p []              = p
         dfs' p@(vs,ns) (x:xs)
           | x `elem` vs        = dfs' p xs
           | otherwise          = dfs' (vs',(x:concat ns'):ns) xs
           where (vs',ns')      = dfs' (x:vs,[]) (nbors x)
         nbors i                = fromJust (lookup i g)

order ms ns                     = ns `zip` map (fromJust . flip lookup ms) ns
  
group ms nss                    = map (order ms) nss

groupBinds (Binds _ te es)      = zipWith f tes ess
  where gs                      = scc (graph evars es)
        tes                     = group te gs
        ess                     = group es gs
        f te es@[(x,e)]         = Binds (x `elem` evars e) te es
        f te es                 = Binds True te es

groupTypes (Types ke ts)        = zipWith Types kes tss
  where gs                      = scc (graph tycons ts)
        kes                     = group ke gs
        tss                     = group ts gs
