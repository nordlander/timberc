module Derive where

import Common
import Core
import Env
import PP


derive ts ds []                       = return ([],[])
derive ts ds (d@(Default _ _ _) : xs)
                                      = do (is,xs) <- derive ts ds xs
                                           return (is,d:xs)
derive ts ds (Derive n sc : xs)       = do let (s,d) = analyze ds sc
                                           b <- mkFunPair d
                                           i <- mkInstance n ts s sc b
                                           -- tr ("### Default instance \n"++render (prInsts i'))
                                           (is,xs) <- derive ts ds xs
                                           return (i:is,xs)

analyze ds sc                         = case tFlat (scheme2Type sc) of
                                           (TId c,[t']) -> findDataType c t'
                                           _ -> errorTree "Not an instance of a one-parameter type constructor" sc 
  where findDataType c t'             = case tFlat t' of
                                          (TId d,_) -> case lookup c (tdefsOf ds) of
                                             Nothing -> errorTree ("Unknown implicit struct type "++show c) sc
                                             Just c'@(DRec True [a] _ _) -> case lookup d (tdefsOf ds) of
                                                                              Nothing -> errorTree ("Unknown data type "++show d) sc
                                                                              Just d'@(DData _ _ _) -> ((c,c'),(d,d')) 
                                                                              Just _ -> errorTree (show d++" is not a data type") sc
                                             Just (DRec True _ _ _) -> errorTree "Implicit struct types must have exactly one type parameter" sc
                                             Just _ -> errorTree (show c++" is not an implicit struct type") sc         
                                                   
mkInstance n ts (nm,DRec True [a] ps ss) sc (ns,bss)
                                      = do  fs <- mapM (mkField a ts ns) ss
                                            return (Binds False [(n,sc)] [(n,foldr ELet (ERec nm fs) bss)])

mkField a ts [from,to] (nm,sc)        = do let nm' = lookup' [(name0 (str n),n {annot = (annot n) {explicit=False}}) | n <- ts ] (name0 (str nm))
                                           e <- expand a from to (EVar nm') sc
                                           return (nm,e)

expand a from to e sc                 = exp0 to (e,scheme2Type sc)
  where exp0 f (e,TFun ts t)          = do ns <- newNames paramSym (length ts)
                                           let xs = map EVar ns
                                           es <- mapM (exp0 from) (zip xs ts)
                                           e' <- exp0 to (EAp e es,t)
                                           eLam' ns e'
        exp0 f (e,TId n)
          | n==a                      = return (EAp (EVar f) [e])
        exp0 _ (e,t)                  = expTup (e,tFlat t)
                                             
        expTup (e,(TId (Tuple n _),ts))
                                      = do let n = length ts
                                           ns <- newNames paramSym n
                                           let xs = map EVar ns
                                           es <- mapM (exp0 to) (zip xs ts)
                                           lam <- eLam' ns (etup n es)
                                           return (ECase e [(PCon (tuple n), lam)])
        expTup (e,_)                  = return e

eLam' xs e                            = do ts <- mapM (\_ -> newTVar Star) xs
                                           return (ELam (zipWith (\x t -> (x,scheme t)) xs ts) e)
                                           
mkFunPair (nm,DData vs ss cs)         = do [from,to] <- mapM newName ["from"++str nm,"to"++str nm]
                                           frhs <- fromRHS
                                           trhs <- toRHS
                                           return ( [from,to], [Binds False [(from,fromType)] [(from,frhs)], Binds False [(to,toType)]  [(to,trhs)]])

  where ot                            = foldr1 tEither (map mkTuple cs)
        dt                            = foldl TAp (TId nm) (map TId vs)
        mkTuple (_,Constr [] _ _)     = tUnit
        mkTuple (_,Constr ss _ _)     = foldr1 (ttup2) (map scheme2Type ss)
        ttup2 t1 t2                   = ttup 2 [t1,t2]
        etup2 e1 e2                   = etup 2 [e1,e2]
        fromType                      = Scheme (F [scheme dt] (R ot)) [] (map (\n -> (n,Star)) vs)
        toType                        = Scheme (F [scheme ot] (R dt)) [] (map (\n -> (n,Star)) vs)

        prefixLR fs e                 = foldr (\f e -> EAp (ECon f) [e]) e (map prim fs)
        mkAlt fs (nm,Constr [] _ _)   = return (PCon nm,prefixLR fs (ECon (prim UNITTERM)))
        mkAlt fs (nm,Constr ss _ _)   = do ns <- newNames tempSym (length ss)
                                           lam <- eLam' ns  (prefixLR fs (foldr1 (etup2) (map EVar ns)))
                                           return (PCon nm,lam)
        alts fs [c]                   = do a <- mkAlt fs c
                                           return [a]
        alts fs (c:cs)                = do a <- mkAlt (fs++[LEFT]) c
                                           as <- alts (fs++[RIGHT]) cs
                                           return (a:as)
        fromRHS                       = do x  <- newName paramSym
                                           as <- alts [] cs
                                           return (ELam [(x,scheme dt)] (ECase (EVar x) as))

        mkCase t@(TAp (TAp (TId (Prim EITHER _)) a) b) (c:cs) (x:xs)
                                      = do [y,z] <- newNames paramSym 2
                                           l <- mkCase a [c] (y:xs)
                                           r <- mkCase b cs (z:xs)
                                           lam1 <- eLam' [y] l
                                           lam2 <- eLam' [z] r
                                           return (ECase (EVar x) [(PCon (prim LEFT),lam1),(PCon (prim RIGHT),lam2)])
        mkCase t@(TAp (TAp (TId (Tuple 2 _)) a) b) cs (x:xs)
                                      = do [y,z] <- newNames paramSym 2
                                           r <- mkCase b cs (z:y:xs)
                                           lam <- eLam' [y,z] r
                                           return (ECase (EVar x) [(PCon (tuple 2),lam)])
        mkCase (TId (Prim UNITTYPE _)) (c:_) xs             
                                      = return (ECon c)
        mkCase t (c:_) xs             = return (EAp (ECon c) (map EVar (reverse xs)))
        toRHS                         = do x <- newName paramSym
                                           r <- mkCase ot (map fst cs) [x] 
                                           eLam' [x] r


ttup n ts                             = foldl TAp (TId (tuple n)) ts
etup n es                             = EAp (ECon (tuple n)) es


scheme2Type (Scheme r _ _)            = rho2Type r
rho2Type (R t)                        = t
rho2Type (F ss r)                     = TFun (map scheme2Type ss) (rho2Type r) 

