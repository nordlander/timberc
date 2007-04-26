module Common (module Common, module Name) where

import PP
import qualified List
import qualified Maybe
import Char
import Name

import Debug.Trace



fromJust                        = Maybe.fromJust
listToMaybe                     = Maybe.listToMaybe

fst3 (a,b,c)                    = a
snd3 (a,b,c)                    = b
thd3 (a,b,c)                    = c

dom                             = map fst

rng                             = map snd

partition p xs                  = List.partition p xs

nub xs                          = List.nub xs

xs \\ ys                        = filter (`notElem` ys) xs

xs `intersect` ys               = xs `List.intersect` ys

xs `union` ys                   = xs `List.union` ys

disjoint xs ys                  = xs `intersect` ys == []

overlaps xs ys                  = not (disjoint xs ys)

intersperse x xs                = List.intersperse x xs

duplicates xs                   = nub (foldl (flip List.delete) xs (nub xs))

rotate n xs                     = let (xs1,xs2) = splitAt n xs in xs2++xs1

separate []                     = ([],[])
separate (Left x : xs)          = let (ls,rs) = separate xs in (x:ls,rs)
separate (Right x : xs)         = let (ls,rs) = separate xs in (ls,x:rs)

showids vs                      = concat (intersperse "," (map show vs))

fmapM f g xs                    = do ys <- mapM g xs
                                     return (f ys)

mapFst f xs                     = [ (f a, b) | (a,b) <- xs ]

mapSnd f xs                     = [ (a, f b) | (a,b) <- xs ]


-- String manipulation -----------------------------------------------------

rmSuffix :: String -> String -> String
rmSuffix suf = reverse . rmPrefix (reverse suf) . reverse

rmPrefix :: String -> String -> String
rmPrefix pre string 
  | pre `List.isPrefixOf` string = drop (length pre) string
  | otherwise                    = error $ "rmPrefix: " ++ string 
                               ++ " is not a prefix of " ++ show pre

dropPrefix [] s                 = (True, s)
dropPrefix (x:xs) (y:ys)
  | x == y                      = dropPrefix xs ys
dropPrefix xs ys                = (False, ys)


dropDigits xs                   = drop 0 xs
  where drop n (x:xs)
          | isDigit x           = drop (10*n + ord x - ord '0') xs
        drop n xs               = (n, xs)




-- Literals ----------------------------------------------------------------

data Lit                        = LInt    Integer
                                | LRat    Rational
                                | LChr    Char
                                | LStr    String
                                deriving  (Eq,Show)

instance Pr Lit where
    pr (LInt i)                 = integer i
    pr (LRat r)                 = rational r
    pr (LChr c)                 = litChar c
    pr (LStr s)                 = litString s


-- Underlying monad ----------------------------------------------------------------------

newtype M s a                   = M ((Int,[s]) -> Either String ((Int,[s]), a))


instance Functor (M s) where
    fmap f x                    = x >>= (return . f)

instance Monad (M s) where
    M m >>= f                   = M $ \k -> 
                                    case m k of 
                                        Right (k',a) -> m' k' where M m' = f a
                                        Left s -> Left s
    return a                    = M $ \k -> Right (k,a)
    fail s                      = M $ \k -> Left s

handle (M m) f                  = M $ \k ->
                                    case m k of
                                        Right r -> Right r
                                        Left s  -> m' k where M m' = f s

expose (M m)                    = M $ \k ->
                                    case m k of
                                        Right (k',a) -> Right (k',Right a)
                                        Left s       -> Right (k, Left s)

unexpose (Right a)              = return a
unexpose (Left b)               = fail b

runM (M m)                      = case m (1,[]) of 
                                    Right (_,x) -> x
                                    Left s      -> error s

newNum                          = M $ \(n,s) -> Right ((n+1,s), n)

addToStore x                    = M $ \(n,s) -> Right ((n,x:s), ())

currentStore                    = M $ \(n,s) -> Right ((n,s), s)

localStore (M m)                = M $ \(n0,s0) ->
                                    case m (n0,[]) of
                                      Right ((n,s), x) -> Right ((n,s0), x)
                                      Left s           -> Left s

newNameMod m s                  = do n <- newNum
                                     return (Name s n m ann)
  where ann                     = if s `elem` explicitSyms then noAnnot { explicit = True } else noAnnot

newName s                       = newNameMod Nothing s

newNames s n                    = mapM (const (newName s)) [1..n]


{-
renaming vs                     = mapM f vs
  where f v | tag v == 0        = do n <- newNum
                                     return (v, v { tag = n })
            | otherwise         = return (v, v)
-}

renaming vs                     = renamingMod Nothing vs            

renamingMod m vs                = mapM f vs
  where f v | tag v == 0        = do n <- newNum
                                     return (v, v { tag = n, fromMod = m})
            | otherwise         = return (v, v)
 
assert e msg
  | e                           = return ()
  | otherwise                   = fail msg




-- Poor man's exception datatype ------------------------------------------------------

encodeCircular ids              = circularMsg ++ concat (intersperse " " (map packName ids))

decodeCircular str
  | ok                          = Just (map unpackName (words rest))
  | otherwise                   = Nothing
  where (ok, rest)              = dropPrefix circularMsg str

circularMsg                     = "Circular subtyping: "



-- Internal identifier conventions -----------------------------------------------------

witnessSym                      = "w"
assumptionSym                   = "v"
tempSym                         = "x"
functionSym                     = "f"
dummySym                        = "d"
paramSym                        = "a"
tyvarSym                        = "t"
coercionSym                     = "c"
labelSym                        = "l"
constrSym                       = "C"
typeSym                         = "T"
stateSym                        = "S"
skolemSym                       = "sk"
selfSym                         = "self"
thisSym                         = "this"
codeSym                         = "code"
tagSym                          = "tag"
instanceSym                     = "inst"
closureSym                      = "CLOS"

isCoercion n                    = isGenerated n && str n == coercionSym
isTemp n                        = isGenerated n && str n == tempSym
isClosure n                     = isGenerated n && str n == closureSym
isDummy n                       = isGenerated n && str n == dummySym

explicitSyms                    = [coercionSym, assumptionSym, witnessSym]


-- Textual variable supply -------------------------------------------------------------

stdSupply                               = alternate (abcdeijklxyz 0 []) (mfghn 0 [])

abcdeijklxyz                            = gensupply "abcdeijklxyz"
mfghn                                   = gensupply "mfghn"

abcSupply                               = gensupply "abcdefghijklmnopqrstuvwxyz" 0 []


gensupply                               :: String -> Int -> [String] -> [String]
gensupply chars depth avoid             = filter (`notElem` avoid) vars
  where vars                            = if depth==0 then vars0 else map (++ show depth) vars0
        vars0                           = map (:"") chars ++ map (:"'") chars ++ concat (map g [1..])
        g n                             = map (replicate n) chars


alternate vs1 vs2 []                    = []
alternate vs1 vs2 (True  : xs)          = head vs1 : alternate (tail vs1) vs2 xs
alternate vs1 vs2 (False : xs)          = head vs2 : alternate vs1 (tail vs2) xs


-- Tracing -----------------------------------------------------------------------------

tr m                            = trace (m++"\n") (return ())

tr' m e                         = trace ("\n"++m++"\n") e


-- Free variables -----------------------------------------------------------------------

class Ids a where
    idents :: a -> [Name]

instance Ids a => Ids [a] where
    idents xs                   = concatMap idents xs

instance Ids a => Ids (Name,a) where
    idents (v,a)                = idents a

tycons x                        = filter isCon (idents x)

tyvars x                        = filter isVar (idents x)

evars x                         = filter isVar (idents x)

svars x                         = filter isState (idents x)


vclose vss vs
  | null vss2                   = nub vs
  | otherwise                   = vclose vss1 (concat vss2 ++ vs)
  where (vss1,vss2)             = partition (null . intersect vs) vss



-- Bound variables -----------------------------------------------------------------------

class BVars a where
    bvars  :: a -> [Name]
    
    bvars _                     = []


-- Mappings -----------------------------------------------------------------------------

infixr 4 @@

type Map a b = [(a,b)]

lookup' assoc x                 = case lookup x assoc of
                                    Just e -> e
                                    Nothing -> error ("Internal: lookup': " ++ show x ++ " not in " ++ show (assoc))

inv assoc                       = map (\(a,b) -> (b,a)) assoc

delete k []                     = []
delete k (x:xs)
  | fst x == k                  = xs
  | otherwise                   = x : delete k xs


delete' ks xs                   = foldr delete xs ks


insert k x []                   = [(k,x)]
insert k x ((k',x'):assoc)
  | k == k'                     = (k,x) : assoc
  | otherwise                   = (k',x') : insert k x assoc


update k f []                   = error "Internal: Common.update"
update k f ((k',x):assoc)
  | k == k'                     = (k, f x) : assoc
  | otherwise                   = (k',x) : update k f assoc


search p []                     = Nothing
search p (a:assoc)
  | p a                         = Just a
  | otherwise                   = search p assoc


insertBefore kx ks []           = [kx]
insertBefore kx ks ((k,x'):assoc)
  | k `elem` ks                 = kx:(k,x'):assoc
  | otherwise                   = (k,x') : insertBefore kx ks assoc




(@@)                            :: Subst b a b => Map a b -> Map a b -> Map a b
s1 @@ s2                        = [(u,subst s1 t) | (u,t) <- s2] ++ s1


merge                           :: (Eq a, Eq b) => Map a b -> Map a b -> Maybe (Map a b)
merge [] s'                     = Just s'
merge ((v,t):s) s'              = case lookup v s' of
                                    Nothing         -> merge s ((v,t):s')
                                    Just t' | t==t' -> merge s s'
                                    _               -> Nothing

nullSubst                       = []

a  +-> b                        = [(a,b)]

restrict s vs                   = filter ((`elem` vs) . fst) s

prune s vs                      = filter ((`notElem` vs) . fst) s

class Subst a i e where
    subst :: Map i e -> a -> a


substVars s xs                  = map (substVar s) xs

substVar s x                    = case lookup x s of
                                    Just x' -> x'
                                    Nothing -> x


instance Subst a i e => Subst [a] i e where
    subst [] xs                 = xs
    subst s xs                  = map (subst s) xs

instance Subst a i e => Subst (Name,a) i e where
    subst s (v,a)               = (v, subst s a)

instance Subst a i e => Subst (Maybe a) i e where
    subst s Nothing             = Nothing
    subst s (Just a)            = Just (subst s a)


newEnv x ts                     = do vs <- mapM (const (newName x)) ts
                                     return (vs `zip` ts)



-- Kinds ---------------------------------------------------------------------------------

data Kind                       = Star
                                | KFun    Kind Kind
                                | KWild
                                | KVar    Int
                                deriving (Eq,Show)

newtype TVar                    = TV (Int,Kind)

type KEnv                       = Map Name Kind

instance Eq TVar where
    TV (n,k) == TV (n',k')      = n == n'

instance Show TVar where
    show (TV (n,k))             = show n

instance Pr TVar where
    pr (TV (n,k))               = pr n


newTV k                         = do n <- newNum
                                     return (TV (n,k))

newKVar                         = do n <- newNum
                                     return (KVar n)

tvKind (TV (n,k))               = k

kvars Star                      = []
kvars (KVar n)                  = [n]
kvars (KFun k1 k2)              = kvars k1 ++ kvars k2

kArgs (KFun k k')               = k : kArgs k'
kArgs k                         = []

kFlat k                         = (kArgs k, kRes k)

kRes (KFun k k')                = kRes k'
kRes k                          = k


instance Subst Kind Int Kind where
    subst s Star                = Star
    subst s k@(KVar n)          = case lookup n s of
                                    Just k' -> k'
                                    Nothing -> k
    subst s (KFun k1 k2)        = KFun (subst s k1) (subst s k2)

instance Subst (Kind,Kind) Int Kind where
    subst s (a,b)               = (subst s a, subst s b)

instance Pr (Name,Kind) where
    pr (n,k)                    = prId n <+> text "::" <+> pr k

instance Pr Kind where
    prn 0 (KFun k1 k2)          = prn 1 k1 <+> text "->" <+> prn 0 k2
    prn 0 k                     = prn 1 k
    prn 1 Star                  = text "*"
    prn 1 (KVar n)              = text ('_':show n)
    prn 1 KWild                 = text "_"
    prn 1 k                     = parens (prn 0 k)


class TVars a where
    tvars                       :: a -> [TVar]

instance TVars a => TVars [a] where
    tvars xs                    = concatMap tvars xs

instance TVars a => TVars (Name,a) where
    tvars (v,a)                 = tvars a





