module Match where

import Common
import Syntax
import Monad
import qualified List

pmc :: Exp -> [Alt Exp] -> M s Exp
pmc e alts                      = do e' <- match0 e alts
                                     return (eMatch e')

pmc' :: [Name] -> [([Pat],Rhs Exp)] -> M s Exp
pmc' ws eqs                     = do e <- match ws eqs
                                     return (eMatch e)


-- The primitive pmc constants -----------------------------------------------------------

eFatbar (EVar (Prim Fail _)) e  = e
eFatbar e (EVar (Prim Fail _))  = e
eFatbar e e'                    = foldl EAp (EVar (prim Fatbar)) [e,e']
eFail                           = EVar (prim Fail)
eCommit e                       = EAp (EVar (prim Commit)) e
eMatch e                        = case e of
                                    EAp (EVar (Prim Commit _)) e' -> e'
                                    ELet bs (EAp (EVar (Prim Commit _)) e') -> ELet bs e'
                                    _  -> EAp (EVar (prim Match)) e


fat []                          = return eFail
fat [m]                         = m
fat (m:ms)                      = do e1 <- m
                                     e2 <- fat ms
                                     return (eFatbar e1 e2)


-- Pattern-matching compiler proper -----------------------------------------------------

match0 (EVar w) alts            = match [w] [ ([p], rh) | Alt p rh <- alts ]
match0 e alts                   = do w <- newNamePos tempSym e
                                     e' <- match0 (EVar w) alts
                                     return (ELet [BEqn (LFun w []) (RExp e)] e')


match ws eqs                    = fat (match1 ws eqs)

match1 ws []                    = []
match1 [] (([],rhs):eqs)        = matchRhs rhs : match1 [] eqs
match1 (w:ws) eqs
  | all isVarEq eqs             = match1 ws (map f eqs)
  where f (EVar v : ps, rh)     = (ps, subst (v +-> EVar w) rh)
match1 ws (eq:eqs)
  | isSigVarEq eq               = matchVar ws eq : match1 ws eqs
  | isLitEq eq                  = matchLits ws [eq] eqs
  | isERecEq eq                 = matchRecs ws [eq] eqs
  | otherwise                   = matchCons ws [prepConEq eq] eqs


isLitEq (p:ps,rh)               = isELit p

isVarEq (p:ps,rh)               = isEVar p

isSigVarEq (p:ps,rh)            = isESigVar p

isERecEq (p:ps,rh)              = isERec p

isConEq (p:ps,rh)               = isEConApp p



matchVar (w:ws) (EVar v:ps, rh) = match ws [(ps, subst (v +-> EVar w) rh)]
matchVar (w:ws) (ESig (EVar v) t : ps, rh)
                                = match ws [(ps, RWhere rh bs)]
  where bs                      = [BSig [v] t, BEqn (LFun v []) (RExp (EVar w))]


matchLits ws eqs (eq:eqs')
  | isLitEq eq                  = matchLits ws (eq:eqs) eqs'
matchLits ws eqs eqs'           = matchLit ws (reverse eqs) : match1 ws eqs'


matchLit (w:ws) eqs             = do alts <- mapM matchAlt lits
                                     return (ECase (EVar w) alts)
  where lits                    = nub [ l | (ELit l : ps, rhs) <- eqs ]
        matchAlt l              = do e' <- match ws eqs'
                                     return (Alt (ELit l) (RExp e'))
          where eqs'            = [ (ps,rhs) | (ELit l' : ps, rhs) <- eqs, l'==l ]

matchRecs ws eqs (eq:eqs')
  | isERecEq eq                 = matchRecs ws (eq:eqs) eqs'
matchRecs ws eqs eqs'           = matchRec ws (reverse eqs) : match1 ws eqs'

matchRec (w:ws) eqs             = do vs <- newNamesPos tempSym fs 
                                     e <- match (vs ++ ws) (map matchAlt eqs)
                                     return (foldr ELet e (zipWith mkEqn vs fs))
  where ERec _ fs               = head (fst (head eqs))
        mkEqn v (Field l _)     = [BEqn (LFun v []) (RExp (ESelect (EVar w) l))]
        matchAlt (ERec _ fs:ps,rh)
                                = (map patOf fs++ps,rh)
        patOf (Field _ p)       = p
 
prepConEq (p:ps,rhs)            = (c, ps', ps, rhs)
  where (ECon c, ps')           = eFlat p


matchCons ws ceqs (eq:eqs')
  | isConEq eq                  = matchCons ws (prepConEq eq : ceqs) eqs'
matchCons ws ceqs eqs'          = matchCon ws (reverse ceqs) : match1 ws eqs'


matchCon (w:ws) ceqs            = do alts <- mapM matchAlt cs
                                     return (ECase (EVar w) alts)
  where cs                      = nub [ c | (c,_,_,_) <- ceqs ]
        matchAlt c              = do vs <- newNamesPos tempSym (maxPat [] 0 eqs_c)
                                     e  <- match (vs++ws) (map (mkeq vs) eqs_c)
                                     return (Alt (ECon c) (RExp (eLam (map EVar vs) e)))
          where eqs_c           = [ (ps', ps, rhs) | (c',ps',ps,rhs) <- ceqs, c==c' ]
                maxPat ps _ []  = ps
                maxPat ps n ((ps',_,_) : ceqs)
                   |length ps' > n = maxPat ps' (length ps') ceqs
                   |otherwise      = maxPat ps n ceqs
--                arity_c         = maximum [ length ps' | (ps', ps, rhs) <- eqs_c ]
        mkeq vs (ps',ps,rhs)    = (ps'++vs' ++ ps, rAp rhs vs')
          where vs'             = map EVar (drop (length ps') vs)


matchRhs (RExp e)               = return (eCommit e)
matchRhs (RWhere rhs bs)        = do e <- matchRhs rhs
                                     return (ELet bs e)
matchRhs (RGrd gs)              = fat [ matchQuals qs e | GExp qs e <- gs ]


matchQuals [] e                 = return (eCommit e)
matchQuals (QGen p e' : qs) e   = match0 e' [Alt p (RGrd [GExp qs e])]
matchQuals (QLet bs : qs) e     = do e' <- matchQuals qs e
                                     return (ELet bs e')
matchQuals (QExp e' : qs) e     = matchQuals (QGen true e' : qs) e



-- Dependency analysis ------------------------------------

{-

lpatToCase e [BEqn (LPat p) rh] = pmc (rh2exp rh) [Alt p (RExp e)]
lpatToCase e bs 
  | null pbs                    = return (ELet bs e)
  | otherwise                   = errorTree "Recursive pattern binding not allowed" (head pbs)
  where pbs                     = filter isLPatEqn bs

lpatToCase2 ss [BEqn (LPat p) rh]= do v <- newNamePos selfSym p
                                      e <- pmc (rh2exp rh) [Alt p (RExp (EDo (Just v) Nothing ss))]
                                      return [SExp e]
lpatToCase2 ss bs  
  | null pbs                    = return (map SBind bs ++ ss)
  | otherwise                   = errorTree "Recursive pattern binding not allowed" (head pbs)
  where pbs                     = filter isLPatEqn bs


mkLet bs e                       = do let bss = groupBindsS bs
                                      foldM lpatToCase e (reverse bss)  -- reverse since foldM is monadic foldl
 
cLet bs ss                       = do let bss = groupBindsS bs
                                      foldM lpatToCase2 ss (reverse bss)                                
        
-}
{-

case e0 of
  C p1 p2 -> e1
  C p3    -> e2
  D p4    -> e3
  D       -> e4

case e0 of
  C p1 p2 -> e1
  C p3 x1 -> e2 x1
  D p4    -> e3
  D x2    -> e4 x2


case e0 of
  C -> \v1 v2 -> case v1 of
                   p1 -> case v2 of
                           p2 -> e1
                   p3 -> case v2 of
                           x1 -> e2 x1
  D -> \v3    -> case v3 of
                   p4 -> e3
                   x2 -> e4 x2

-}
