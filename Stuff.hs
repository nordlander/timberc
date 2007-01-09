module Stuff where

import List(sort)
import Common
import Syntax


dostuff (Module c ds)           = Module c (stuff (mkEnv ds) ds)

{-
    This module implements Record Stuffing; i.e. replacing ".." in record patterns and expressions 
    by sequences of bindings of the form "x=x" for missing record fields x.  Also adds type tag to 
    anonymous record expressions/patterns.
-}

-- The selector environment --------------------------------------------------------------------------------------

data Env                        = Env { sels :: Map Name [Name], self :: Maybe Name }

mkEnv ds                        = Env { sels = map transClose recEnv, self = Nothing }
  where recEnv                  = [ (c,(map type2head ts, sort (concat (map sels ss)))) | DRec _ c _ ts ss <- ds ]
        sels (Sig vs _)         = vs
        transClose (c,(cs,ss))  = (c, ss ++ concat (map (selectors recEnv [c]) cs))
        selectors re cs0 c
          | c `elem` cs0        = error ("Circular record dependecies: " ++ showids (c:cs0))
          | otherwise           = case lookup c re of
                                    Just (cs,ss) -> ss ++ concat (map (selectors re (c:cs0)) cs)
                                    Nothing      -> error ("Unknown record constructor: " ++ show c)


selsFromType env c              = case lookup c (sels env) of
                                    Just ss -> ss
                                    Nothing -> error ("Unknown record constructor: " ++ show c)


typeFromSels env ss             = f (sels env) ss
  where f [] ss                 = error ("No record type with selectors " ++ showids (map chopSel ss))
        f  ((c,ss'):se) ss
          | ss == ss'           = c
          | otherwise           = f se ss


haveSelf env                    = self env /= Nothing


-- Stuffing -------------------------------------------------------------------------------------------

class Stuff a where
    stuff :: Env -> a -> a

instance Stuff a => Stuff [a] where
    stuff env                     = map (stuff env)

instance Stuff Decl where
    stuff env (DBind b)           = DBind (stuff env b)
    stuff env d                   = d

instance Stuff Bind where
    stuff env (BEqn lh rh)        = BEqn (stuff env lh) (stuff env rh)
    stuff env b                   = b

instance Stuff Lhs where
    stuff env (LFun v ps)         = LFun v (stuff env ps)
    stuff env (LPat p)            = LPat (stuff env p)

instance Stuff (Rhs Exp) where
    stuff env (RExp e)            = RExp (stuff env e)
    stuff env (RGrd gs)           = RGrd (stuff env gs)
    stuff env (RWhere e bs)       = RWhere (stuff env e) (stuff env bs)

instance Stuff (GExp Exp) where
    stuff env (GExp qs e)         = GExp (stuff env qs) (stuff env e)

instance Stuff Qual where
    stuff env (QExp e)            = QExp (stuff env e)
    stuff env (QGen p e)          = QGen (stuff env p) (stuff env e)
    stuff env (QLet bs)           = QLet (stuff env bs)

instance Stuff Exp where
    stuff env (ERec Nothing fs)      = ERec (Just (c,True)) (stuff env fs)
      where c                        = typeFromSels env (sort (bvars fs))
    stuff env (ERec (Just (c,all)) fs)
      | all && not (null miss)       = error ("Missing selectors in record: " ++ showids (map chopSel miss))
      | otherwise                    = ERec (Just (c,True)) (fs' ++ stuff env fs)
      where miss                     = selsFromType env c \\ bvars fs
            fs'                      = map (\s -> Field s (EVar (chopSel s))) miss
 
    stuff env (EAp e1 e2)            = EAp (stuff env e1) (stuff env e2)
    stuff env (ETup es)              = ETup (stuff env es)
    stuff env (EList es)             = EList (stuff env es)
    stuff env (ESig e t)             = ESig (stuff env e) t
    stuff env (ELam ps e)            = ELam (stuff env ps) (stuff env e)
    stuff env (ECase e as)           = ECase (stuff env e) (stuff env as)
    stuff env (EIf e1 e2 e3)         = EIf (stuff env e1) (stuff env e2) (stuff env e3)
    stuff env (ENeg e)               = ENeg (stuff env e)
    stuff env (ESeq e1 e2 e3)        = ESeq (stuff env e1) (stuff env e2) (stuff env e3)
    stuff env (EComp e qs)           = EComp (stuff env e) (stuff env qs)
    stuff env (ESectR e op)          = ESectR (stuff env e) op
    stuff env (ESectL op e)          = ESectL op (stuff env e)
    stuff env (ESelect e s)          = ESelect (stuff env e) s

    stuff env (ETempl Nothing t ss)  = stuff env (ETempl (Just (name0 "self")) t ss)
    stuff env (EDo Nothing t ss)
      | haveSelf env                 = stuff env (EDo (self env) t ss)
      | otherwise                    = stuff env (EDo (Just (name0 "self")) t ss)
    stuff env (EAct Nothing ss)
      | haveSelf env                 = stuff env (EAct (self env) ss)
      | otherwise                    = error "Illegal action"
    stuff env (EReq Nothing ss)
      | haveSelf env                 = stuff env (EReq (self env) ss)
      | otherwise                    = error "Illegal request"

    stuff env (ETempl v t ss)        = ETempl v t (stuffT (env{self=v}) ss)
    stuff env (EDo v t ss)           = EDo v t (stuffS (env{self=v}) ss)
    stuff env (EAct v ss)            = EAct v [SExp (EDo v Nothing (stuffS env ss))]
    stuff env (EReq v ss)            = EReq v [SExp (EDo v Nothing (stuffS env ss))]

    stuff env (EAfter e1 e2)         = EAfter (stuff env e1) (stuff env e2)
    stuff env (EBefore e1 e2)        = EBefore (stuff env e1) (stuff env e2) 

    stuff env e                      = e

instance Stuff (Alt Exp) where
    stuff env (Alt p rh)          = Alt (stuff env p) (stuff env rh)

instance Stuff Field where
    stuff env (Field l e)         = Field l (stuff env e)

instance Stuff (Maybe Exp) where
    stuff env Nothing             = Nothing
    stuff env (Just e)            = Just (stuff env e)


stuffS env []                     = []
stuffS env [SExp e]               = [SExp (stuff env e)]
stuffS env (SExp e : ss)          = SGen EWild (stuff env e) : stuffS env ss
stuffS env [SRet e]               = [SRet (stuff env e)]
stuffS env (SRet e : ss)          = error "Illegal return"
stuffS env (SGen p e : ss)        = SGen (stuff env p) (stuff env e) : stuffS env ss
stuffS env (SBind b : ss)         = SBind (stuff env b) : stuffS env ss
stuffS env (SAss p e : ss)        = SAss (stuff env p) (stuff env e) : stuffS env ss
stuffS env (SCase e as : ss)      = stuffS env (SExp (ECase e (map doAlt as)) : ss)
  where doAlt (Alt p r)           = Alt p (doRhs r)
        doRhs (RExp ss)           = RExp (EDo (self env) Nothing ss)
        doRhs (RGrd gs)           = RGrd (map doGrd gs)
        doRhs (RWhere r bs)       = RWhere (doRhs r) bs
        doGrd (GExp qs ss)        = GExp qs (EDo (self env) Nothing ss)
stuffS env (SIf e ss' : ss)       = doIf (EIf e (EDo (self env) Nothing ss')) ss
  where doIf f (SElsif e ss':ss)  = doIf (f . EIf e (EDo (self env) Nothing ss')) ss
        doIf f (SElse ss':ss)     = stuffS env (SExp (f (EDo (self env) Nothing ss')) : ss)
        doIf f ss                 = stuffS env (SExp (f (EDo (self env) Nothing [])) : ss)
stuffS env (SElsif e ss : _)      = error "Illegal elsif"
stuffS env (SElse ss : _)         = error "Illegal else"
stuffS env (SForall q ss' : ss)   = error "forall stmt not yet implemented"
stuffS env (SWhile e ss' : ss)    = error "while stmt not yet implemented"


stuffT env [SExp e]               = [SExp (stuff env e)]
stuffT env [SRet e]               = [SRet (stuff env e)]
stuffT env (SGen p e : ss)        = SGen (stuff env p) (stuff env e) : stuffT env ss
stuffT env (SBind b : ss)         = SBind (stuff env b) : stuffT env ss
stuffT env (SAss p e : ss)        = SAss (stuff env p) (stuff env e) : stuffT env ss
stuffT env _                      = error "Illegal statement in template"
