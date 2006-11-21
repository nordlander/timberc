module Eliminate(eliminate) where

import Common
import Kindle


eliminate m                     = elModule m


elModule (Module m cs fs vs)    = do cs0 <- elCDefs env cs
                                     (cs1,fs') <- elFDefs env fs
                                     (cs2,vs2,vs') <- elVDefs env vs
                                     let (cs3,fs1) = separate (cs1++cs2)
                                     return (Module m (cs0++cs3) (fs1++fs') (vs2++vs'))
  where env                     = []


elType t@(TClos _ _)            = TId (type2id t)
elType (TFun ts t)              = TFun (map elType ts) (elType t)
elType t                        = t

elTEnv te                       = mapSnd elType te


elCDefs env ce                  = mapM (elCDef env) ce

elCDef env (c, Class te)        = return (c,Class te')
  where te'                     = elTEnv te
elCDef env d                    = return d


elFDef env (x, Fun te t b)      = do (cs, b') <- elBody env b
                                     return (cs, (x,Fun (elTEnv te) (elType t) b'))


elFDefs env fs                  = do (css,fs') <- fmap unzip (mapM (elFDef env) fs)
                                     return (concat css, fs')


elBody env (BDef fs b)          = do (cs1,fs') <- elFDefs env fs
                                     (cs2,b') <- elBody env b
                                     return (cs1++cs2, BDef fs' b')
elBody env (BLet v b)           = do (cs1,vs1,v') <- elVDef env v
                                     (cs2,b') <- elBody env b
                                     return (cs1++cs2, eLet vs1 (BLet v' b'))
elBody env (BCase t e alts d)   = do (cs1,vs1,e') <- elExp env e
                                     (css,alts') <- fmap unzip (mapM (elAlt env) alts)
                                     (cs2,d') <- elBody env d
                                     return (cs1++concat css++cs2, eLet vs1 (BCase t e' alts' d'))
elBody env (BCase' e alts d)    = do (cs1,vs1,e') <- elExp env e
                                     (css,alts') <- fmap unzip (mapM (elAlt' env) alts)
                                     (cs2,d') <- elBody env d
                                     return (cs1++concat css++cs2, eLet vs1 (BCase' e' alts' d'))
elBody env (BSeq b1 b2)         = do (cs1,b1') <- elBody env b1
                                     (cs2,b2') <- elBody env b2
                                     return (cs1++cs2, BSeq b1' b2')
elBody env (BBreak)             = return ([], BBreak)
elBody env (BGen v b)           = do (cs1,vs1,v') <- elVDef env v
                                     (cs2,b') <- elBody env b
                                     return (cs1++cs2, eLet vs1 (BGen v' b'))
elBody env (BAss x l e b)       = do (cs1,vs1,e') <- elExp env e
                                     (cs2,b') <- elBody env b
                                     return (cs1++cs2, eLet vs1 (BAss x l e' b'))
elBody env (BExp e)             = do (cs, vs, e') <- elExp env e
                                     return (cs, eLet vs (BExp e'))


elAlt env (Alt c x b)           = do (cs,b') <- elBody env b
                                     return (cs, Alt c x b')

elAlt' env (Alt' l b)           = do (cs,b') <- elBody env b
                                     return (cs, Alt' l b')


elVDef env (x, Val t e)         = do (cs,vs,e') <- elExp env e
                                     return (cs, vs, (x,Val (elType t) e'))


elVDefs env vs                  = do (css,vss,vs') <- fmap unzip3 (mapM (elVDef env) vs)
                                     return (concat css, concat vss, vs')


elExp env (ELit l)              = return ([], [], ELit l)
elExp env (EVar x)              = return ([], [], EVar x)
elExp env (ESel e l)            = do (cs, vs, e') <- elExp env e
                                     return (cs, vs, ESel e' l)
elExp env (ECall e es)          = do (cs1, vs1, e') <- elExp env e
                                     (cs2, vs2, es') <- elExpList env es
                                     return (cs1++cs2, vs1++vs2, ECall e' es')
elExp env (ENew c vs)           = do (cs1,vs1,vs') <- elVDefs env vs
                                     return (cs1, vs1, ENew c vs')
        
elExp env (ECast t t' e)        = do (cs,vs,e') <- elExp env e
                                     return (cs, vs, ECast (elType t) (elType t') e')
elExp env (EEnter t e es)       = do (cs1,vs1,e') <- elVarExp env t e
                                     (cs2,vs2,es') <- elExpList env es
                                     return (cs1++cs2, vs1++vs2, ECall (ESel e' "enter") (e':es'))
elExp env (EClose te t e [])    = do f <- newVar functionSym
                                     x <- newVar tempSym
                                     (cs1,vs1,e') <- elExp env e
                                     let c   = type2id (TClos (rng te) t)
                                         te' = (x, TId c) : elTEnv te
                                         t'  = elType t
                                         fn  = Fun te' t' (body [] vs1 (BExp e'))
                                         r   = ENew c [("enter", Val (TFun (rng te') t') (EVar f))]
                                     return (Right (f,fn) : cs1, [], r)
elExp env (EClose te t e vs)    = do f <- newVar functionSym
                                     x <- newVar tempSym
                                     c <- newVar constrSym
                                     (cs1,vs1,e') <- elExp env e
                                     (cs2,vs2,vs') <- elVDefs env vs
                                     let te' = (x, TId c) : elTEnv te
                                         ts  = rng te'
                                         t'  = elType t
                                         cl  = Class (("enter", TFun ts t') : valEnv vs)
                                         fvs = dom vs
                                         s   = fvs `zip` map (ESel (EVar x)) fvs
                                         fn  = Fun te' t' (body [] (subst s vs1) (BExp (subst s e')))
                                         r   = ENew c (("enter", Val (TFun ts t') (EVar f)) : vs')
                                         e'' = ECast (TId c) (elType (TClos (rng te) t')) r
                                     return (Left (c,cl) : Right (f,fn) : cs1 ++ cs2, vs2, e'')


elExpList env es                = do (css,vss,es') <- fmap unzip3 (mapM (elExp env) es)
                                     return (concat css, concat vss, es')


elVarExp env t (EVar x)         = return ([], [], EVar x)
elVarExp env t e                = do (cs,vs,e') <- elExp env e
                                     x <- newVar tempSym
                                     return (cs, vs ++ [(x,Val (elType t) e')], EVar x)

