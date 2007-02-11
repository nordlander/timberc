module Lambdalift(lambdalift) where

import Common
import Kindle


lambdalift m                            = return llModule m)


data Env                                = Env { locals     :: ATEnv,
                                                expansions :: Map Name [Name] }


addLocals env te                        = env { locals = te ++ locals env }

addExpansions env fve                   = env { expansions = fve ++ expansions env }


llModule (Module m ds bs)               = Module m ds (concat fss ++ fs') vs
  where (fss,fs')                       = unzip (map (llFDef env0) fs)
        env0                            = Env { locals = [], expansions = [] }


llVDefs env vs                          = map (llVDef env) vs


llFDef env (x, Fun te t b)              = (fs, (x, Fun te t b'))
  where (fs, b')                        = llBody (addLocals env te)  b


llVDef env (x, v)                       = (x, llVal env v)


llVal env (Val t e)                     = Val t (llExp env e)



{-

   \v ->
   letrec f x = ... v ... in
   letrec g y = ... f e ... in
   ... g n ...


==>


   letrec f v x = ... v ... in
   letrec g y   = ... f v e ... in
   \v ->
   g v n


-}


llBody env (BDef fs b)                  = {-tr' ("Lifting  " ++ showids xs ++ "\n" ++
                                               " free0:  " ++ showids free0 ++ "\n" ++
                                               " free1:  " ++ showids free1 ++ "\n" ++
                                               " locals: " ++ showids (locals env) ++ "\n" ++
                                               " fte:    " ++ show fte ++ "\n" ++
                                               " fs':    " ++ show fs' ++ "\n" ++
                                               " lifted: " ++ show (map lift fs1) ++ "\n") -}
                                              (concat fss ++ map lift fs' ++ fs'', b')
  where xs                              = dom fs
        te                              = funEnv fs
        free0                           = evars fs \\ xs
        free1                           = free0 ++ concat [ ys | (y,ys) <- expansions env, y `elem` free0 ]
        fte                             = locals env `restrict` free1
        env'                            = addExpansions env (xs `zip` repeat (dom fte))
        (fss,fs')                       = unzip (map (llFDef env') fs)
        lift (x, Fun te t b)            = (x, Fun (fte++te) t b)
        (fs'', b')                      = llBody env' b
llBody env (BLet v b)                   = (fs, BLet (llVDef env v) b')
  where (fs,b')                         = llBody (addLocals env (valEnv [v])) b
llBody env (BCase t e alts d)           = (concat fss ++ fs, BCase t (llExp env e) alts' d')
  where (fss,alts')                     = unzip (map (llAlt env) alts)
        (fs,d')                         = llBody env d
llBody env (BCase' e alts d)            = (concat fss ++ fs, BCase' (llExp env e) alts' d')
  where (fss,alts')                     = unzip (map (llAlt' env) alts)
        (fs,d')                         = llBody env d
llBody env (BSeq b1 b2)                 = (fs1++fs2, BSeq b1' b2')
  where (fs1,b1')                       = llBody env b1
        (fs2,b2')                       = llBody env b2
llBody env (BBreak)                     = ([], BBreak)
llBody env (BGen v b)                   = (fs, BGen (llVDef env v) b')
  where (fs,b')                         = llBody (addLocals env (valEnv [v])) b

llBody env (BAss x l e b)               = (fs, BAss x l (llExp env e) b')
  where (fs,b')                         = llBody env b
llBody env (BExp e)                     = ([], BExp (llExp env e))


llAlt env (Alt c x b)                   = (fs, Alt c x b')
  where (fs, b')                        = llBody (addLocals env [(x,TId c)]) b


llAlt' env (Alt' l b)                   = (fs, Alt' l b')
  where (fs, b')                        = llBody env b


llExp env (ELit l)                      = ELit l
llExp env (EVar x)
  | x `elem` dom (expansions env)       = error "Internal: llExp"
  | otherwise                           = EVar x
llExp env (ESel e l)                    = ESel (llExp env e) l
llExp env (ENew c vs)                   = ENew c (llVDefs env vs)
llExp env (ECall e@(EVar x) es)         = case lookup x (expansions env) of
                                            Just xs -> ECall e (map EVar xs ++ map (llExp env) es)
                                            Nothing -> ECall e (map (llExp env) es)
llExp env (ECall e es)                  = ECall (llExp env e) (map (llExp env) es)

llExp env (EClose te t e vs)            = EClose te t e' (llVDefs env vs ++ map mkVDef te')
  where e'                              = llExp (addLocals env te) e
        te'                             = locals env `restrict` (evars e' \\ dom te)
        mkVDef (x,t)                    = (x, Val t (EVar x))
llExp env (EEnter t e es)               = EEnter t (llExp env e) (map (llExp env) es)
llExp env (ECast t t' e)                = ECast t t' (llExp env e)