module Collect(collect) where


import Common
import Kindle



collect m                               = return (colModule m)



data Collection                         = CClos [Type] Type
                                        | CCons Id [Type]
                                        deriving (Eq,Show)


colModule (Module m ds fs vs)           = Module m (ds++ds') fs vs
  where col                             = concatMap colDecl ds ++ concatMap colFun fs ++ concatMap colVal vs
        ds'                             = concatMap (col2class ds) (nub col)


col2class ds (CClos ts t)               = [(c, Class [("enter", TFun (TId c : ts) t)])]
  where c                               = type2id (TClos ts t)
col2class ds (CCons c ts)               = []      -- Temporary!!!!


colDecl (c,Class te)                    = concatMap colSig te
colDecl (c,Union cs)                    = []



colSig (x,t)                            = colType t


colType (TFun ts t)                     = concatMap colType ts ++ colType t
colType (TClos ts t)                    = CClos ts t : concatMap colType ts ++ colType t
colType t                               = []


colFun (x,Fun te t b)                   = concatMap colSig te ++ colType t ++ colBody b


colVal (x,Val t e)                      = colType t ++ colExp e


colBody (BDef fs b)                     = concatMap colFun fs ++ colBody b
colBody (BLet v b)                      = colVal v ++ colBody b
colBody (BCase t e alts d)              = colType t ++ colExp e ++ concatMap colAlt alts ++ colBody d
colBody (BCase' e alts d)               = colExp e ++ concatMap colAlt' alts ++ colBody d
colBody (BSeq b1 b2)                    = colBody b1 ++ colBody b2
colBody (BBreak)                        = []
colBody (BGen v b)                      = colVal v ++ colBody b
colBody (BAss x l e b)                  = colExp e ++ colBody b
colBody (BExp e)                        = colExp e


colAlt (Alt c x b)                      = colBody b

colAlt' (Alt' l b)                      = colBody b


colExp (EVar x)                         = []
colExp (ELit l)                         = []
colExp (ESel e l)                       = colExp e
colExp (ECall e es)                     = colExp e ++ concatMap colExp es
colExp (ENew c vs)                      = CCons c (rng (valEnv vs)) : concatMap colVal vs

colExp (EEnter t e es)                  = colType t ++ colExp e ++ concatMap colExp es
colExp (EClose te t e vs)               = colType (TClos (rng te) t) ++ colExp e ++ concatMap colVal vs
colExp (ECast t t' e)                   = colType t ++ colType t' ++ colExp e
