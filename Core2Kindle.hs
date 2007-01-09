module Core2Kindle(core2kindle) where
{- -}
import Common
import Core
import Name
import Depend
import qualified Kindle
import qualified Char

{-

Core
====

p  ::=  { T:k = D } ; { x:s = f }                                                                  program
D  ::=  { l : s } \\ { a:k }  |  { K ss } \\ { a:k }                                               declaration
f  ::=  \xs:ss -> f  |  template@x:s c  |  action@x c  |  request@x c  |  do@x:s c                 closure expression
c  ::=  return e  |  { x:s = f }; c  |  x:s = e; c  |  x:s <- e; c  |  x := e; c  |  e             command
e  ::=  x  |  n  |  e es  |  f  |  letrec { x:s = f } in e  |  let x:s = e in e  |                 expression
        case e of { K -> e }  |  K es  |  T { l = e }  |  e.l
s  ::=  r \\ as:ks                                                                                 type scheme
r  ::=  ss -> r  |  t                                                                              rho type
t  ::=  a ts  |  T ts  |  ts -> t                                                                  type
k  ::=  *  |  k -> k                                                                               kind

T                                                                                                  type constructor
a                                                                                                  type variable
K                                                                                                  term constructor
x                                                                                                  term variable
l                                                                                                  field selector

Kindle
======

p  ::=  { T = D } ; { m:(ts)t = f } ; { x:t = e }                                                  program
D  ::=  class { x:t, m:(ts)t }  |  enum { K }                                                      declaration
f  ::=  (xs) c                                                                                     closure expression
c  ::=  { m:(ts)t = f }; c  |  x:t = e; c  |  e.x := e; c  |  return x  |  switch x { K -> c }     command
e  ::=  x  |  n  |  m(es)  |  (t)e  |  new T { x = e, m = f } |  e.x  |  e.m(es)  |  K  |  this    expression
t  ::=  T  |  ?                                                                                    type

T                                                                                                  type name
K                                                                                                  constant name
m                                                                                                  method/function name
x                                                                                                  field/variable name


Translating functions    FUN[ f ]  =  f' : (ts)t
=====================

TYPE[ ss ] = ts        WITH xs:ts BODY[ e ]  =  c : t
-----------------------------------------------------
FUN[ \xs:ss -> e ]  =  (xs) c : (ts) t


TYPE[ s ] = T    WITH x:t WITHSELF x CMD[ c ]  =  c' : t
--------------------------------------------------------
FUN[ do@x:s c ]  =  (x) c' : (T)t


TYPE[ s ] = T    WITH x:T WITHSELF x CMD[ c ]  =  c' : t
------------------------------------------------------------------------------------------
FUN[ template@x:s c ]  =  (_) x:T = new T {}; c'  :  (?)t


VARENV(x) = T    TYPENV(T) = class{ xs:ts }    WITHSELF x CMD[ c ]  =  c' : t
---------------------------------------------------------------------------------
FUN[ request@x ]  =  (_) LOCK(x); c'; UNLOCK(x)  :  (?)t


VARENV(x) = T    TYPENV(T) = class{ xs:ts }    WITHSELF x CMD[ c ]  =  c' : t     f = (x) LOCK(x); c'; UNLOCK(x)
--------------------------------------------------------------------------------------------------------------------
FUN[ action@x ]  =  (a,b) y:Msg = new Msg { aft=a, bef=b, to=x, code=f }; return POST(y)  :  (Time,Time)Msg



Translating commands    CMD[ c ]  =  c' : t
====================

EXP[ e ]  =  e' : t WHERE E  
------------------------------------
CMD[ return e ]  =  E; return e' : t


FTYPE[ ss ] = (tss)ts    WITH xs:(tss)ts FUN-MATCH((tss)ts)[ fs ]  =  fs'     WITH xs:(tss)ts CMD[ c ]  =  c' : t
-------------------------------------------------------------------------------------------------------------
CMD[ xs:ss=fs; c ]  =  xs:(tss)ts=fs'; c'  :  t


TYPE[ s ] = t    EXP-MATCH(t)[ e ]  =  e' WHERE E    WITH x:t CMD[ c ]  =  c' : t'
----------------------------------------------------------------------------------
CMD[ x:s=e; c ]  =  x:t=e'; c'  :  t'


TYPE[ s ] = t     STATENV() = y:ts    EXP-MATCH(t)[ e y ]  =  e'    WITH x:t CMD[ c ]  =  c' : t'
-------------------------------------------------------------------------------------------------
CMD[ x:s <- e; c ]  =  x:t=e'; c'  :  t'


STATEENV() = y:ts   
-----------------------------------------------------------
CMD[ x := e; c ]  =  


Translating commands   M|X |- c --> c' : t
====================

E|M|X |- e --> e' : t      x new
-----------------------------------------
M|X |- return e --> E; x:t=e'; return x : t


|- ss --> ts    M|X,xs:ts |- fs --> fs' : ts   M|X,xs:ts |- c --> c' : t
--------------------------------------------------------------------
M|X |- xs:ss=fs; c --> xs:ts=fs; c' : t


|- s --> t    E|M|X |- e --> e' : t    M|X,x:t |- c --> c' : t'
-----------------------------------------------------------
M|X |- x:s=e; c --> E,x:t=e'; c' : t'


|- s --> t    E|M|X |- e 0 0 --> e' : t    M|X,x:t=e' |- c --> c' : t'
------------------------------------------------------------------
M|X |- x:s <- primAction2O e; c --> E,x:t=e'; c' : t'


|- s --> t    E|M|X |- e y --> e' : t    M|X,x:t |- c --> c' : t'
-------------------------------------------------------------        -- y ("self")
M|X |- x:s <- e; c --> x:t=e'; c' : t'


S(x) = t     E|M|X |- e --> e' : t    M|X |- c --> c' : t'
------------------------------------------------------               -- S(x)
M|X |- x := e; c --> E; y.x := e'; c' : t'


E|M|X |- e y --> e' : t   x new
-------------------------------                                      -- y ("self")
M|X |- e --> x:t=e'; return x : t



Translating bodies   M|X |- e --> c : t
==================


|- ss --> ts    M|X,xs:ts |- fs --> fs' : ts    M|X,xs:ts |- e --> c : t
--------------------------------------------------------------------
M|X |- letrec xs:ss=fs in e --> xs:ts=ds; c : t


|- s --> t      E|M|X |- e --> e1 : t    M|X,x:t |- e' --> c : t'
-------------------------------------------------------------
M|X |- let x:s=e in e' --> E; x:t=e1; c : t'


E|M|X |- e --> e' : t     |- ss_i --> ts_i     M|X,xs_i:ts_i |- e_i --> c_i : t'       x new
----------------------------------------------------------------------------------------------------------
M|X |- case e of {K_i -> \xs_i:ss_i->e_i} --> E; x:t=e'; switch x of {K_i -> xs_i:ts_i=(K_i)x.args;c_i} : t'


E|M|X |- e --> e' : t       x new
----------------------------------
M|X |- e --> E; x:t=e'; return x : t



Translating expressions   E|M|X |- e --> e' : t
=======================


M|X(x) = t
------------------
.|M|X |- x --> x : t


--------------------
.|M|X |- n --> n : Int


E|M|X |- e -> e' : t'   t/=t'
---------------------------
E|M|X |- e -> (t)e' : t


E|M|X |- e --> e' : Msg              x,y new
------------------------------------------------------
E;x:(?)Msg=(y)e'|M|X |- primAction2O e --> e'(0,0) : Msg


E|M|X |- e --> e' : (ts)t    E'|M|X |- es --> es' : ts
--------------------------------------------------
E,E'|M|X |- x es --> e'(es') : t


E|M|X |- e --> e' : (ts)t    E'|M|X |- es --> es' : ts
--------------------------------------------------
E,E'|M|X |- e es --> e'(es') : t


M|X(K) = class { tag : T; args : ts }   E|M|X |- es --> es' : ts
------------------------------------------------------------
E|M|X |- K es --> new T { tag = K, args = es' } : K


M|X(T) = class { xs : ts }   E|M|X |- es --> es' : ts
-------------------------------------------------
E|M|X |- T { xs = es } --> new T { xs = es' } : T


M|X(l) = (T)t   E|M|X |- e --> e' : T
---------------------------------
E|M|X |- e.l --> e'.l : t


|- ss --> ts    M|X,xs:ts |- fs --> fs' : ts    E|M|X,xs:ts |- e --> e' : t
------------------------------------------------------------------------
xs:ts=fs';E|M|X |- letrec xs:ss=fs in e --> e' : t


|- s --> t    E|M|X |- e --> e1 : t     E'|M|X,x:t |- e' --> e2 : t'
----------------------------------------------------------------
E;x:t=e1;E'|M|X |- let x:s=e in e' --> e2 : t'


M|X |- case e of {K_i->e_i} --> c : t     x new
-----------------------------------------------
x:()t=()c|M|X |- case e of {K_i->e_i} --> x() : t


M|X |- f --> f' : t     x new
---------------------------
x:t=f|M|X |- f --> x : t



Translating type declarations
=============================


|- s_i --> t_i
-------------------------------------------------------------
|- T:k = { l_i : s_i } \\ as:ks --> T = class { l_i : t_i }


T |- ss_i --> D_i
------------------------------------------------------------------------------------------------
|- T:k = { K_i ss_i } \\ as:ks --> Ttag = enum { K_i } ; T = class { tag : Ttag }; { K_i = D_i }


|- s_i --> t_i
---------------------------------------------------
T |- s_1 s_2 ... --> class { tag : Ttag; arg_1 : t_1; arg_2 : t_2 ... }



Translating types
=================


|- r --> r
-------------------
|- r \\ as:ks --> r


|- ss --> ts    |- r --> t
--------------------------
|- ss -> r --> (ts)t


|- t --> t'
------------------
|- Cmd t --> (?)t'


|- t --> t'
------------------
|- Template t --> (?)t'


|- t --> t'
------------------
|- Request t --> (?)t'


------------------
|- Action --> (Time,Time)Msg


|- s --> s'    |- t --> t'
--------------------------
|- O s t -> (s')t'


|- t --> t'
---------------
|- Ref t --> t'


|- T ts --> T


|- a ts --> ?


|- ts --> ts'    |- t --> t'
----------------------------
|- ts -> t --> CLOS_ts'_t'



-}

core2kindle                         :: Module -> M Kindle.Module
core2kindle m                       = cModule m



data Env                            = Env { decls  :: Kindle.Decls,
                                            tenv   :: Kindle.TEnv,
                                            dict   :: Map Name Name,
                                            self   :: Maybe Name }

env0                                = Env [] [] [] Nothing

addDecls ds env                     = env { decls = ds ++ decls env }

addTEnv te env                      = env { tenv = te ++ tenv env }

addDict d env                       = env { dict = d ++ dict env }

setSelf x env                       = env { self = Just x }




cModule (Module m ds _ bs)          = do env <- conDict ds
                                         ds <- cDecls env ds
                                         (ds',bs) <- cBindsList (addDecls ds env) (groupBinds bs)
                                         return (Kindle.Module m (ds++ds') bs)

conDict (Types ke ds)               = do d <- renaming cons
                                         return (env0 { dict = d })
  where cons                        = concat [ c : dom cs | (c,DData _ _ cs) <- ds ]


cDecls env (Types ke ds)            = do dss <- mapM cDecl ds
                                         return (concat dss)
  where cDecl (c,DRec _ vs [] ss)   = do (ds,te) <- cTEnv ss
                                         return ((c, Kindle.Struct te) : ds)
        cDecl (c,DType vs t)        = error ("Internal: cannot yet compile type synonyms ")
        cDecl (c,DData vs [] cs)    = do dss <- mapM (cCon t) cs
                                         tag <- newName "tag"
                                         return ((c', Kindle.Enum cs') : (c, Kindle.Struct [(tag,t)]) : concat dss)
          where c'                  = substVar (dict env) c
                t                   = Kindle.ValT (Kindle.TId c')
                cs'                 = substVars (dict env) (dom cs)

        cCon t (c,Constr ts [] _)   = do tag <- newName "tag"
                                         te <- newEnv "arg" ts
                                         (ds,te) <- cTEnv te
                                         return (ds ++ [(c, Kindle.Struct ((tag,t):te))])


cTEnv ss                            = do (dss,te) <- fmap unzip (mapM conv ss)
                                         return (concat dss, te)
  where conv (x,s)                  = do (ds, ts, t) <- cType (deQualify s)
                                         case ts of
                                           [] -> return (ds, (x, Kindle.ValT t))
                                           _  -> return (ds, (x, Kindle.FunT ts t))


cATEnv te                             = do (dss,te) <- fmap unzip (mapM conv te)
                                           return (concat dss, te)
  where conv (x,s)                    = do (ds,t) <- cAType (deQualify s)
                                           return (ds, (x,t))


-- Reduce schemes and rho types to ordinary (function) types by simply removing the quantifiers, leaving the tyvars free.
deQualify (Scheme rh [] ke)         = deQualify' rh
-- (The next alternative should ideally have been handled in a previous pass, 
-- to match the corresponding catenation of witness parameters more closely.)
deQualify (Scheme rh ps ke)         = deQualify' (tFunCat ps rh)

deQualify' (R t)                    = t
deQualify' (F ts rh)                = TFun (map deQualify ts) (deQualify' rh)


cType (TFun ts t)                   = do (dss,ts) <- fmap unzip (mapM cAType ts)
                                         (ds,ts,t) <- cType' ts t
                                         return (ds++concat dss, ts, t)
cType t                             = cType' [] t



cType' ts (TId (Prim Action _))            = return ([], ts++[t,t], Kindle.TId (prim Msg))
  where t                                  = Kindle.TId (prim Time)
cType' ts (TAp (TId (Prim Request _)) t)   = do (ds,t) <- cAType t
                                                return (ds, ts++[Kindle.TWild], t)
cType' ts (TAp (TId (Prim Template _)) t)  = do (ds,t) <- cAType t
                                                return (ds, ts++[Kindle.TWild], t)
cType' ts (TAp (TId (Prim Cmd _)) t)       = do (ds, t) <- cAType t
                                                return (ds, ts++[Kindle.TWild], t)
cType' ts (TAp (TAp (TId (Prim O _)) s) t) = do (ds1, s) <- cAType s
                                                (ds2, t) <- cAType t
                                                return (ds1++ds2, ts++[s], t)
cType' ts (TAp t t')                       = cType' ts t
cType' ts (TId n)                          = return ([], ts, Kindle.TId n)
cType' ts (TVar _)                         = return ([], ts, Kindle.TWild)
cType' ts (TWild)                          = return ([], ts, Kindle.TWild)


cAType t                              = do (ds, ts, t) <- cType t
                                           if null ts then
                                               return (ds, t)
                                            else do 
                                               c <- newName "CLOS"
                                               f <- newName ".code"
                                               return ((c, Kindle.Struct [(f,Kindle.FunT ts t)]) : ds, Kindle.TId c)


cBindsList env []                     = return ([], [])
cBindsList env (bs:bss)               = do (ds1,bs1) <- cBinds env bs
                                           (ds2,bs2) <- cBindsList (addTEnv (Kindle.mkTEnv bs1) env) bss
                                           return (ds1++ds2, bs1++bs2)

cBinds env (Binds r te eqs)           = do (ds,te) <- cTEnv te
                                           assert (not r || all Kindle.isFunT te) "Illegal value recursion"
                                           (dss,bss) <- fmap unzip (mapM (cBind (addTEnv te env)) eqs)
                                           return (concat (ds:dss), concat bss)
  where cBind env (x,e)               = case lookup' (tenv env) x of
                                          Kindle.ValT t -> do
                                              (ds,bs,e) <- matchVal env t e
                                              return (ds, bs ++ [(x, Kindle.Val t e)])
                                          Kindle.FunT ts t -> do
                                              (ds,bs,xs,c) <- matchFun env ts t e
                                              return (ds, bs ++ [(x, Kindle.Fun t (xs `zip` ts) c)])


matchVal env t e                      = do (ds,bs,e,t') <- cExp env e
                                           if t /= t' then
                                               return (ds, bs, Kindle.ECast t e)
                                            else
                                               return (ds, bs, e)

matchFun env ts t e                   = do (ds,te,c,t') <- cFun env e
                                           undefined
                                         


cFun env (ELam te e)                  = do (ds,te) <- cATEnv te
                                           (ds',te',c,t) <- cFun' env e
                                           return (ds++ds', te++te', c, t)
cFun env e                            = cFun' env e

{-
                  explicit var n with type t   explicit var n no type       no var no type, but outer scope     nothing

 do               s = n, state = fields of t   s = n, state = outer         s = outer, state = outer            s = "self", state = empty
 template         s = n, state = fields of t   s = n, state = inferred      s = "self", state = inferred        s = "self", state = inferred

 request/action   illegal                      s = n, state = fields of n   s = outer, state = outer,           illegal

   
-}


cFun' env (EReq x e)                  = do (ds,c,t) <- cCmd (pushSelf x env) c
                                           y <- newName "_"
                                           return (ds, [(y, Kindle.TWild)], protect x c, t)
                                          
cFun' env (EAct x e)                  = do (ds,c,t) <- cCmd env (CExp (EReq x c))
                                           y  <- newName "_"
                                           a  <- newName "a"
                                           b  <- newName "b"
                                           m  <- newName "m"
                                           mt <- newName "Msg"
                                           f  <- newName ".code"
                                           bl <- newName ".baseline"
                                           dl <- newName ".deadline"
                                           let bs  = [(m, Kindle.Val tMsg (Kindle.ECast (Kindle.TId mt) (Kindle.ENew mt bs'))),
                                                      (y, Kindle.Val tUnit (Kindle.ECall (prim ASYNC) (map Kindle.EVar [m,a,b])))]
                                               c'  = Kindle.CBind bs (Kindle.CRet (Kindle.EVar m))
                                               bs' = [(f, Kindle.Fun t [] c)]
                                               te  = [(f,Kindle.FunT [] t), (bl,Kindle.ValT tTime), (dl,Kindle.ValT tTime)]
                                               ds  = [(mt, Kindle.Struct te)]
                                           return (ds, [(a,tTime),(b,tTime)], c', tMsg)
  where tTime                         = Kindle.TId (prim Time)
        tMsg                          = Kindle.TId (prim Msg)
        tUnit                         = Kindle.TId (prim UNIT)

cFun' env (ETempl x t te c)           = do (ds,c,t) <- cCmd (pushSelf x env') c
                                           y <- newName "_"
                                           let c' = Kindle.CBind [(x,Kindle.Val t0 (Kindle.ENew n []))] c
                                           return (ds, [(y,Kindle.TWild)], c', t)
  where n                             = tId (tHead (deQualify s))
        Kindle.Struct se'             = lookup' (decls env) n
        t0                            = Kindle.TId n
        env'                          = addDict (dom se `zip` dom se')

--cFun' env (EDo c)                     = do (ds,c,t) <- cCmd (pushSelf x env) c
--                                           return (ds, [(x,t0)], c, t)

cFun' env e                           = do (ds,c,t) <- cBody env e
                                           return (ds, [], c, t)


cBody = undefined

protect = undefined

pushSelf = undefined

cExp = undefined

cCmd = undefined