module Core2Kindle(core2kindle) where
{- -}
import Common
import Core
import qualified Kindle
import qualified Char


core2kindle = undefined
{-
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

p  ::=  { T = D } ; { m(ts):t = f } ; { x:t = e }                                                  program

D  ::=  class { m(ts):t, x:t }  |  enum { K }                                                      declaration

f  ::=  (xs) c                                                                                     closure expression

c  ::=  { m(ts):t = f }; c  |  x:t = e; c  |  e.x := e; c  |  return x  |  switch x { K -> c }     command

e  ::=  x  |  n  |  m(es)  |  (t)e  |  new T { m = f, x = e } |  e.x  |  e.m(es)  |  K  |  this    expression

t  ::=  T  |  ?                                                                                    type

T                                                                                                  type name

K                                                                                                  constant name

m                                                                                                  method/function name

x                                                                                                  field/variable name


Translating closures     A |- f --> f' : (ts)t
====================


|- ss --> ts     A,xs:ts |- e --> c : t
---------------------------------------
A |- \xs:ss -> e --> (xs) c : (ts) t


|- s --> t    A|x:t |- c --> c' : t'
------------------------------------
A |- do@x:s c --> (x) c' : (t)t'


|- s --> T     A|x:T |- c --> c' : t
----------------------------------------------------
A |- template@x:s c --> (_) x:T = new T {}; c' : (?)t


A(x) = T    A|x:T |- c --> c' : t
---------------------------------------------------------
A |- request@x c --> (_) lock(x); UNLOCK(x,c',t) : (?)t


A(x) = T    A|x:T |- c --> c' : t    y new
---------------------------------------------------------------------------------------------------------------------
A |- action@x c --> (a,b) Msg y = new Msg { aft = a; bef = b; to = x; code = (x) c'}; return post(y) : (Time,Time)Msg



UNLOCK(x, E;return e,             t) = E; y:t = e; unlock(x); return y
UNLOCK(x, E;case e of {K_i->c_i}, t) = E; case e of {K_i -> UNLOCK(x,c_i,t)}



Translating commands   A |- c --> c' : t
====================

E|A |- e --> e' : t      x new
-----------------------------------------
A |- return e --> E; x:t=e'; return x : t


|- ss --> ts    A,xs:ts |- fs --> fs' : ts   A,xs:ts |- c --> c' : t
--------------------------------------------------------------------
A |- xs:ss=fs; c --> xs:ts=fs; c' : t


|- s --> t    E|A |- e --> e' : t    A,x:t |- c --> c' : t'
-----------------------------------------------------------
A |- x:s=e; c --> E,x:t=e'; c' : t'


|- s --> t    E|A |- e 0 0 --> e' : t    A,x:t=e' |- c --> c' : t'
------------------------------------------------------------------
A |- x:s <- primAction2O e; c --> E,x:t=e'; c' : t'


|- s --> t    E|A |- e y --> e' : t    A,x:t |- c --> c' : t'
-------------------------------------------------------------        -- y ("self")
A |- x:s <- e; c --> x:t=e'; c' : t'


S(x) = t     E|A |- e --> e' : t    A |- c --> c' : t'
------------------------------------------------------               -- S(x)
A |- x := e; c --> E; y.x := e'; c' : t'


E|A |- e y --> e' : t   x new
-------------------------------                                      -- y ("self")
A |- e --> x:t=e'; return x : t



Translating bodies   A |- e --> c : t
==================


|- ss --> ts    A,xs:ts |- fs --> fs' : ts    A,xs:ts |- e --> c : t
--------------------------------------------------------------------
A |- letrec xs:ss=fs in e --> xs:ts=ds; c : t


|- s --> t      E|A |- e --> e1 : t    A,x:t |- e' --> c : t'
-------------------------------------------------------------
A |- let x:s=e in e' --> E; x:t=e1; c : t'


E|A |- e --> e' : t     |- ss_i --> ts_i     A,xs_i:ts_i |- e_i --> c_i : t'       x new
----------------------------------------------------------------------------------------------------------
A |- case e of {K_i -> \xs_i:ss_i->e_i} --> E; x:t=e'; switch x of {K_i -> xs_i:ts_i=(K_i)x.args;c_i} : t'


E|A |- e --> e' : t       x new
----------------------------------
A |- e --> E; x:t=e'; return x : t



Translating expressions   E|A |- e --> e' : t
=======================


A(x) = t
------------------
.|A |- x --> x : t


--------------------
.|A |- n --> n : Int


E|A |- e -> e' : t'   t/=t'
---------------------------
E|A |- e -> (t)e' : t


E|A |- e --> e' : Msg              x,y new
------------------------------------------------------
E;x:(?)Msg=(y)e'|A |- primAction2O e --> e'(0,0) : Msg


E|A |- e --> e' : (ts)t    E'|A |- es --> es' : ts
--------------------------------------------------
E,E'|A |- e es --> e'(es') : t


A(K) = class { tag : T; args : ts }   E|A |- es --> es' : ts
------------------------------------------------------------
E|A |- K es --> new T { tag = K, args = es' } : K


A(T) = class { xs : ts }   E|A |- es --> es' : ts
-------------------------------------------------
E|A |- T { xs = es } --> new T { xs = es' } : T


A(l) = (T)t   E|A |- e --> e' : T
---------------------------------
E|A |- e.l --> e'.l : t


|- ss --> ts    A,xs:ts |- fs --> fs' : ts    E|A,xs:ts |- e --> e' : t
------------------------------------------------------------------------
xs:ts=fs';E|A |- letrec xs:ss=fs in e --> e' : t


|- s --> t    E|A |- e --> e1 : t     E'|A,x:t |- e' --> e2 : t'
----------------------------------------------------------------
E;x:t=e1;E'|A |- let x:s=e in e' --> e2 : t'


A |- case e of {K_i->e_i} --> c : t     x new
-----------------------------------------------
x:()t=()c|A |- case e of {K_i->e_i} --> x() : t


A |- f --> f' : t     x new
---------------------------
x:t=f|A |- f --> x : t



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
core2kindle m                       = convModule m



data Env                            = Env { decls :: Kindle.CEnv,
                                            binds :: Kindle.TEnv,
                                            state :: Kindle.TEnv }


addDecls ds env                     = env { decls = ds ++ decls env }

setState vs env                     = env { state = vs }





convModule (Module m ds [] bs)      = do cs <- convDecls ds
                                         (fs,vs) <- convBinds (addDecls cs env0) bs
                                         return (Kindle.Module m cs fs vs)




convDecls (Types ke ds)             = do css <- mapM convDecl ds
                                         return (concat css)
  where convDecl (c,DData vs _ cs)  = do cs <- mapM convCon cs
                                         return ((c, Kindle.Enum (dom cs)) : cs)
        convDecl (c,DRec _ vs _ ss) = return [(c, Kindle.Struct (convTEnv ss))]
        convDecl (c,DType vs t)     = error ("Internal: cannot yet compile type synonyms ")
        convCon (c,Constr ts ps ke) = do te1 <- newEnv "arg" ts
                                         te2 <- newEnv "wit" ps
                                         return (c, Kindle.Struct (convTEnv (te1++te2)))


convTEnv te                         = mapSnd convScheme te


convScheme (Scheme rh ps ke)        = undefined


convBinds env (Binds r te eqs)      = do (fss,vss) <- fmap unzip (mapM convBind eqs)
	      	       	  	         undefined                                         
  where env1                        = addTypes te1 env
        te1                         = convTEnv te
        convBind (x,ELam te' e)     = convEqn t x (dom te' `zip` ts) e
          where Kindle.TFun ts t    = lookup x te1
        convBind (x,e)              = convEqn t x [] e
          where t                   = lookup x te1
--        convEqn t x te (ETempl y te' (Just t) c)


{-


c2kModule (Module m ds [] bs)       = do cs <- c2kDecls ds
                                         (fs,vs) <- c2kBinds (addDecls cs env0) bs
                                         return (Kindle.Module m cs fs vs)
  where env0                        = Env { decls = Kindle.primDS,
                                            state = [] }


c2kDecls (Types ke ds)              = do dss <- mapM c2kDecl ds
                                         return (concat dss)


c2kDecl (c, DData vs _ cs)          = do ds <- mapM c2kConstr cs
                                         return ((untick c, Kindle.Union (dom cs)) : ds)
c2kDecl (c, DRec _ vs _ ss)         = return [(untick c, Kindle.Class ve)]
  where ve                          = c2kTEnv ss
c2kDecl (c, DType vs t)             = error "Internal: type synonyms not yet implemented"


c2kConstr (c, Constr ts ps ke)      = do xs <- newVars "arg" (length ts)
                                         xs' <- newVars "wit" (length ps)
                                         let ve = c2kTEnv (xs `zip` ts ++ xs' `zip` ps)
                                         return (untick c, Kindle.Class ve)


convertVar x
  | x `elem` Kindle.syms            = x
  | x == "/="                       = "!="
  | x == Prim.div                   = "/"
  | x == Prim.mod                   = "%"
  | x == Prim.negate                = "-"
  | x == Prim.not                   = "!"
  | isSym x                         = "sym_" ++ concat (map (show . Char.ord) x)
  | otherwise                       = untick x
-}

noFun ([],t)                        = t
noFun (ts,t)                        = Kindle.TClos ts t


splitFun n (ts,t)                   = (ts1, noFun (ts2,t))
  where (ts1,ts2)                   = splitAt n ts
        

funRange n t                        = snd (splitFun n t)


c2kScheme (Scheme rh [] ke)         = c2kRho rh
c2kScheme (Scheme rh ps ke)         = c2kScheme (Scheme (tFun ps rh) [] ke)


c2kRho (F sc rh)                    = let (ts,t) = c2kRho rh in (c2kVScheme sc : ts, t)
c2kRho (R t)                        = c2kType t


c2kType t                           = f (tFlat t)
  where f (TId c, [t1,t2]) 
          | c `elem` ["->",Prim.o]  = let (ts,t) = c2kType t2 in (c2kVType t1 : ts, t)
        f (TId c, [t])
          | c `elem` prims          = ([Kindle.TPoly], c2kVType t)
          where prims               = [Prim.template,Prim.cmd,Prim.request]
        f (TId c, [])
          | c == Prim.action        = ([Kindle.TPoly], Kindle.TId Prim.message)
        f (t', ts)                  = ([], c2kT t' (map c2kVType ts))


c2kT (TId "Int") _                  = Kindle.TPrim "int"
c2kT (TId "Float") _                = Kindle.TPrim "float"
c2kT (TId "Char") _                 = Kindle.TPrim "char"
c2kT (TId c) ts
  | isCon c                         = Kindle.TId c
  | otherwise                       = Kindle.TPoly
c2kT _ _                            = Kindle.TWild



c2kVType t                          = noFun (c2kType t)

c2kVScheme sc                       = noFun (c2kScheme sc)

c2kTEnv sigs                        = map f sigs
  where f (x,t)                     = (convertVar x, c2kVScheme t)


token                               = "_token"



c2kBindsNew :: Env -> Binds -> M ([FDef],[VDef])
c2kBindsNew env (Binds _ te0 eqs)   = do (fss,vss) <- fmap unzip (mapM (c2kBind env te) eqs)
                                         return (concat fss, concat vss)


c2kBind env te0 (v, ELam te e)      = c2kBind' env v te (lookup' te0 v) e
c2kBind env te0 (v, e)              = c2kBind' env v [] (lookup' te0 v) e


c2kBind' env v te t (EAct x c)      = c2kCmd env x c
c2kBind' env v te t (EReq x c)      = c2kCmd env x c
-- c2kBind' env v te (EDo c)        = c2kCmd env c
c2kBind' env v te t (ETempl x te c) = do b <- c2kCmd env x c
	       	    	      	         undefined
--                                         return ([(v, Kindle.Fun (te++[(x,Kindle.TWild)])], t
c2kBind' env v [] t e               = do (fs,vs,e) <- c2kExp env e
                                         return (fs, vs++[(v,Kindle.Val (c2kVScheme t) e)])
c2kBind' env v te t e               = do b <- c2kBody env e
                                         return ([(v, Kindle.Fun te t b)], [])
                                         


c2kCmd :: Env -> Id -> Cmd -> M Kindle.Body
c2kCmd env x (CGen v t e c)         = do (fs,vs,e) <- c2kExp env (EAp e (EVar x))
                                         t <- c2kVScheme t
                                         b <- c2kCmd env x c
                                         return (Kindle.body fs (vs++[(convertVar v, Kindle.Val t e)]) b)
c2kCmd env x (CAss v e c)           = do (fs,vs,e) <- c2kExp env e
                                         b <- c2kCmd env x c
                                         return (Kindle.body fs vs (Kindle.BAss (convertVar x) (convertVar v) e b))
c2kCmd env x (CLet bs c)            = do (fs,vs) <- c2kBinds env bs
                                         b <- c2kCmd env c
                                         return (Kindle.body fs vs b)
c2kCmd env x (CRet e)               = do (fs,vs,e) <- c2kExp env e
                                         return (Kindle.body fs vs (Kindle.BExp e))
c2kCmd env x (CExp e)               = c2kCmdExp env x e



c2kCmdExp env x e                   = do (fs,vs,e) <- c2kExp env (EAp e (EVar x))
                                         return (Kindle.body fs vs (Kindle.BExp e))

c2kBinds env (Binds rec te0 eqs)    = walk eqs
  where walk []                     = return ([],[])
        walk ((x,e):eqs)
          | etaExpands e            = do f <- c2kMeth env (funRange (n + 1) t) e
                                         (fs,vs) <- walk eqs
                                         return ((convertVar x, f) : fs, vs)
          | arity e > 0             = do f <- c2kFun env (funRange n t)  e
                                         (fs,vs) <- walk eqs
                                         return ((convertVar x, f) : fs, vs)
          | otherwise               = do (fs1,vs1,e') <- c2kExp env e
                                         (fs2,vs2) <- walk eqs
                                         return (fs1++fs2, vs1++[(convertVar x, Kindle.Val (noFun t) e')]++vs2)
          where t                   = c2kScheme (lookup' te0 x)
                n                   = arity e


c2kMeth env t e0                    = do -- tr ("Flat: " ++ show gs)
                                         -- tr ("      " ++ show e' ++ " " ++ show es')
                                         b <- foldSeq env nullSubst (e',es') gs
                                         return (Kindle.Fun (mapSnd c2kVScheme te ++ [(token,t0)]) t b)
  where (te,e)                      = eAbs e0
        (gs, e', es')               = flatSeq (eFlat e)
        t0                          = stateType gs


c2kMethBody env e                   = foldSeq env nullSubst (e',es') gs
  where (gs, e', es')               = flatSeq (eFlat e)


c2kFun env t (ELam te e)            = do e' <- c2kBody env e
                                         return (Kindle.Fun (mapSnd c2kVScheme te) t e')
c2kFun env t e                      = do e' <- c2kBody env e
                                         return (Kindle.Fun [] t e')


renameBinds bs e                    = do xs' <- mapM newVar xs
                                         let s   = xs `zip` xs'
                                             es' = if rec then subst s es else es
                                             ys' = substVars (xs`zip`xs') ys
                                         return (Binds rec (ys' `zip` ts) (xs' `zip` es'), subst s e)
  where Binds rec te eqs            = bs
        (xs,es)                     = unzip eqs
        (ys,ts)                     = unzip te




c2kLetBody env bs e                 = do (fs,vs) <- c2kBinds env bs
                                         b <- c2kBody env e
                                         return (Kindle.body fs vs b)


c2kBody env (ELet bs e)
  | clash bs                        = do (bs',e') <- renameBinds bs e
                                         c2kLetBody env bs' e'
  | otherwise                       = c2kLetBody env bs e
c2kBody env (ECase e as d)
  | any isLitAlt as                 = do (fs,vs,e') <- c2kExp env e
                                         as' <- mapM (c2kAlt' env) as
                                         d' <- c2kBody env d
                                         return (Kindle.body fs vs (Kindle.BCase' e' as' d'))
  | otherwise                       = do (fs,vs,e') <- c2kExp env e
                                         as' <- mapM (c2kAlt env) as
                                         d' <- c2kBody env d
                                         return (Kindle.body fs vs (Kindle.BCase Kindle.TWild e' as' d'))
c2kBody env e                       = c2kMatch env (eFlat e)



c2kMatch env (EVar c, [])
  | c == Prim.fail                  = return Kindle.BBreak
c2kMatch env (EVar c, [e])
  | c == Prim.match                 = c2kBody env e
  | c == Prim.commit                = c2kBody env e
c2kMatch env (EVar c, [e1,e2])
  | c == Prim.fatbar                = do b1 <- c2kBody env e1
                                         b2 <- c2kBody env e2
                                         return (Kindle.BSeq b1 b2)
c2kMatch env (EVar c, es)
  | c `elem` matchprims             = error ("Internal.c2kBuiltins: " ++ c ++ " " ++ show (length es) ++ "  " ++ show es)
  where matchprims                  = [ Prim.match, Prim.commit, Prim.fail, Prim.fatbar ]
c2kMatch env (e, es)                = do (fs,vs,e') <- c2kExp env (eAp e es)
                                         return (Kindle.body fs vs (Kindle.BExp e')) 
                                         


c2kAlt env (PCon c, e)              = do x <- newVar tempSym
                                         b <- c2kBody env (eLet (bss x) (e2 x))
                                         return (Kindle.Alt c' x b)
  where c'                          = untick c
        Kindle.Class ve             = Kindle.findClassByName (decls env) c'
        sels                        = dom ve
        n                           = length sels
        (te,e1)                     = eAbs e
        m                           = length te
        (te1,te2)                   = splitAt n te
        (sels1,sels2)               = splitAt m sels
        bss x | n <= m              = zipWith (f x) te1 sels
              | otherwise           = zipWith (f x) te sels1
        e2 x | n <= m               = eLam te2 e1
             | otherwise            = eAp e1 (es x)
        es x                        = map (\s -> EAp (ESel s) (EVar x)) sels2
        f x xt s                    = Binds False [xt] [(fst xt, EAp (ESel s) (EVar x))]


c2kAlt' env (PLit l, e)             = do e' <- c2kBody env e
                                         return (Kindle.Alt' l e')


c2kExpList env []                   = return ([],[],[])
c2kExpList env (e:es)               = do (fs1,vs1,e') <- c2kExp env e
                                         (fs2,vs2,es') <- c2kExpList env es
                                         return (fs1++fs2, vs1++vs2, e':es')


c2kExp env (ELit l)                 = return ([], [], Kindle.ELit l)
c2kExp env (ERec eqs)               = do (fs,vs,es') <- c2kExpList env es
                                         let eqs' = sels' `zip` es'
                                             es'' = map (lookup' eqs') (dom ve)
                                         return (fs, vs, Kindle.ENew c (Kindle.mkVDefs ve es''))
  where (sels,es)                   = unzip eqs
        sels'                       = map chopSel sels
        (c, Kindle.Class ve)        = Kindle.findClassByLabel (decls env) (head sels')
c2kExp env e
  | etaExpands e                    = do f <- c2kMeth env Kindle.TWild e
                                         x <- newVar tempSym
                                         return ([(x,f)], [], Kindle.EVar x)
  | otherwise                       = c2kApp env e []


c2kApp env (EAp e e') es            = do (fs1,vs1,e'') <- c2kExp env e'
                                         (fs2,vs2,e) <- c2kApp env e (e'':es)
                                         return (fs1++fs2, vs1++vs2, e)
c2kApp env (ELet bs e) es
  | mustRename                      = do (bs',e') <- renameBinds bs e
                                         c2kLetApp env bs' e' es
  | otherwise                       = c2kLetApp env bs e es
  where mustRename                  = bvars bs `overlaps` evars es || clash bs
c2kApp env (ECon c) es              = do return ([], [], Kindle.ENew (untick c) (Kindle.mkVDefs ve es))
  where Kindle.Class ve             = Kindle.findClassByName (decls env) c'
        c'                          = untick c
c2kApp env (ESel l) [e]             = return ([], [], Kindle.ESel e (chopSel l))
c2kApp env (ESel l) (e:es)          = return ([], [], Kindle.ECall (Kindle.ESel e (chopSel l)) es)
c2kApp env (EVar x) []              = return ([], [], Kindle.EVar (convertVar x))
c2kApp env (EVar x) es              = return ([], [], Kindle.ECall (Kindle.EVar (convertVar x)) es)
c2kApp env e es                     = do f <- c2kFun env Kindle.TWild e
                                         x <- newVar tempSym
                                         if Kindle.arity f == 0 || not (null es) then 
                                            return ([(x,f)], [], Kindle.ECall (Kindle.EVar x) es)
                                          else 
                                            return ([(x,f)], [], Kindle.EVar x)


c2kLetApp env bs e es               = do (fs1,vs1) <- c2kBinds env bs
                                         (fs2,vs2,e') <- c2kApp env e es
                                         return (fs1++fs2, vs1++vs2, e')



{-

-- BIND a b = \s ->  x <- a s;
--                   b x s
--
-- RET e    = \s ->  e
--
-- SET e    = \s ->  s.x1 := e.x1;
--                   ...
--                   s.xn := e.xn
--
-- GET      = \s ->  s
--
-- ACT o e  = \s ->  ACT$ o (\s' -> e s')
--
-- REQ o e  = \s ->  REQ$ o (\s' -> e s')
--
-- NEW e    = \s ->  NEW$ e
--
-- AFT t e  = \s ->  x <- AFT$ t;
--                   e s
--
-- BEF t e  = \s ->  x <- BEF$ t;
--                   e s

-}

etaExpands (ELam te e)              = etaExpands e
etaExpands (ELet bs e)              = etaExpands e
etaExpands (ECase e alts d)         = any etaExpands (d : map snd alts)
etaExpands (EAp e e')               = etaExpands e
etaExpands (EVar x)                 = x `elem` etaprims
etaExpands e                        = False

etaprims                            = [Prim.bind, Prim.ret, Prim.act, Prim.req, Prim.new, Prim.aft, Prim.bef, Prim.set, Prim.get]


stateType ((x,t,e,es):gs)
  | e == EVar Prim.get              = c2kVScheme t
  | e == EVar Prim.set              = case eFlat (head es) of
                                        (ECon c, _) -> c2kVType (TId c)
                                        _ -> stateType gs
  | otherwise                       = stateType gs
stateType []                        = Kindle.TPoly


mkAssign e b                        = foldr assign b (proper e)
  where proper (Kindle.ENew c [v])  = mapSnd Kindle.expOfVal [v]
        proper (Kindle.ENew c vs)   = concat (zipWith f vs [1..])
          where f (l,Kindle.Val _ e) n
                  | e == ident      = []
                  | otherwise       = [(l,e)]
                  where ident       = Kindle.ESel (Kindle.EVar token) (chopSel (Prim.tuple_sel size n))
                size                = length vs
  
        assign (l,e) b              = Kindle.BAss token l e b


foldSeq env s e0 ((x,t,EVar y,[]):gs)
  | y == Prim.get                   = foldSeq env (x +-> token) e0 gs
foldSeq env s e0 ((x,t,EVar y,[e]):gs)
  | y == Prim.set                   = do (fs,vs,e') <- c2kExp env (subst s e)
                                         b <- foldSeq env s e0 gs
                                         return (Kindle.body fs vs (mkAssign e' b))
foldSeq env s e0 ((x,t,e,es):gs)    = do (fs,vs,e') <- c2kStmt env (subst s e) (subst s es)
                                         b <- foldSeq env s e0 gs
                                         return (Kindle.body fs vs (Kindle.BGen (x, Kindle.Val (c2kVScheme t) e') b))
foldSeq env s (EVar y, [e]) []
  | y == Prim.set                   = do (fs,vs,e') <- c2kExp env (subst s e)
                                         return (Kindle.body fs vs (mkAssign e' unitBody))
  where unitBody                    = Kindle.BExp (Kindle.ENew Prim.unit [])
foldSeq env s (e0,es0) []           = do (fs,vs,e') <- c2kStmt env (subst s e0) (subst s es0)
                                         return (Kindle.body fs vs (Kindle.BExp e'))

c2kStmt env (EVar y) [e]
  | y == Prim.ret                   = c2kExp env e
  | y `elem` [Prim.new,Prim.aft,Prim.bef]
                                    = do (fs,vs,e') <- c2kExp env e
                                         return (fs, vs, Kindle.ECall (Kindle.EVar (mkRTS y)) [e'])
c2kStmt env (EVar y) [e1,e2]        
  | y `elem` [Prim.act,Prim.req]    = do (fs1,vs1,e1') <- c2kExp env e1
                                         (fs2,vs2,e2') <- c2kExp env e2
                                         return (fs1++fs2, vs1++vs2, Kindle.ECall (Kindle.EVar (mkRTS y)) [e1',e2'])
c2kStmt env e es                    = c2kExp env (eAp e (es++[EVar token]))



flatSeq (EVar y, [e1,ELam [(x,t)] e2])
  | y == Prim.bind                  = (gs1++[(x,t,e1',es1)]++gs2, e2', es2)
  where (gs1,e1',es1)               = flatSeq (eFlat e1)
        (gs2,e2',es2)               = flatSeq (eFlat e2)
flatSeq (EVar y, [e1,e2])
  | y `elem` [Prim.aft,Prim.bef]    = ([(dummy,scheme (TId Prim.unit),EVar y,[e1])], e2', es2)
  where (e2',es2)                   = eFlat e2
flatSeq (e, es)                     = ([], e, es)


dummy                               = "_dummy"
                                      

mkRTS x
  | x == Prim.act                   = Prim.rtsAct
  | x == Prim.req                   = Prim.rtsReq
  | x == Prim.new                   = Prim.rtsNew
  | x == Prim.aft                   = Prim.rtsAft
  | x == Prim.bef                   = Prim.rtsBef
  | otherwise                       = x
-}
