module Prepare4C(prepare4c) where

import Monad
import Common
import Kindle
import qualified Core
import qualified Core2Kindle

-- Establishes that:
--   every struct function simply relays all work to a global function with an explicit "this" parameter
--   every EEnter expression has a variable as its body
--   every ENew expression occurs at the rhs of a Val binding

-- Implements boxing and unboxing of non-ptr values that are cast to/from polymorphic type
-- Replaces tag-only structs with casts from the actual tag value (and corresponding switch cmds)
-- Removes CBreak commands that appear directly under a CSeq (without any intervening CSwitch)
-- Removes redundant defaults in CSwitch commands

-- (Not yet):
-- Flattens the struct subtyping graph by inlining the first coercion field
-- Ditto for enums...


prepare4c e2 e3 m             = localStore (pModule e2 e3 m)


data Env                        = Env { decls   :: Decls,
                                        tenv    :: TEnv,
                                        nulls   :: [Name],
                                        singles :: [Name],
                                        this    :: Maybe (Name,AType) }

env0                            = Env { decls = [], tenv = [], nulls = [], singles = [], this = Nothing }

addDecls ds env                 = env { decls = ds ++ decls env, nulls = ns0 ++ nulls env, singles = ns1 ++ singles env }
  where nss                     = [ ns | (_,Struct _ ns) <- ds ]
        ns                      = concat nss
        ns0                     = filter (isNullCon . lookup' ds) ns
        ns1                     = map head . filter ((==1) . length) . map (\\ns0) $ nss
        isNullCon (Struct [(Prim Tag _, ValT (TId (Prim Int _)))] [])  = True
        isNullCon _                                                    = False

addTEnv te env                  = env { tenv = te ++ tenv env }

addBinds bs env                 = addTEnv (mapSnd typeOf bs) env

addVals te env                  = env { tenv = mapSnd ValT te ++ tenv env }

setThis x t env                 = env { this = Just (x,t) }

findDecl env n
  | isTuple n                   = Struct (mapSnd ValT (tupleSels n)) []
  | otherwise                   = lookup' (decls env) n

allCons env (ACon n _ : _)
  | isTuple n                   = [n]
  | otherwise                   = head [ ns | (_,Struct _ ns) <- decls env, n `elem` ns ]
allCons env _                   = []

pModule e2 dsi (Module m ns ds bs)      
                                = do -- tr (render (vcat (map pr dsi))
                                     let (_,_,Core.Binds _ te1 _,Core.Binds _ te2 _) = e2
                                     tei <- Core2Kindle.c2kTEnv dsi (te1++te2)
                                     let env1 = addTEnv (primTEnv++tei) (addDecls (primDecls++dsi) env0)
                                         env  = addBinds bs (addDecls ds env1)
                                     (bs1,bs) <- pMap (pBind env) bs
                                     bs2 <- currentStore
                                     return (Module m ns (pDecls env ds) (bs1 ++ bs ++ reverse bs2))



pDecls env ds                   = map pDecl (prune ds (nulls env))
  where pDecl (n,Struct te cs)
         | n `elem` singles env = (n, Struct (tail te) cs)
        pDecl d                 = d


pMap f xs                       = do (bss,xs) <- fmap unzip (mapM f xs)
                                     return (concat bss, xs)


-- Prepare bindings
pBind env (x, Val t e)          = do (bs,_,e) <- pRhsExp env e 
                                     return (bs, (x, Val t e))
pBind env (x, Fun t te c)       = do c <- pCmd (addVals te env) c
                                     return ([], (x, Fun t te c))


-- Prepare struct bindings
pSBind env n (x, Val t e)       = do (bs,_,e) <- pRhsExp env e 
                                     return (bs, (x, Val t e))
pSBind env n b@(x, Fun t te (CRet (ECall f (EThis:es))))
  | es == map EVar (dom te)     = return ([], b)
pSBind env n (x, Fun t te c)    = do f <- newName functionSym
                                     y <- newName thisSym
                                     c <- pCmd (addVals te (setThis y (TId n) env)) c
                                     addToStore (f, Fun t ((y,TId n):te) c)
                                     return ([], (x, Fun t te (CRet (ECall f (EThis : map EVar (dom te))))))


-- Prepare commands
pCmd env (CRet e)               = do (bs,e) <- pExp env e
                                     return (cBind bs (CRet e))
pCmd env (CRun e c)             = do (bs,e) <- pExp env e
                                     liftM (cBind bs . CRun e) (pCmd env c)
pCmd env (CBind r bs c)         = do (bs1,bs) <- pMap (pBind (if r then env' else env)) bs
                                     liftM (cBind bs1 . CBind r bs) (pCmd env' c)
  where env'                    = addBinds bs env
pCmd env (CUpd x e c)           = do (bs,e) <- pExp env e
                                     liftM (cBind bs . CUpd x e) (pCmd env c)
pCmd env (CUpdS e x e' c)       = do (bs,e) <- pExp env e
                                     (bs',e') <- pExp env e'
                                     liftM (cBind bs . cBind bs' . CUpdS e x e') (pCmd env c)
pCmd env (CUpdA e i e' c)       = do (bs,e) <- pExp env e
                                     (bs',e') <- pExp env e'
                                     liftM (cBind bs . cBind bs' . CUpdA e i e') (pCmd env c)
pCmd env (CSwitch e alts)
  | any litA alts               = do (bs,e) <- pExp env e
                                     alts <- mapM (pAlt env) alts
                                     return (cBind bs (CSwitch e alts))
  | otherwise                   = do (bs,e) <- pExp env e
                                     alts <- mapM (pAlt env) alts
                                     let (alts0,alts1) = partition nullA [ a | a@(ACon _ _) <- alts ]
                                         altsW         = [ a | a@(AWild _) <- alts ]
                                     return (cBind bs (pSwitch env e (alts0++absent0 altsW) (alts1++absent1 altsW)))
  where nullA (ACon n c)        = n `elem` nulls env
        nullA _                 = False
        absent                  = allCons env alts \\ [ n | ACon n _ <- alts ]
        (abs0,abs1)             = partition (`elem` nulls env) absent
        absent0 altsW           = [ ACon n d | n <- abs0, AWild d <- altsW ]
        absent1 altsW           = [ a | a <- altsW, not (null abs1) ]
        litA (ALit _ _)         = True
        litA _                  = False
pCmd env (CSeq c c')            = liftM2 pSeq (pCmd env c) (pCmd env c')
pCmd env (CBreak)               = return CBreak
pCmd env (CRaise e)             = do (bs,e) <- pExp env e
                                     return (cBind bs (CRaise e))
pCmd env (CWhile e c c')        = do (bs,e) <- pExp env e
                                     c <- pCmd env c
                                     liftM (cBind bs . CWhile e c) (pCmd env c')
pCmd env (CCont)                = return CCont


pSeq c1 c2                      = case anchor c1 of
                                    (bf,CBreak) -> bf c2
                                    (bf,CCont)  -> c1
                                    _           -> CSeq c1 c2
  where anchor (CBind r bs c)   = (CBind r bs . bf, c')
          where (bf,c')         = anchor c
        anchor (CRun e c)       = (CRun e . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpd x e c)     = (CUpd x e . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpdS e x e' c) = (CUpdS e x e' . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpdA e i e' c) = (CUpdA e i e' . bf, c')
          where (bf,c')         = anchor c
        anchor c                = (id, c)


pSwitch env e [] [ACon n c]
  | n `elem` singles env        = c
pSwitch env e [] [AWild c]      = c
pSwitch env e [] alts1          = CSwitch e alts1
pSwitch env e [ACon n c] []
  | n `elem` singles env        = c
pSwitch env e alts0 []          = CSwitch (tagless e) alts0
pSwitch env e alts0 alts1       = pSwitch env e (alts0++[AWild d]) []
  where d                       = pSwitch env e [] alts1

tagless e                       = ECast (TId (prim Int)) (noTag e)

noTag (ESel e (Prim Tag _))     = e
noTag e                         = e


-- Prepare switch alternatives
pAlt env (ALit l c)             = liftM (ALit l) (pCmd env c)
pAlt env (ACon n c)             = liftM (ACon n) (pCmd env c)
pAlt env (AWild c)              = liftM AWild (pCmd env c)


-- Prepare a right-hand-side expression
pRhsExp env (ENew n bs)
  | n `elem` nulls env          = return ([], TId n, EVar n)
  | otherwise                   = do (bs1,bs) <- pMap (pSBind env n) bs'
                                     return (bs1, TId n, ENew n bs)
  where bs'                     = if n `elem` singles env then prune bs [prim Tag] else bs
pRhsExp env e                   = pExp' env e


-- Prepare an expression in an arbitrary position
pExp env e                      = do (bs,t,e) <- pExp' env e
                                     return (bs,e)
                                     

-- Prepare an expression in an arbitrary position and compute its type
pExp' env e@(EVar x)
  | isCon x                         = return ([], TId x, e)
  | otherwise                       = return ([], rngType (lookup' (tenv env) x), e)
pExp' env e@(ELit l)                = return ([], litType l, e)
pExp' env (EThis)                   = return ([], t, EVar x)
  where (x,t)                       = fromJust (this env)
pExp' env (ESel e l)                = do (bs,TId n,e) <- pExp' env e
                                         let Struct te _ = findDecl env n
                                         return (bs, rngType (lookup' te l), ESel e l)
pExp' env (ECall f es)              = do (bs,es) <- pMap (pExp env) es
                                         return (bs, rngType (lookup' (tenv env) f), ECall f es)
pExp' env (EEnter (EVar x) f es)    = do (bs1,TId n,e) <- pExp' env (EVar x)
                                         (bs2,es) <- pMap (pExp env) es
                                         let Struct te _ = findDecl env n
                                         return (bs1++bs2, rngType (lookup' te f), EEnter e f es)
pExp' env (EEnter e f es)           = do (bs1,t@(TId n),e) <- pRhsExp env e
                                         (bs2,es) <- pMap (pExp env) es
                                         x <- newName tempSym
                                         let Struct te _ = findDecl env n
                                         return (bs1++bs2++[(x, Val t e)], rngType (lookup' te f), EEnter (EVar x) f es)
pExp' env (ECast TWild e)           = do (bs,t',e) <- pExp' env e
                                         if mustBox t' then do
                                             x <- newName tempSym
                                             let bs1 = [(x, Val (TId (box t')) (ENew (box t') bs2))]
                                                 bs2 = [(prim Value, Val t' e)]
                                             return (bs++bs1, TWild, ECast TWild (EVar x))
                                          else if smallPrim t' then
                                             return (bs, TWild, ECast TWild (ECast (TId (prim Int)) e))
                                          else if t' /= TWild then
                                             return (bs, TWild, ECast TWild e)
                                          else
                                             return (bs, TWild, e)
pExp' env (ECast t e)               = do (bs,t',e) <- pExp' env e
                                         if t'==TWild && mustBox t then
                                             return (bs, t, ESel (ECast (TId (box t)) e) (prim Value))
                                          else if t'==TWild && smallPrim t then
                                             return (bs, t, ECast t (ECast (TId (prim Int)) e))
                                          else if t /= t' then
                                             return (bs, t, ECast t e)
                                          else
                                             return (bs, t, e)
pExp' env (ENew n bs)
  | n `elem` nulls env              = return ([], TId n, EVar n)
  | otherwise                       = do (bs1,bs) <- pMap (pSBind env n) bs'
                                         x <- newName tempSym
                                         return (bs1++[(x, Val (TId n) (ENew n bs))], TId n, EVar x)
  where bs'                         = if n `elem` singles env then prune bs [prim Tag] else bs


mustBox (TId (Prim p _))            = p `elem` boxedPrims
mustBox _                           = False

smallPrim (TId (Prim p _))          = p `elem` smallPrims
smallPrim _                         = False

box (TId (Prim Time _))             = prim TimeBox
box (TId (Prim Int _))              = prim IntBox
box (TId (Prim Float _))            = prim FloatBox

