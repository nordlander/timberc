-- -*- haskell-hugs-program-args: ("-98" "+.") -*-
module Main where

-- Haskell98
import System(getArgs)
import List(isSuffixOf)
import qualified Monad
import qualified Char
import qualified Control.Exception as Exception ( catchDyn, catch )
import System.Console.GetOpt
import qualified Directory

-- Timber Compiler
import System.FilePath
import Config
import Execution
import PP
import Common
import Parser
import qualified Syntax
import qualified Core
import Desugar1
import Rename
import Desugar2
import Depend
import Syntax2Core
import Kind
import Type
import Termred
import Type2
import Kindle
import Kindlered
import Core2Kindle
import Lambdalift
import Prepare4C
import Kindle2C
import Data.Binary
import Interfaces
import GHC.Exception
{-

Lexer:
- Checks lexical syntax and builds token stream
- Removes comments

Parser:
- Checks basic syntax and builds parse tree
- Builds constructor syntax (parsed as qualified types)
- Resolves nested infix expressions
- Converts record labels to proper selectors (with prefix .)
- Replaces prefix tuple constructors with primitive names

Desugar1:
- Checks record type acyclicity and selector consistence
- Completes dotdot record patterns and expressions with missing fields
- Translates instance declarations into instance signatures and named record terms
- Checks and resolves if/elsif/else statements
- Makes the "self" variable explicit.
- Checks validity of return statement
- Replaces prefix expression statement with dummy generator statement
- Replaces if/case statements with corresponding expressions forms
- Checks the restricted command syntax of template bodies

Rename:
- Checks for duplicate declarations and kind signatures
- Checks for duplicate selectors and constructors, and overlaps with class members
- Checks for duplicate equations and type signatures
- Checks for duplicated variables in patterns and templates
- Checks for duplication and dandling abstractions in type predicates
- Checks for unbound variables (not yet implemented -- what to do with globals?)

- Makes every type variable binding explicit in signatures and constructor declarations
- Shuffles signatures as close as possible to the corresponding definition (incl. inside patterns)

- Renames local variables using unique names
- Renames local (explicitly quantified) type variables using unique names

Desugar2:
- Checks validity of type and type scheme syntax in signatures and declarations
- Checks qualified types for syntactic ambiguity
- Removes special syntax for list, tuple and function types, as well as subtype predicates
- Captures single-variable predicates as short-hand for wildcard kind signature
- Checks syntactic validity of patterns
- Replaces after, before, [] and (:) with primitive names
- Replaces tuple expressions with primitive constructor applications 
- Translates if expressions, operator sections, negations
- Collects functions defined with multiple equations while checking arities
- Flattens pattern bindings
- Removes non-trivial patterns in lambdas, assignments and generators
- Translates list expressions/patterns and list comprehensions

- Applies pattern-matching compiler to case expressions

Syntax2Core:
- Injects instance signatures among global type signatures

- Splits predicate contexts into kind environments (quantifiers) and predicates proper (qualifiers)
- Checks validity of base type syntax in declarations
- Replaces _ kinds with unique unification variable
- Replaces _ types with unique unification variable

- Applies up/down signature propagation within constructor/selector/member/signature environment
- Completes bindings and lambdas with missing signatures

- Does dependency analysis on local bindings
- Builds Core syntax tree

Kind:
- Does dependency analysis on type declarations
- Performs kind inference, replacing every kind unification variable (defaulting to *)

Decls:
- Extracts selector and constructor type schemes
- Generates global member signatures and bindings
- Replaces subtyping in type declarations with explicit coercion constructors/selectors

- Initiate subtyping graphs with reflexivity axioms (identity witness)
- Computes subtype above & below graph, closed under transitivity (checks cyclic and ambiguity errors)
- Computes instance overlap graph, closed under superclass relation (checks cyclic and ambiguity errors)

Type:
- Does dependency analysis on top-level bindings
- Performs type inference and constraint simplification/solving
- Makes all function calls match the arity of the called function
- Inserts explicit overloading and subtyping witnesses

Termred:
- Does simple term level reduction and restricted inlining within primitive environment (witnesses only)

Core2Kindle:
- Converts type expressions by making closure types explicit
- Approximates polymorphism by the wildcard type
- Splits non-trivial datatypes into constructor records with a common tag field

- Encodes monadic code by means of extra (mutable) state argument

- Sorts equations into value or function definitions, on basis of arity
- Makes fatbar and fail pattern-matching operators explicit, removes match and commit
- Implements after and before primitives in terms of Time arithmetic
- 
- Replaces act and req by calls to the rts

- Builds Kindle abstract syntax

LambdaLift:
- Abstracts out free local variables from local function definitions and adds corresponding arguments to call sites
- Abstracts out free local variables from struct functions as extra value fields referenced through "this"
- Moves local function definitions to the top level

Kindle2C:
- Shotrcuts data representation for nullary constructors
- Pretty-prints abstract syntax as C code


Yet unknown:
- Prepend module name to global names (terms, types, constructors, selectors, generated *witnesses*)

-}


-- | This compiles a set of Timber modules. 
-- | Example: 'compile ["Main"] compiles the module in Main.t
-- | leaving the result in Main.c
-- | It also compiles every module *called* by this module 
-- | right now.


compileTimber clo ifs (sm,t_file) ti_file c_file h_file
                        = do let Syntax.Module n is _ _ = sm
                             putStrLn ("[compiling "++ t_file++"]")
                             (imps,ifs') <- chaseIfaceFiles is ifs
                             let ((htxt,mtxt),ifc) = runM (passes imps sm)
                             encodeCFile ti_file ifc
                             writeFile c_file mtxt
                             writeFile h_file htxt
                             return ((n,ifc):ifs')
  where passes imps par = do (e0,e1,e2,e3) <- initEnvs imps
                             (d1,a0) <- pass (desugar1 e0)                Desugar1  par
                             rn      <- pass (renameM e1)                 Rename    d1
                             d2      <- pass desugar2                     Desugar2  rn
                             co      <- pass syntax2core                  S2C       d2
                             kc      <- pass (kindcheck e2)               KCheck    co
                             tc      <- pass (typecheck e2)               TCheck    kc
                             rd      <- pass (termred e2)                 Termred   tc
                             tc2     <- pass (typecheck2 e2)              Type2     rd
                             (ki,ds) <- pass (core2kindle e2 e3)          C2K       tc2
                             ki'     <- pass (kindlered e3)               Kindlered ki
                             ll      <- pass (lambdalift e3)              LLift     ki'
                             pc      <- pass (prepare4c e2 e3)            Prepare4C ll
                             c       <- pass (kindle2c (init_order imps)) K2C       pc
                             return (c,ifaceMod a0 tc2 ds)

        pass m p a      = do -- tr ("Pass " ++ show p ++ "...")
                             r <- m a
                             Monad.when (dumpAfter clo p) 
                                $ tr ("#### Result after " ++ show p ++ ":\n\n" ++ render (pr r))
                             Monad.when (stopAfter clo p)
                                $ fail ("#### Terminated after " ++ show p ++ ".")
                             return r                                  

makeProg clo cfg root   = do txt <- readFile (root ++ ".t")
                             let ms@(Syntax.Module n is _ _) = runM (parser txt)
                             (imps,ss) <- chaseSyntaxFiles is [(n,ms)]
                             let cs = compile_order imps
                                 is = filter nonDummy cs
                             let ps = map (\(n,ii) -> (thd ii,modToPath (str n)++".t")) is ++ [(ms,root++".t")]
                             ifs <- compileAll (clo {shortcut = True}) [] ps
                             r <- checkRoot clo ifs root
                             let basefiles = map (rmSuffix ".t" . snd) ps
                                 c_files   = map (++ ".c") basefiles
                                 o_files   = map ((++ ".o") . rmDirs) basefiles
                             mapM (compileC cfg clo) c_files
                             linkO cfg clo{binTarget = root} r o_files
  where nonDummy (_,(_,_,Syntax.Module n _ _ _)) = str n /= ""


parse t_file            = do t_exists <- Directory.doesFileExist t_file
                             Monad.when (not t_exists) (fail ("File " ++ t_file ++ " does not exist."))
                             txt <- readFile t_file
                             return (runM (parser txt),t_file)

compileAll clo ifs []   = return ifs
compileAll clo ifs (p@(ms,t_file):t_files)
                        = do res <- checkUpToDate clo t_file ti_file c_file h_file (impNames ms)
                             if res then do 
                                 putStrLn ("[skipping " ++ t_file ++ " (output is up to date)]")
                                 compileAll clo ifs t_files
                              else do
                                 ifs' <- compileTimber clo ifs (longName p) ti_file c_file h_file
                                 compileAll clo ifs' t_files
  where base            = rmSuffix ".t" t_file
        ti_file         = base ++ ".ti"
        c_file          = base ++ ".c"
        h_file          = base ++ ".h"
        qm              = takeBaseName t_file
        longName (Syntax.Module m a b c,t)
          |reverse(takeWhile (/= '.') (reverse qm))==str m = (Syntax.Module (name0 qm) a b c,t_file)
          |otherwise = errorIds "Module name not last constructor id in file name" [m]

checkUpToDate clo t_file ti_file c_file h_file imps
  | shortcut clo        = do ti_exists <- Directory.doesFileExist ti_file
                             c_exists  <- Directory.doesFileExist c_file
                             h_exists  <- Directory.doesFileExist h_file
                             if not ti_exists || not c_exists || not h_exists then 
                                 return False 
                              else do
                                 t_time  <- Directory.getModificationTime t_file
                                 ti_time <- Directory.getModificationTime ti_file
                                 c_time  <- Directory.getModificationTime c_file
                                 h_time  <- Directory.getModificationTime h_file
                                 ti_OKs <- mapM (tiOK ti_time) imps
                                 return (t_time < ti_time && t_time < c_time && t_time < h_time && and ti_OKs)
  | otherwise           = return False
  where tiOK ti_time1 n = do let ti_file = modToPath (str n) ++ ".ti"
                             ti_exists <- Directory.doesFileExist ti_file
                             if (not ti_exists) then
                                return True  -- library module
                               else do ti_time <- Directory.getModificationTime ti_file
                                       return (ti_time <= ti_time1)
------------------------------------------------------------------------------

main                = do args <- getArgs
                         main2 args

-- | We have the second entry point so ghci/hugs users can call
-- | main2 directly with arguments.

main2 args          = do (clo, files) <- Exception.catchDyn (cmdLineOpts args)
                                         fatalErrorHandler
                         cfg          <- Exception.catchDyn (readCfg clo)
                                         fatalErrorHandler

                         let t_files  = filter (".t"  `isSuffixOf`) files
                             i_files  = filter (".ti" `isSuffixOf`) files
                             o_files  = filter (".o" `isSuffixOf`) files
                             badfiles = files \\ (t_files++i_files++o_files)
                         
                         Monad.when (not (null badfiles)) $ do
                             fail ("Bad input files: " ++ showids badfiles)
                         
                         mapM (listIface cfg) i_files
--                         Monad.when (null t_files) stopCompiler
                         
                         ps <- mapM parse t_files
                         ifs <- compileAll clo [] ps `catchException` handleError
                         Monad.when (stopAtC clo) stopCompiler
                         
                         let root = make clo
                         Monad.when (root/="") (makeProg clo cfg root)

                         let basefiles = map (rmSuffix ".t") t_files
                             c_files   = map (++ ".c") basefiles
                         mapM (compileC cfg clo) c_files
                         Monad.when (stopAtO clo) stopCompiler

                         let basenames = map rmDirs basefiles
                             o_files'   = map (++ ".o") basenames
                         Monad.when(not (null basenames)) (do r <- checkRoot clo ifs (last basenames)
                                                              linkO cfg clo r (o_files ++ o_files'))

                         return ()

handleError (ErrorCall mess) = do
  putStr ("*** Timber compilation error ***\n"++mess++"\n")
  abortCompiler
handleError e = throw e
{-
test pass           = compileTimber clo [] "Test.t"
  where clo         = CmdLineOpts { isVerbose = False,
                                    binTarget = "a.out",
                                    target    = "Trivial",
                                    root      = "root",
                                    make      = "",
                                    shortcut  = False,
                                    stopAtC   = False,
                                    stopAtO   = False,
                                    dumpAfter = (==pass),
                                    stopAfter = const False }
-}                                      


checkRoot clo ifs def       = do if1 <- getIFile rootMod
                                 if2 <- getIFile rtsMod 
                                 let ts = tEnv if2
                                     ke = Core.ksigsOf ts
                                     ds = Core.tdefsOf ts
                                     te = Core.tsigsOf (valEnv if1)
                                 case lookup rootT ke of
                                     Nothing   -> fail ("Cannot locate RootType in module " ++ rtsMod)
                                     Just Star -> case lookup rootT ds of
                                        Nothing                -> error "Internal: checkRoot"
                                        Just (Core.DType _ t') -> checkRoot' te t'
                                        Just _                 -> checkRoot' te (Core.TId rootT)
                                     Just _    -> fail ("Bad RootType in module " ++ rtsMod)
                                         
  where rtsMod              = target clo
        (r,rootMod)         = splitQual (root clo) def
        rootN               = qName rootMod (name0 r)
        rootT               = qName rtsMod (name0 "RootType")
        checkRoot' te t0    = case [ (n,t) | (n,t) <- te, n == rootN ] of
                                [(n,t)]  -> if Core.simpleInst t t0
                                            then return n
                                            else fail ("Incorrect root type: " ++ render (pr t)++"; should be "++render(pr t0))
                                _ -> fail ("Cannot locate root " ++ (root clo) ++ " in module " ++ rootMod)
        getIFile m          = case lookup (name0 m) ifs of
                                Just ifc -> return ifc
                                Nothing -> do (ifc,_) <- decodeModule (modToPath m ++ ".ti")
                                              return ifc

------------------------------------------------------------------------------

-- | Catch internal errors, print them and then abortCompiler.
fatalErrorHandler   :: TimbercException -> IO a
fatalErrorHandler e =  do putStrLn (show e)
                          abortCompiler





-- Getting import info ---------------------------------------------------------


chaseImps                         :: (String -> IO (a,String)) -> (a -> [Name]) -> String -> [Syntax.Import] -> Map Name a -> IO (Map Name (ImportInfo a), Map Name a)
chaseImps readModule iNames suff imps ifs              
                                     = do bms <- mapM (readImport ifs) imps
                                          let newpairs = [p | (p,True) <- bms]
                                              ifs1 =  [(c,ifc) | (c,(_,_,ifc)) <- newpairs] ++ ifs
                                          (rms,ifs2) <- chaseRecursively ifs1 (map impName imps) [] (concatMap (iNames  . thd . snd) (dom bms))
                                          return (map fst bms ++ rms, ifs2)
  where readIfile ifs c              = case lookup c ifs of
                                        Just ifc -> return (ifc,False)
                                        Nothing -> do (ifc,f) <- readModule f
                                                      return (ifc,True)
                                          where f = modToPath(str c) ++ suff
        readImport ifs (Syntax.Import b c) 
                                     = do (ifc,isNew) <- readIfile ifs c
                                          return ((c,(b,True,ifc)),isNew)
        chaseRecursively ifs vs ms []= return (ms,ifs)
        chaseRecursively ifs vs ms (r : rs)
             | elem r vs             = chaseRecursively ifs vs ms rs
             | otherwise             = do (ifc,isNew)  <- readIfile ifs r
                                          chaseRecursively (if isNew then (r,ifc) : ifs else ifs) (r : vs) ((r,(False,False,ifc)) : ms) 
                                                           (rs ++ iNames ifc)
        
thd (_,_,x)                         = x

chaseIfaceFiles                      = chaseImps decodeModule impsOf ".ti"
                                     
impName (Syntax.Import b c)          = c
impNames (Syntax.Module _ is _ _)    = map impName is

chaseSyntaxFiles                     = chaseImps readSyntax impNames ".t"
  where readSyntax f                 = (do cont <- readFile f
                                           let sm = runM (parser cont)
                                           return (sm,f)) `catch` (\ e -> do  t_exists <- Directory.doesFileExist libf
                                                                              if t_exists 
                                                                                then return (Syntax.Module (name0 "") [] [] [],libf) 
                                                                                else fail ("File "++ f ++ " does not exist."))
                                                                              
           where libf                = Config.libDir ++ "/" ++ f

transImps (_,_,ifc)                  = impsOf ifc

compile_order imps                   = case topSort (impNames . thd)  imps of 
                                         Left ms  -> errorIds "Mutually recursive modules" ms
                                         Right is -> is

init_order imps                      = case topSort transImps imps of
                                         Left ms  -> errorIds "Mutually recursive modules" ms
                                         Right is -> map fst is


