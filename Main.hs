-- -*- haskell-hugs-program-args: ("-98" "+.") -*-
module Main where

-- Haskell98
import System(getArgs)
import List(isSuffixOf)
import qualified Monad
import qualified Char
import qualified Control.Exception as Exception ( catchDyn, catch )
import System.Console.GetOpt

-- Timber Compiler
import Config
import Execution
import PP
import Common
import Parser
import Desugar1
import Rename
import Desugar2
import Syntax2Core
import Kind
import Type
import Termred
import Kindle
import Core2Kindle
--import Clos
--import Lambdalift
--import Collect
--import Eliminate
import Kindle2C

{-

Lexer:
- Checks lexical syntax and builds token stream
- Removes comments

Parser:
- Checks basic syntax and builds parse tree
- Builds constructor syntax (pared as qualified types)
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

- Does dependency analysis on bindings and declarations
- Builds Core syntax tree

Kind:
- Performs kind inference, replacing every kind unification variable (defaulting to *)

Decls:
- Extracts selector and constructor type schemes
- Generates global member signatures and bindings
- Replaces subtyping in type declarations with explicit coercion constructors/selectors

- Initiate subtyping graphs with reflexivity axioms (identity witness)
- Computes subtype above & below graph, closed under transitivity (checks cyclic and ambiguity errors)
- Computes instance overlap graph, closed under superclass relation (checks cyclic and ambiguity errors)

Type:
- Performs type inference and constraint simplification/solving
- Inserts explicit overloading and subtyping witnesses

Termred:
- Does simple term level reduction and restricted inlining within primitive environment (witnesses only)

Core2Kindle:
- Converts symbolic names to "sym_NN" syntax
- Converts div, mod, negate, not to C/Kindle symbols
- Replaces ' with _ in variable names
- Translates Int, Char, Float typenames to C/Kindle counterparts

- Converts type expressions by making function arities and closure types explicit
- Approximates polymorphism by wildcard type (temporary solution)

- Encodes monadic code by means of extra (mutable) state argument

- Sorts equations into value or function definitions, on basis of arity
- Makes fatbar and fail pattern-matching operators explicit, removes match and commit
- Replaces aft and bef primitive constants by side-effecting rts calls
- Replaces act, req and templ by rts calls
- Singles out "hard" expression atoms as arity 0 functions
- Splits case expressions into literal and constructor variants

- Builds Kindle abstract syntax

Clos:
- Eta-expands and splits argument lists as necessary to make arities match
- Replaces anonymous abstractions with closure terms
- Converts function calls to closure enter terms as guided by the closure type
- Inserts explicit typecast terms

LambdaLift:
- Abstracts out all free local variables from function definitions and closure constructions
- Adds free variable parameters to function calls and closures

Collect:
- Collects all closure types referenced in the program and generates corresponding Kindle classes
- Collects all types at which classes are instantiated (and currently drops them)

Eliminate:
- Moves local function definitions to the top level
- Replaces closure types with corresponding class names (deterministically generated)
- Replaces closure terms with corresponding class instantiations and custom "enter" method
- Replaces closure enter with "enter" method calls

Kindle2C:
- Classifies Kindle unions and classes according to their number of fields
- Makes initialization assignments explicit at object allocation time
- Shotrcuts data representation for nullary constructors
- Generates custom "main" function
- Pretty-prints abstract syntax as C code


Yet unknown:
- Prepend module name to global names (terms, types, constructors, selectors, generated *witnesses*)

-}


-- | This compiles a set of Timber modules. 
-- | Example: 'compile ["Main"] compiles the module in Main.t
-- | leaving the result in Main.c
-- | It also compiles every module *called* by this module 
-- | right now.


compileTimber clo f = do putStrLn $ "[loading module " ++ show f ++ "]"
                         txt <-  catch (readFile f)
                                 (\e -> error $ "File " ++ f ++ " does not exist.")
                         let (htxt, mtxt) = runM (passes txt)
                         writeFile (rmSuffix ".t" f ++ ".c") mtxt
                         -- XXX Make the filename static for now. Will break with modules.
                         --- writeFile (rmSuffix ".t" f ++ ".h") htxt
                         writeFile "defs.h" htxt

  where passes txt  = do par <- pass parser         Parser              txt
                         d1  <- pass desugar1       Desugar1            par
                         rn  <- pass renameM        Rename              d1
                         d2  <- pass desugar2       Desugar2            rn
                         co  <- pass syntax2core    S2C                 d2
                         kc  <- pass kindcheck      KCheck              co
                         tc  <- pass typecheck      TCheck              kc
                         rd  <- pass termred        Termred             tc
                         ki  <- pass core2kindle    C2K                 rd
{-
                         cl  <- pass clos           Clos                ki
                         ll  <- pass lambdalift     LLift               cl
                         ct  <- pass collect        Coll                ll
                         el  <- pass eliminate      Elim                ct
                         c   <- pass kindle2c       K2C                 el
                         return c
-}
			 return undefined

        pass        :: (Pr b) => (a -> M b) -> Pass -> a -> M b
        pass m p a  = do -- tr ("Pass " ++ show p ++ "...")
                        r <- m a
                        Monad.when (dumpAfter clo p) 
                                 $ fail ("#### Result after " ++ show p ++ ":\n\n" ++ render (pr r))
                        if stopAfter clo p
                           then fail ("#### Terminated after " ++ show p ++ ".")
                           else return r                                  



------------------------------------------------------------------------------

main                = do args <- getArgs
                         main2 args

-- | We have the second entry point so ghci/hugs users can call
-- | main2 directly with arguments.
-- | This replaces any perl driver.

main2 args          = do (clo, files) <- Exception.catchDyn (cmdLineOpts args)
                                         fatalErrorHandler
                         cfg          <- Exception.catchDyn (readCfg clo)
                                         fatalErrorHandler

                         let timber_files = [ file | file <- files, ".t" `isSuffixOf` file ]
                         Monad.when (null timber_files) (do
                                                         putStrLn "No files to compile found."
                                                         abortCompiler)
                         mapM (compileTimber clo) timber_files
                         Monad.when (stopAtC clo) stopCompiler
                         let c_modules = [ rmSuffix ".c" file | file <- files, ".c" `isSuffixOf` file ]

                         -- all the timber modules should have produced c modules
                         let all_modules = map t2mName timber_files ++ c_modules
                         mapM_ (compileC cfg clo) all_modules
                         Monad.when (stopAtO clo) stopCompiler

                         -- finally, perform the last link
                         linkO cfg clo all_modules
                         return ()



test pass           = compileTimber clo "Test.t"
  where clo         = CmdLineOpts { isVerbose = False,
                                    binTarget = "a.out",
                                    cfgDir    = "../etc",
                                    target    = "default",
                                    doGc      = False,
                                    rootFun   = "root",
                                    stopAtC   = False,
                                    stopAtO   = False,
                                    dumpAfter = f,
                                    stopAfter = const False }
        f p             = p == pass
                                        

t2mName nm 
 | ".t" `isSuffixOf` nm = rmSuffix ".t" nm
 | otherwise            = error $ "bad timber file name: " ++ nm


------------------------------------------------------------------------------

-- | Catch internal errors, print them and then abortCompiler.
fatalErrorHandler   :: TimbercException -> IO a
fatalErrorHandler e =  do putStrLn (show e)
                          abortCompiler





