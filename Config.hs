--------------------------------------------------------------------------
--
-- Compiler configuration
--
-- This module contains both what GHC calls "dynamic flags", flags that
-- can change between each compilation, and static flags. Initial idea
-- from GHC, slightly modified to fit our needs.
--
---------------------------------------------------------------------------

module Config (
               -- Our Data from files/flags.
               CfgOpts(..),
               CmdLineOpts(..),
               
               -- Read out configuration from file.
               readCfg,
               -- Query about our options from flags.
               cmdLineOpts,
                          
               -- Path of main RTS file.
               rtsFile,
                      
               -- Pass data. XXX Does not belong here?
               Pass(..),
               allPasses,
                        
               -- Dynamic Exceptions
               TimbercException(..),
               ) where


import Char
import System.Console.GetOpt
import System                 ( getArgs, getEnv, getProgName )
import qualified Control.Exception as Exception ( throwDyn, Exception )
import Data.Dynamic

-- Timber Compiler
import CompileConfig          ( timberRoot )

-- | Contains the configuration for the compiler with the current
-- command line switches.
data CfgOpts         = CfgOpts { defaultCompiler :: FilePath,
                                 compilerFlags   :: FilePath,
                                 includePath     :: String,
                                 rootDir        :: FilePath
                               } deriving (Show, Eq, Read)

-- | Command line options.
data CmdLineOpts     = CmdLineOpts { isVerbose :: Bool,
                                     binTarget :: String,
                                     cfgDir    :: String,
                                     target    :: String,
                                     doGc      :: Bool,
                                     rootFun   :: String,
                                     stopAtC   :: Bool,
                                     stopAtO   :: Bool,
                                     dumpAfter :: Pass -> Bool,
                                     stopAfter :: Pass -> Bool
                                   }

options              :: [OptDescr Flag]
options              = [ Option ['v'] 
                                ["verbose"] 
                                (NoArg Verbose)
                                "be verbose",
                         Option ['o'] 
                                ["output"]  
                                (ReqArg BinTarget "TARGET")     
                                "binary target",
                         Option []    
                                ["cfg"]
                                (ReqArg TimberCfg "Config DIR")
                                "Config directory of compiler",
                         Option []
                                ["target"]
                                (ReqArg Target "Target")
                                "Target architecture",
                         Option []
                                ["enable-gc"]
                                (NoArg DoGc)
                                "Enable garbage collector",
                         Option []
                                ["root"]
                                (ReqArg RootFun "[[File:]Module.]function")
                                "Root function",
                         Option ['C'] 
                                ["stop-at-c"]
                                (NoArg StopAtC)
                                "stop compile at .c file",
                         Option ['c'] 
                                ["stop-at-o"] 
                                (NoArg StopAtO)
                                "stop compile at .o file" 
                       ]
                       ++ 
                       [ Option []
                                ["ddump-" ++ map Char.toLower (show pass)]
                                (NoArg $ DumpAfter pass)
                                ("dump " ++ show pass ++ " output to stdout")
                       | pass <- allPasses 
                       ] 
                       ++ 
                       [ Option []
                                ["stop-after-" ++ map Char.toLower (show pass)]
                                (NoArg $ StopAfter pass)
                                ("dump " ++ show pass ++ " output to stdout")
                       | pass <- allPasses 
                       ]


data Flag            = Verbose
                     | Version
                     | BinTarget String
                     | TimberCfg String
                     | Target String
                     | DoGc
                     | RootFun String
                     | StopAtC
                     | StopAtO
                     | DumpAfter Pass
                     | StopAfter Pass
                     deriving (Show,Eq)

data TimbercException
  = CmdLineError String  -- User did something wrong with command line options.
  | Panic String         -- Unexpected faults in timberc, panic.
  deriving (Eq)

instance Typeable TimbercException where
  typeOf _ = mkTyConApp (mkTyCon "TimbercException") []

-- | Let us show our exceptions as expected.
instance Show TimbercException where
  showsPrec _ (CmdLineError s) = showString s
  showsPrec _ (Panic s)        = showString $
                                 "Panic! Please file a bug report!\n\n" ++ s


cmdLineOpts          :: [String] -> IO (CmdLineOpts,[String])
cmdLineOpts args     = case getOpt Permute options args of
                         (flags,n,[])   -> do opts <- mkCmdLineOpts flags
                                              return (opts,n)
                         (_,_,errs)     -> do pgm <- getProgName
                                              Exception.throwDyn (emsg pgm errs)
  where header otc   = "Usage: " ++ otc ++ " [OPTION...] files..."
        emsg pgm errs = (CmdLineError
                         (concat errs ++ usageInfo (header pgm) options))


-- CmdLineOpts is a set of functions that can be queried
-- about the command line options.

mkCmdLineOpts        :: [Flag] -> IO CmdLineOpts
mkCmdLineOpts flags  =  do cfg <- System.getEnv "TIMBER_CFG" `catch`
                                  (\ _ -> return "")
                           return $ CmdLineOpts 
                                  { isVerbose = find Verbose,
                                    binTarget = first "a.out"
                                                [ target | (BinTarget target) <- flags ],
                                    cfgDir    = first cfg
                                                [ target | (TimberCfg target) <- flags ],
                                    target    = first "default"
                                                [ target | (Target target) <- flags ],
                                    doGc      = find DoGc,
                                    rootFun   = first "root"
                                                [ target | (Target target) <- flags ],
                                    stopAtC   = find StopAtC,
                                    stopAtO   = find StopAtO,
                                    dumpAfter = find . DumpAfter,
                                    stopAfter = find . StopAfter
                                  }
  where 
    first def []     = def
    first def (opt:_)= opt
    find opt         = first False [ True | flag <- flags, flag == opt ]





-- | Reads a configuration file in the format of CfgOpts.
readCfg clo
    = if null (cfgDir clo) then
        parseCfg $ timberRoot ++ "/etc/timberc-" ++ target clo ++ ".cfg"
      else
        parseCfg $ cfgDir clo ++ "/timberc-" ++ target clo ++ ".cfg"

-- | Internal help routine for readCfg.
parseCfg file
    = do
      txt <- catch (readFile file)
             (\e -> Exception.throwDyn $ emsg file)
      config <- safeRead file txt
      return config
    where        
        safeRead :: FilePath -> String -> IO CfgOpts
        safeRead file text =
            let val = case [x | (x,t) <- reads text, ("","") <- lex t] of
                      [x] -> x
                      [] -> error ("Parse error in " ++ file ++ "\n")
                      _ -> error ("File not found: " ++ file ++ "\n")
                in (return $! val)
        emsg f = (CmdLineError $ "Could not open " ++ file)


-- | The different compiler passes.
-- XXX Should not be in this file.
data Pass            = Parser
                     | Expand
                     | Desugar
                     | S2C
                     | KCheck
                     | TCheck
                     | Termred
                     | C2K
                     | Clos
                     | LLift
                     | Coll
                     | Elim
                     | K2C
                     | Main                  -- top level 'pass'
                     deriving (Show,Eq,Ord,Enum)
                        
allPasses            :: [Pass]
allPasses            = [Parser .. K2C]

rtsFile cfg          = rtsDir cfg ++ "/rts.c"

rtsDir cfg           = rootDir cfg ++ "/rts"

