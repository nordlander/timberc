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
                          
               -- Configurable paths
               libDir,
               includeDir,
               rtsDir,
               rtsCfg,
               rtsMain,
               
               -- Print usage info and exit.
               helpMsg,
                      
               -- Pass data. XXX Does not belong here?
               Pass(..),
               allPasses,
                        
               -- Dynamic Exceptions
               TimbercException(..),
               ) where


import Char
import System.IO.Unsafe ( unsafePerformIO )
import System.Console.GetOpt
import System                 ( getArgs, getEnv, getProgName )
import qualified Control.Exception as Exception ( throwIO, Exception )
import Data.Dynamic

-- Cabal Configuration
import Paths_timberc

-- | Contains the configuration for the compiler with the current
-- command line switches.
data CfgOpts         = CfgOpts { cCompiler       :: FilePath,
                                 compileFlags    :: String,
                                 linkFlags       :: String
                               } deriving (Show, Eq, Read)

-- | Command line options.
data CmdLineOpts     = CmdLineOpts { isVerbose :: Bool,
                                     datadir   :: FilePath,
                                     target    :: String,
                                     outfile   :: FilePath,
                                     root      :: String,
                                     make      :: String,
                                     api       :: Bool,
                                     pager     :: FilePath,
                                     shortcut  :: Bool,
                                     stopAtC   :: Bool,
                                     stopAtO   :: Bool,
                                     dumpAfter :: Pass -> Bool,
                                     stopAfter :: Pass -> Bool
                                   }


options              :: [OptDescr Flag]
options              = [ Option []
                                ["help"]
                                (NoArg Help)
                                "Show this message",
                         Option [] 
                                ["version"] 
                                (NoArg Version)
                                "Print version information",
                         Option [] 
                                ["print-datadir"] 
                                (NoArg PrintDatadir)
                                "Print effective datadir path",
                         Option ['v'] 
                                ["verbose"] 
                                (NoArg Verbose)
                                "Be verbose",
                         Option []
                                ["datadir"]
                                (ReqArg Datadir "DIRECTORY")
                                "Path to installed libraries and run-time systems",
                         Option []
                                ["target"]
                                (ReqArg Target "TARGET")
                                "Target platform",
                         Option ['o'] 
                                ["output"]  
                                (ReqArg Outfile "FILE")     
                                "Name of executable output",
                         Option []
                                ["root"]
                                (ReqArg Root "[MODULE.]NAME")
                                "Define root of executable program",
                         Option []
                                ["make"]
                                (ReqArg Make "[MODULE.]NAME")
                                "Make program with given root module",
                         Option []
                                ["api"]
                                (NoArg Api)
                                "Produce html file with API",
                         Option [] 
                                ["pager"] 
                                (ReqArg Pager "PATH")
                                "Use command PATH to display .html files",
                         Option ['s']
                                ["shortcut"]
                                (NoArg ShortCut)
                                "Try to reuse existing .c, .h and .ti files",
                         Option ['C'] 
                                ["stop-at-c"]
                                (NoArg StopAtC)
                                "Stop compiler after .c file generation",
                         Option ['c'] 
                                ["stop-at-o"] 
                                (NoArg StopAtO)
                                "Stop compiler after .o file generation"
                       ]
                       ++ 
                       [ Option []
                                ["dump-" ++ map Char.toLower (show pass)]
                                (NoArg $ DumpAfter pass)
                                ("Dump " ++ show pass ++ " output to stdout")
                       | pass <- allPasses 
                       ] 
                       ++ 
                       [ Option []
                                ["stop-after-" ++ map Char.toLower (show pass)]
                                (NoArg $ StopAfter pass)
                                ("Stop compiler after pass " ++ show pass)
                       | pass <- allPasses 
                       ]
                       


data Flag            = Help
                     | Verbose
                     | Version
                     | PrintDatadir
                     | Datadir String
                     | Target String
                     | Outfile String
                     | Root String
                     | Make String
                     | Api
                     | Pager String
                     | ShortCut
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

instance Exception.Exception TimbercException

cmdLineOpts          :: [String] -> IO (CmdLineOpts,[String])
cmdLineOpts args     = do globalCfgFile <- System.getEnv "TIMBER_CFG" `catch` (\_ -> return "/etc/timberc")
                          globalCfg <- readFile globalCfgFile `catch` (\_ -> return "")
                          pager <- System.getEnv "PAGER" `catch` (\_ -> return "")
                          let pagerCfg = if null pager then "" else "--pager " ++ pager
                          userCfgFile <- System.getEnv "HOME" `catch` (\_ -> return "")
                          userCfg <- readFile (userCfgFile ++ "/.timberc") `catch` (\_ -> return "")
                          case getOpt Permute options (args ++ words userCfg ++ words pagerCfg ++ words globalCfg) of
                            (flags,n,[]) 
                              | Help `elem` flags -> do 
                                  msg <- helpMsg
                                  Exception.throwIO (CmdLineError msg)
                              | Version `elem` flags ->
                                  Exception.throwIO (CmdLineError "version...?")
                              | PrintDatadir `elem` flags ->
                                  Exception.throwIO (CmdLineError (datadir (mkCmdLineOpts flags)))
                              | otherwise -> 
                                  return (mkCmdLineOpts flags, n)
                            (_,_,errs) -> do 
                                  msg <- helpMsg
                                  Exception.throwIO (CmdLineError (concat errs ++ msg))


helpMsg              = do pgm <- getProgName
                          return (usageInfo (header pgm) options)
  where header pgm   = "Usage: " ++ pgm ++ " [OPTION...] files..."
                         


-- CmdLineOpts is a set of functions that can be queried
-- about the command line options.

mkCmdLineOpts        :: [Flag] -> CmdLineOpts
mkCmdLineOpts flags  =  CmdLineOpts { isVerbose = find Verbose,
-- XXXpj: This will *NOT* work on Windows. We need to enter IO.
                                      datadir   = first (unsafePerformIO getDataDir)
                                                  [ dir | (Datadir dir) <- flags ],
                                      target    = first "POSIX"
                                                  [ target | (Target target) <- flags ],
                                      outfile   = first "a.out"
                                                  [ file | (Outfile file) <- flags ],
                                      root      = first "root"
                                                  [ root | (Root root) <- flags ],
                                      make      = first ""
                                                  [ root | Make root <- flags ],
                                      api       = find Api,
                                      pager     = first "less"
                                                  [ p | (Pager p) <- flags ],
                                      shortcut  = find ShortCut,
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
readCfg clo = parseCfg (rtsCfg clo)


-- | Internal help routine for readCfg.
parseCfg file
    = do
      txt <- catch (readFile file)
             (\e -> Exception.throwIO $ emsg file)
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
                     | Desugar1
                     | Rename
                     | Desugar2
                     | S2C
                     | KCheck
                     | TCheck
                     | Termred
                     | Type2
                     | C2K
                     | Kindlered
                     | LLift
                     | Prepare4C
                     | K2C
                     | Main                  -- top level 'pass'
                     deriving (Show,Eq,Ord,Enum)
                        
allPasses            :: [Pass]
allPasses            = [Parser .. K2C]


rtsDir clo           = datadir clo ++ "/rts" ++ target clo
rtsCfg clo           = rtsDir clo ++ "/timberc.cfg"
rtsMain clo          = rtsDir clo ++ "/main.c"
libDir clo           = datadir clo ++ "/lib"
includeDir clo       = datadir clo ++ "/include"
