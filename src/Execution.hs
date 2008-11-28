--------------------------------------------------------------------------
--
-- Execution control.
--
-- This module contains an interface to the backend and functions to
-- control the execution of the compiler. 
--
---------------------------------------------------------------------------

module Execution (
                  abortCompiler,
                  stopCompiler,
                  compileC,
                  linkO,
                 ) where
    
import System                 (system, exitWith, ExitCode(..))
import qualified Monad
import qualified Directory
import Common

-- Timber compiler
import Config
import Name


-- | Compile a C-file. 
compileC cfg clo c_file = do
                             ldir <- libDir
                             idir <- includeDir 
                             rdir <- rtsDir clo
                             let cmd = cCompiler cfg
                                     ++ compileFlags cfg
                                     ++ " -I " ++ ldir ++ " " 
                                     ++ " -I " ++ idir ++ " " 
                                     ++ " -I " ++ rdir ++ " " 
                                     ++ " -I . "
                                     ++ c_file
                                 o_file = rmSuffix ".c" (rmDirs c_file) ++ ".o"
                             res <- checkUpToDate c_file o_file
                             if not res then do
                                putStrLn ("[compiling "++c_file++"]")
                                execCmd clo cmd
                              else return ()
  where checkUpToDate c_file o_file
                        = do o_exists <- Directory.doesFileExist o_file
                             if not o_exists then
                                return False
                               else do
                                 c_time  <- Directory.getModificationTime c_file
                                 o_time  <- Directory.getModificationTime o_file
                                 return (c_time <= o_time)

-- | Link together a bunch of object files.
linkO cfg clo r o_files = do let Just rmod  = fromMod r
                                 rootId     = name2str r
                                 initId     = "_init_" ++ map f rmod
                                 f '\''     = '_'
                                 f c        = c
                             rdir <- rtsDir clo
                             idir <- includeDir
                             ldir <- libDir
                             let cmd = cCompiler cfg
                                       ++ linkFlags cfg
                                       ++ " -o " ++ binTarget clo ++ " "
                                       ++ unwords o_files ++ " "
                                       ++ " -L" ++ rdir ++ " " 
                                       ++ " -I" ++ idir ++ " " 
                                       ++ " -I" ++ ldir ++ " " 
                                       ++ " -I " ++ rdir ++ " " 
                                       ++ " -DROOT=" ++ rootId ++ " "
                                       ++ " -DROOTINIT=" ++ initId ++ " "
                                       ++ rdir ++ "/main.c "
                                       ++ " -lTimber"
                             putStrLn "[linking]"
                             execCmd clo cmd


-- | Return with exit code /= 0
abortCompiler         = exitWith (ExitFailure 1)

-- | Return with exit code == 0
stopCompiler          = exitWith (ExitSuccess)

execCmd clo cmd       = do Monad.when (isVerbose clo)
                                    (putStrLn ("exec: " ++ show cmd))
                           exitCode <- system $ cmd     
                           case exitCode of
                             ExitSuccess -> return ()
                             _           -> stopCompiler

