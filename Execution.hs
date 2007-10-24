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

-- Timber compiler
import Config
import Name


-- | Compile a C-file. 
compileC cfg clo c_file = do let cmd = cCompiler cfg
                                     ++ compileFlags cfg
                                     ++ " -I " ++ includeDir ++ " " 
                                     ++ " -I " ++ rtsDir clo ++ " " 
                                     ++ c_file
                             execCmd clo cmd

-- | Link together a bunch of object files.
linkO cfg clo r o_files = do let Just rmod  = fromMod r
                                 rootId     = name2str r
                                 initId     = "_init_" ++ map f rmod
                                 f '\''     = '_'
                                 f c        = c
                             let cmd = cCompiler cfg
                                       ++ linkFlags cfg
                                       ++ " -o " ++ binTarget clo ++ " "
                                       ++ unwords o_files ++ " "
                                       ++ " -L" ++ rtsDir clo ++ " " 
                                       ++ " -I" ++ includeDir ++ " " 
                                       ++ " -I " ++ rtsDir clo ++ " " 
                                       ++ " -DROOT=" ++ rootId ++ " "
                                       ++ " -DROOTINIT=" ++ initId ++ " "
                                       ++ rtsDir clo ++ "/main.c "
                                       ++ " -lTimber "
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

