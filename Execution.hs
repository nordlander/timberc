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
compileC cfg clo mod  = do let cfile file = file ++ ".c"
                           let cmd = cCompiler cfg
                                     ++ compileFlags cfg
                                     ++ " -I " ++ libDir ++ " " 
                                     ++ cfile mod 
                           execCmd clo cmd

-- | Link together a bunch of object files.
linkO cfg clo r mods  = do let ofile file = file ++ ".o"
                               Just rmod  = fromMod r
                               rootId     = name2str r
                               initId     = "_init_" ++ map f rmod
                               f '\''     = '_'
                               f c        = c
                           let cmd = cCompiler cfg
                                     ++ linkFlags cfg
                                     ++ " -I" ++ libDir ++ " " 
                                     ++ " -L" ++ rtsDir clo ++ " " 
                                     ++ " -o " ++ binTarget clo ++ " "
                                     ++ unwords (map ofile mods) ++ " "
                                     ++ " -DROOT=" ++ rootId ++ " "
                                     ++ " -DROOTINIT=" ++ initId ++ " "
                                     ++ rtsDir clo ++ "/main.c "
--                                     ++ rtsDir clo ++ "/rts.o "         -- include in libTimber...
--                                     ++ libDir ++ "/Prelude.o "         -- include in libTimber...
--                                     ++ libDir ++ "/Trivial.o "         -- include in libTimber...
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

