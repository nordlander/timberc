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
                           let cmd = defaultCompiler cfg ++
                                     (if (doGc clo)
                                      then " -DENABLE_GC "
                                      else "")
                                     ++ " -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast "
                                     ++ compilerFlags cfg ++ includePath cfg
                                     ++ cfile mod 
                           execCmd clo cmd

-- | Link together a bunch of object files.
linkO cfg clo r mods  = do let ofile file = file ++ ".o"
                               Just rmod  = fromMod r
                               initId     = "_init_" ++ map f rmod
                               rootId     = name2str r
                               f '/'      = '_'
                               f x        = x
                           let cmd = defaultCompiler cfg ++
                                     (if (doGc clo)
                                      then " -DENABLE_GC "
                                      else "")
                                     ++ includePath cfg ++  " -o "
                                     ++ binTarget clo ++ " "
                                     ++ unwords (map ofile mods) ++ " "
                                     ++ " -DROOT=" ++ rootId ++ " "
                                     ++ " -DROOTINIT=" ++ initId ++ " "
                                     ++ rtsDir cfg clo ++ "/main.c "
                                     ++ rtsDir cfg clo ++ "/rts.o "
                                     ++ "Prelude.o"
                           execCmd clo cmd


-- | Return with exit code /= 0
abortCompiler         = exitWith (ExitFailure 1)

-- | Return with exit code == 0
stopCompiler          = exitWith (ExitSuccess)

execCmd opt cmd       = do Monad.when (isVerbose opt)
                                    (putStrLn ("exec: " ++ show cmd))
                           exitCode <- system $ cmd     
                           case exitCode of
                             ExitSuccess -> return ()
                             _           -> stopCompiler

