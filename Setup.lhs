#! /usr/bin/env runhaskell
 
> import System.Cmd
> import System.FilePath

> import Distribution.Simple -- (defaultMainWithHooks, autoconfUserHooks)
> import Distribution.Simple.InstallDirs  (CopyDest(..))
> import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs, InstallDirs(..) )
> main = defaultMainWithHooks autoconfUserHooks { postInst = myPostInst }

> myPostInst args iflags pkg_descr lbi = do
>    let dirs = absoluteInstallDirs pkg_descr lbi NoCopyDest
>    compileTimberLibrary dirs
>    compileRTS dirs
>    return ()

> compileRTS dirs = do
>    let rtsDir = datadir dirs </> "rtsPOSIX"
>        targets = ["rts.o", "libTimber.a"]
>    mapM (\file -> system("cd " ++ rtsDir ++ " && make " ++ file)) targets
>    return ()

> compileTimberLibrary dirs = do
>    let preludeDir = datadir dirs </> "lib"
>        rtsDir = datadir dirs </> "rtsPOSIX"
>        timberc = bindir dirs </> "timberc"
>        tFiles = ["Prelude.t", "BitOps.t", "Data.Functional.List.t", "Data.Objects.Dictionary.t", "Data.Objects.Stack.t", "POSIX.t", "RandomGenerator.t"]
>    mapM (\file ->  system("cd " ++ rtsDir ++ " && pwd &&  " ++ timberc ++ " -v --api --target POSIX -c ../lib/" ++ file)) tFiles
>    return ()
