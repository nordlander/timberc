module Static where


import Pretty
import Common
import Core
import Env
import Kind

{-
static (Module i ds is bss)             = do env <- kiDecls primKE ds
                                             return (env, bss)
-}