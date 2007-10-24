module POSIX where
    
type RootType = Env -> Template Prog

record Env =
    argv   :: [String]
    output :: String -> Request ()
    exit   :: Int -> Request ()

record Prog =
    input :: String -> Action
