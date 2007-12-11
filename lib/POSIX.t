module POSIX where
    
type RootType = Env -> Template Prog

record File =
    read  :: Request String
    write :: String -> Request String
    
record Env =
    argv   :: [String]
    stdin  :: File
    stdout :: File
    exit   :: Int -> Request ()

record Prog =
    start :: Action
    io    :: Action
