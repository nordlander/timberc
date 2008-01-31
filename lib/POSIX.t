module POSIX where
    
type RootType = Env -> Class Prog 

struct File where
    read  :: Request String
    write :: String -> Request String
    
struct Env where
    argv   :: [String]
    stdin  :: File
    stdout :: File
    exit   :: Int -> Request ()

struct Prog where
    start :: Action
    io    :: Action
