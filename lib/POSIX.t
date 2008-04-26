module POSIX where
    
type RootType = Env -> Class Prog 

data AccessMode = Read | Write

struct File where
    read  :: Request String
    write :: String -> Request String
    
struct Env where
    argv   :: [String]
    stdin  :: File
    stdout :: File
    exit   :: Int -> Request ()
    open   :: String -> AccessMode -> Request File
 
struct Prog where
    start :: Action
    io    :: Action


