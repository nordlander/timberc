module POSIX2 where
    
type RootType = Env -> Class Prog

type Prog = Action

struct File where
    close :: Request ()
    seek  :: Int -> Request Int
--    bufsize :: Int -> Request Int
    
struct RFile < File where
    read  :: Request String
    
struct WFile < File where
    write :: String -> Request Int
   
struct Env where
    exit     :: Int -> Request ()
    argv     :: [String]
    stdin    :: RFile
    stdout   :: WFile
    openR    :: String -> Request (Maybe RFile)
    openW    :: String -> Request (Maybe WFile)
    installR :: RFile -> Action -> Request ()
    installW :: WFile -> Action -> Request ()

{-
root env = 
    class
        s := "!"
        bla = action
                  x <- env.stdin.read
                  env.stdout.write (x ++ s)
                  s := '!':s
        result action
                 env.installR env.stdin bla


root env = class result action
             x <- env.stdin.read
             env.stdout.write "Hello world!"
-}