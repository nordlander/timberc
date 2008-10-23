module POSIX where
    
type RootType = Env -> Class Prog

type Prog = Action

struct Env where
    exit     :: Int -> Request ()
    argv     :: Array String
    stdin    :: RFile
    stdout   :: WFile
    openR    :: String -> Request (Maybe RFile)
    openW    :: String -> Request (Maybe WFile)
    getTime  :: Request Time
    inet     :: Internet

struct Closable where
    close :: Request ()

struct File < Closable where
    seek  :: Int -> Request Int
    
struct RFile < File where
    read     :: Request String
    installR :: (String -> Action) -> Request ()
    
struct WFile < File where
    write    :: String -> Request Int
    installW :: Action -> Request ()

data Host = Host String
data Port = Port Int

struct Internet where
    tcp :: Sockets

struct Destination < Closable where
    deliver   :: String -> Action    

struct Connection < Destination where
    established :: Action 
    neterror    :: String -> Action             

struct Peer < Destination where
    host :: Host
    port :: Port

struct Sockets where
    connect :: Host -> Port -> (Peer -> Class Connection) -> Request ()
    listen  :: Port -> (Peer -> Class Connection) -> Request Closable

instance showHost :: Show Host
showHost = struct
    show (Host nm) = nm
