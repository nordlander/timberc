module POSIX where
    
type RootType = Env -> Class Prog

type Prog = Action

struct Env where
    exit      :: Int -> Request ()
    argv      :: Array String
    stdin     :: RFile
    stdout    :: WFile
    openR     :: String -> Request (Maybe RFile)
    openW     :: String -> Request (Maybe WFile)
    startTime :: Time
    inet      :: Internet

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

struct Socket < Closable where
    remoteHost :: Host
    remotePort :: Port
    inFile     :: RFile
    outFile    :: WFile

struct Connection < Closable where 
    established :: Action
    neterror    :: String -> Action

struct Sockets where
    connect :: Host -> Port -> (Socket -> Class Connection) -> Request ()
    listen  :: Port -> (Socket -> Class Connection) -> Request Closable

instance showHost :: Show Host
showHost = struct
    show (Host nm) = nm
