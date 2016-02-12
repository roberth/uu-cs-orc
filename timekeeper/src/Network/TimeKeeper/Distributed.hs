{-# LANGUAGE TemplateHaskell #-}
module Network.TimeKeeper.Distributed where

import Network.TimeKeeper.Server
import Network.TimeKeeper.Protocol
import Control.Monad.Free

import Data.Text hiding (filter, map, zip, replicate, length)

import Data.IORef

import Control.Concurrent hiding (newChan)
import Control.Concurrent.STM
import Network
import Network.BSD
import System.IO
import Control.Monad
import Data.Map as M hiding (filter, map)
import Data.Maybe (maybeToList)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)
import System.Environment
import Network.Socket hiding (shutdown, accept, send)
import Language.Haskell.TH


-- | Holds the data relevant to a server
data ServerData = ServerData
    { servStore :: TVar (Store Handle)  -- The Store that contains the state of the server
    , pchan     :: TChan Update         -- A proxychannel which can be used to send messages to the Leader (by a seperate Process on the local Node)
    , serverPid :: ProcessId            -- The ProccessId of the server
    }
-- | Holds the data relevant to the leader
data LeaderData = LeaderData
    { servers   :: TVar [ProcessId]                         -- List of connected Servers
    , workers   :: TVar (Map ProcessId (TVar [Handle]))     -- Map containing all clients connected to a specific Server
    , requests  :: TVar (Map Text ProcessId)                -- Clients that have connected to a server but not yet to the Leader
    , proxychan :: TChan (Process ())                       -- A proxychannel which can be used to perform actions in the Process Monad 
    }
    
masterPort :: Int
masterPort = 44444

-- ---------------------------------------------------------------------------
-- Leader Code

-- | Builds the LeaderData object and starts the leader Processes 
leader :: [ProcessId] -> Process ()
leader procs = do
    s <- liftIO $ newTVarIO procs
    w <- liftIO $ newTVarIO M.empty
    r <- liftIO $ newTVarIO M.empty
    pc <- liftIO $ newTChanIO 
    let ld = LeaderData { servers = s, workers = w, requests = r, proxychan = pc }
    mapM_ (\pid -> spawnLocal $ handleServer ld pid) procs    
    spawnLocal $ forever $ join $ liftIO $ atomically $ readTChan pc
    liftIO $ leaderListener ld

-- | Listens to incoming connections from Clients
leaderListener :: LeaderData -> IO ()
leaderListener ld = withSocketsDo $ do
    sock <- listenOn (PortNumber (fromIntegral masterPort))
    putStrLn ("Leader listening on port " ++ show masterPort)
    forever $ do
        (handle, host, port) <- accept sock
        putStrLn ("Accepted client connection from: " ++ show handle) 
        hPutStrLn handle "Hello. Do you have a message for me?"       
        forkFinally (setupClient ld handle) (\_ -> hClose handle)

-- | Adds a clients handle to the workers field, but only after its Server has confirmed the client        
setupClient :: LeaderData -> Handle -> IO ()
setupClient ld h = do
    NotifyLeader name <- retryRead h
    hPutStrLn h "Valid name recieved"
    atomically $ do
      reqMap <- readTVar $ requests ld
      case M.lookup name reqMap of
          Nothing ->
            retry
          Just pid -> do
            workerMap <- readTVar $ workers ld
            case M.lookup pid workerMap of
              Nothing -> do
                entry <- newTVar [h]
                let workerMap' = M.insert pid entry workerMap
                writeTVar (workers ld) workerMap'   
              Just handlesT -> do
                handles <- readTVar handlesT
                writeTVar handlesT $ h:handles
    setupClient ld h

-- | Send the required data to the Server, and listen to the messages it sends
handleServer :: LeaderData -> ProcessId -> Process ()
handleServer ld pid = do
    hostName <- liftIO $ getHostName
    send pid hostName
    (sendPort, recvPort) <- newChan
    send pid sendPort
    forever $ receiveChan recvPort >>= handleLeaderMessages ld      

-- | Handles a single message send by a server    
handleLeaderMessages :: LeaderData -> Update -> Process ()
handleLeaderMessages ld u = liftIO $ atomically $ do
    case u of
      PutUpdate pid p v -> do
        serverList <- readTVar $ servers ld
        mapM_ (\pid' -> if pid == pid' then return () else writeTChan (proxychan ld) (send pid' (Put p v))) serverList
      NewClient pid name -> do
        reqMap <- readTVar $ requests ld
        let reqMap' = M.insert name pid reqMap
        writeTVar (requests ld) reqMap'

-- ---------------------------------------------------------------------------
-- Server Code   
        
-- | Set some handle settings and start listening to incoming commands         
talk :: ServerData -> Handle -> IO ()
talk sd h = do 
    hSetNewlineMode h universalNewlineMode
    hSetBuffering h LineBuffering
    runConnection sd h $ serverConnection h

-- | Handles the IO part of the Server    
runConnection :: ServerData -> Handle -> ConnectionM Handle a -> IO a
runConnection sd h (Pure a) = return a
runConnection sd h (Free f) = run f where
  continue c = runConnection sd h c

  run (ReceiveAny c) = do
    hPutStrLn h "Enter a command"
    command <- retryRead h
    continue $ c $ Left command

  run (PutState newSt a c) = do
    atomically $ writeTVar (servStore sd) newSt
    case a of
      Just (Put path val) -> do 
        atomically $ writeTChan (pchan sd) $ PutUpdate (serverPid sd) path val
        continue c
      otherwise           -> continue c

  run (GetState c) = do
    store <- atomically $ readTVar (servStore sd)
    continue (c store)
    
  run (AtomicModifyState f c) = do 
        atomically $ do
            store <- readTVar (servStore sd)
            let store' = f store
            writeTVar (servStore sd) store'
        continue c

  run (SendTo addr event c) = do
    hPutStrLn addr $ "To " ++ show addr ++ ": " ++ show event
    continue c

  run (Reply event c) = do
    hPutStrLn h $ "Message to you: " ++ show event
    continue c  
    
-- | Reads and parses a command
retryRead :: Read a => Handle -> IO a
retryRead h =
  do command <- hGetLine h
     let parse = map fst . filter (\(_, remaining) -> remaining == "") . reads
     case parse command of
       [x]  ->  return x
       _    ->  hPutStrLn h ("Parse error") >> retryRead h

-- | Handles a single message received from the Leader
handleRemoteMessage :: TVar (Store Handle) -> Action -> Process ()
handleRemoteMessage st (Put path newValue) = liftIO $ do
    (store, oldValue) <- atomically $ do
      store <- readTVar st
      let newStore = updateStore store path newValue
      writeTVar st newStore
      let oldValue = M.lookup path (storeData store)
      return (newStore, oldValue)
    let subs = join $ maybeToList $ M.lookup path (storeSubscriptions store)
        message = Updated path oldValue newValue
    mapM_ (\h -> hPutStrLn h $ "To " ++ show h ++ ": " ++ show message) subs

-- | Listens to incoming connection from clients, and starts a new thread for each new connection
socketListener :: ServerData -> Int -> ProcessId -> HostName -> IO ()
socketListener sd p pid lHostName = withSocketsDo $ do
    sock <- listenOn (PortNumber (fromIntegral p))
    putStrLn ("Listening on port " ++ show p)
    forever $ do
        (handle, host, port) <- accept sock
        putStrLn ("Accepted connection from: " ++ show handle)
        let name = show pid ++ show port
        atomically $ writeTChan (pchan sd) $ NewClient pid $ pack name
        hPutStrLn handle ("Connect to the leader located at " ++ show lHostName ++ " at port " ++ show masterPort ++ " and send 'NotifyLeader { name = \"" ++ name ++ "\" }'.")
        forkFinally (talk sd handle) (\_ -> hClose handle)       

-- | Sets up the ServerData object for this server and starts up the different threads/processes that this server uses        
server :: (Int, ProcessId) -> Process ()
server (port, lpid) = do 
    st <- liftIO $ newTVarIO emptyStore
    pc <- liftIO $ newTChanIO
    pid <- getSelfPid
    lHostName <- expect
    let sd = ServerData { servStore = st, pchan = pc, serverPid = pid }
    liftIO $ forkIO $ socketListener sd port pid lHostName
    mySendPort <- spawnChannelLocal $ serverProxy pc
    sendPort <- expect
    sendChan mySendPort sendPort
    forever $ do m <- expect; handleRemoteMessage st m

-- | Passes on messages send in a TChan to the Leader   
serverProxy :: Serializable a => TChan a-> ReceivePort (SendPort a) -> Process ()
serverProxy pc recvPort = do
    sendPort <- receiveChan recvPort
    forever $ do 
      msg <- liftIO $ atomically $ readTChan pc
      sendChan sendPort msg
    
remotable ['server]

-- ---------------------------------------------------------------------------
-- Starting up the Leader and Servers

-- | Initial code that starts up a couple of Servers (one on each node, except for its own) and then starts the Leader on itself
master :: [NodeId] -> Process ()
master peers = do
    liftIO $ putStrLn "Leader Started"
    liftIO $ putStrLn (show (length peers))
    pid <- getSelfPid
    let args = zip [(masterPort+1)..] $ replicate (length peers) pid
        run nid (port, mpid) = do
          liftIO $ putStrLn ("Starting node on " ++ show nid ++ " with port " ++ show port)
          spawn nid ($(mkClosure 'server) (port, mpid))
    pids <- zipWithM run peers args
    leader pids
       
main :: IO ()
main = distribMain master __remoteTable

-- | Code taken from Simon Marlow that sets up the Cloud Haskell backend 
distribMain :: ([NodeId] -> Process ()) -> (RemoteTable -> RemoteTable) -> IO ()
distribMain master frtable = do
  args <- getArgs
  let rtable = frtable initRemoteTable

  case args of
    [] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startMaster backend master
    [ "master", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startMaster backend master
    [ "slave" ] -> do
      backend <- initializeBackend defaultHost defaultPort rtable
      startSlave backend
    [ "slave", port ] -> do
      backend <- initializeBackend defaultHost port rtable
      startSlave backend
    [ "slave", host, port ] -> do
      backend <- initializeBackend host port rtable
      startSlave backend

defaultHost = "localhost"
defaultPort = "44444"
