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

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)
import System.Environment
import Network.Socket hiding (shutdown, accept, send)
import Language.Haskell.TH

type ServerData = (TVar (Store Handle), TChan Update) -- TODO: Datatype maken
data LeaderData = LeaderData
    { servers   :: TVar [ProcessId]
    , workers   :: TVar (Map ProcessId (TVar [Handle]))
    , requests  :: TVar (Map Text ProcessId)
    , proxychan :: TChan (Process ())
    }
    
masterPort :: Int
masterPort = 44414

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

leaderListener :: LeaderData -> IO ()
leaderListener ld = withSocketsDo $ do
    sock <- listenOn (PortNumber (fromIntegral masterPort))
    putStrLn ("Leader listening on port " ++ show masterPort)
    forever $ do
        (handle, host, port) <- accept sock
        putStrLn ("Accepted client connection from: " ++ show handle)        
        forkIO (setupClient ld handle)
        
setupClient :: LeaderData -> Handle -> IO ()
setupClient ld h = do
    NotifyLeader name <- retryRead h
    putStrLn ("Name recieved from: " ++ unpack name)
    atomically $ do
      reqMap <- readTVar $ requests ld
      case M.lookup name reqMap of
          Nothing -> retry
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
    
handleServer :: LeaderData -> ProcessId -> Process ()
handleServer ld pid = do
    hostName <- liftIO $ getHostName
    send pid hostName
    (sendPort, recvPort) <- newChan
    send pid sendPort
    forever $ receiveChan recvPort >>= handleLeaderMessages ld      
    
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
       
talk :: ServerData -> Handle -> IO ()
talk sd h = do 
    hSetNewlineMode h universalNewlineMode
    hSetBuffering h LineBuffering
    runConnection sd h $ serverConnection h
    
runConnection :: ServerData -> Handle -> ConnectionM Handle a -> IO a
runConnection (st, pc) h (Pure a) = return a
runConnection (st, pc) h (Free f) = run f where
  continue c = runConnection (st, pc) h c

  run (ReceiveAny c) = do
    hPutStrLn h "Enter a command"
    command <- retryRead h
    continue $ c $ Left command

  run (PutState newSt c) = do
    atomically $ writeTVar st newSt    
    continue c

  run (GetState c) = do
    store <- atomically $ readTVar st
    continue (c store)
    
  run (AtomicModifyState f c) = do 
        atomically $ do
            store <- readTVar st
            let store' = f store
            writeTVar st store'
        continue c

  run (SendTo addr event c) = do
    hPutStrLn addr $ "To " ++ show addr ++ ": " ++ show event
    continue c

  run (Reply event c) = do
    hPutStrLn h $ "Message to you: " ++ show event
    continue c

retryRead :: Read a => Handle -> IO a
retryRead h =
  do command <- hGetLine h
     let parse = map fst . filter (\(_, remaining) -> remaining == "") . reads
     case parse command of
       [x]  ->  return x
       _    ->  hPutStrLn h "Parse error" >> retryRead h

handleRemoteMessage :: TVar (Store Handle) -> Action -> Process ()
handleRemoteMessage st (Put path newValue) = liftIO $ atomically $ do
    store <- readTVar st
    let newStore = updateStore store path newValue
    writeTVar st newStore       
       
socketListener :: ServerData -> Int -> ProcessId -> HostName -> IO ()
socketListener sd@(_, pc) p pid lHostName = withSocketsDo $ do
    sock <- listenOn (PortNumber (fromIntegral p))
    putStrLn ("Listening on port " ++ show p)
    forever $ do
        (handle, host, port) <- accept sock
        putStrLn ("Accepted connection from: " ++ show handle)
        let name = show pid ++ show port
        atomically $ writeTChan pc $ NewClient pid $ pack name
        hPutStrLn handle ("Connect to the leader located at " ++ show lHostName ++ " at port " ++ show masterPort ++ " and send 'NotifyLeader { name = \"" ++ name ++ "\" }'.")
        forkFinally (talk sd handle) (\_ -> hClose handle)       
        
server :: (Int, ProcessId) -> Process ()
server (port, lpid) = do 
    st <- liftIO $ newTVarIO emptyStore
    pc <- liftIO $ newTChanIO
    pid <- getSelfPid
    lHostName <- expect
    liftIO $ forkIO $ socketListener (st, pc) port pid lHostName
    mySendPort <- spawnChannelLocal $ serverProxy pc
    sendPort <- expect
    sendChan mySendPort sendPort
    forever $ do m <- expect; handleRemoteMessage st m
    
serverProxy :: Serializable a => TChan a -> ReceivePort (SendPort a) -> Process ()
serverProxy pc recvPort = do
    sendPort <- receiveChan recvPort
    forever $ do 
      msg <- liftIO $ atomically $ readTChan pc
      sendChan sendPort msg
    
remotable ['server]
       
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
    --pids <- mapM (flip spawn ($(mkClosure 'server) args)) peers
    leader pids
       
main :: IO ()
main = distribMain master __remoteTable

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
