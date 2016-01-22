{-# LANGUAGE TemplateHaskell #-}
module Network.TimeKeeper.Distributed where

import Network.TimeKeeper.Server
import Network.TimeKeeper.Protocol
import Control.Monad.Free

import Data.Text (pack)

import Data.IORef

import Control.Concurrent
import Control.Concurrent.STM
import Network
import System.IO
import Control.Monad

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Static hiding (initRemoteTable)
import System.Environment
import Network.Socket hiding (shutdown, accept, send)

type ServerData = (TVar (Store Handle), TChan (Action))
       
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
       
socketListener :: ServerData -> Int -> IO ()
socketListener sd port = withSocketsDo $ do
    sock <- listenOn (PortNumber (fromIntegral port))
    putStrLn ("Listening on port " ++ show port)
    forever $ do
        (handle, host, port) <- accept sock
        putStrLn ("Accepted connection from: " ++ show handle)
        forkFinally (talk sd handle) (\_ -> hClose handle)       
        
server :: (Int, ProcessId) -> Process ()
server (port, mpid) = do
    st <- liftIO $ newTVarIO emptyStore
    pc <- liftIO $ newTChanIO
    liftIO $ forkIO $ socketListener (st, pc) port
    spawnLocal $ forever $ (liftIO $ atomically $ readTChan pc) >>= send mpid
    forever $ do m <- expect; handleRemoteMessage st m
    
remotable ['server]

masterPort :: Int
masterPort = 44444
       
master :: [NodeId] -> Process ()
master peers = do
    pid <- getSelfPid
    nid <- getSelfNode
    spawn nid ($(mkClosure 'server) (masterPort, pid))
    return ()
       
main :: IO ()
main = distribMain master Main.__remoteTable

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