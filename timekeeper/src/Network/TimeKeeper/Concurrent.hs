module Network.TimeKeeper.Concurrent where
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

port :: Int
port = 44444

main :: IO ()
main = do
    putStrLn "Hi"
    st <- newTVarIO emptyStore
    sock <- listenOn (PortNumber (fromIntegral port))
    putStrLn ("Listening on port " ++ show port)
    forever $ do
        (handle, host, port) <- accept sock
        putStrLn ("Accepted connection from: " ++ show handle)
        forkFinally (talk st handle) (\_ -> hClose handle)
          
talk :: TVar (Store Handle) -> Handle -> IO ()
talk st h = do
    hSetNewlineMode h universalNewlineMode
    hSetBuffering h LineBuffering
    runConnection st h $ serverConnection h
    
runConnection :: TVar (Store Handle) -> Handle -> ConnectionM Handle a -> IO a
runConnection st h (Pure a) = return a
runConnection st h (Free f) = run f where
  continue c = runConnection st h c

  run (ReceiveAny c) = do
    hPutStrLn h "Enter a command"
    command <- retryRead h
    continue $ c $ Left command

  run (PutState newSt a c) = do
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
