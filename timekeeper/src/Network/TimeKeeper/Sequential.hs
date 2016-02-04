module Network.TimeKeeper.Sequential where
import Network.TimeKeeper.Server
import Network.TimeKeeper.Protocol
import Control.Monad.Free

import Data.Text (pack)

import Data.IORef

-- | Runs the server logic as a simple command line program.
main :: IO ()
main = do
  st <- newIORef emptyStore
  runConnection st $ serverConnection ()

runConnection :: IORef (Store ()) -> ConnectionM () a -> IO a
runConnection st (Pure a) = return a
runConnection st (Free f) = run f where
  continue c = runConnection st c

  run (ReceiveAny c) = do
    putStrLn "Enter a command"
    command <- retryRead
    continue $ c $ Left command

  run (PutState newSt a c) = do
    writeIORef st newSt
    continue c

  run (GetState c) = do
    store <- readIORef st
    continue (c store)

  run (SendTo addr event c) = do
    putStrLn $ "To " ++ show addr ++ ": " ++ show event
    continue c

  run (Reply event c) = do
    putStrLn $ "Message to you: " ++ show event
    continue c

retryRead :: Read a => IO a
retryRead =
  do command <- getLine
     let parse = map fst . filter (\(_, remaining) -> remaining == "") . reads
     case parse command of
       [x]  ->  return x
       _    ->  putStrLn "Parse error" >> retryRead
