#!/usr/bin/env runhaskell

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO
import System.Process

putOut = hPutStrLn stdout
putLog = hPutStrLn stderr

main = do
  
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering

  buildPending <- atomically $ newTVar True
  forkIO (builder buildPending)

  (inotifyPipeR, inotifyPipeW) <- createPipe
  inotifyProc <- runProcess "inotifywait"
        [ "--excludei"
        ,   "(^|/)(.*~|\\.#.*|#.*#|\\.git|dist|\\.HTF|result|result-[0-9]*)$"
        , "-mr"
        , "-e"
        ,   "CREATE,MODIFY,DELETE"
        , "--"
        , "src"
        , "test"
        ]
        Nothing -- same working dir
        Nothing -- same environment vars
        Nothing -- same stdin (who cares)
        (Just inotifyPipeW) -- stdout
        Nothing -- same stdout

  -- also accept <ENTER> on stdin
  forkIO $ forever $ do
    getLine
    atomically $ writeTVar buildPending True
    

  forever $ do
    hGetLine inotifyPipeR >>= (putLog . ("Got event " ++))
    atomically $ do
      writeTVar buildPending True

builder buildPending = forever $ do
  atomically $ do
    pending <- readTVar buildPending
    when (not pending) retry
  putLog "Triggered, backing off..."
  threadDelay (200 * 1000) -- micros, so 200 * 1000 = 0.2s
  putLog "Building..."
  atomically $ do
    writeTVar buildPending False
  rawSystem "./run-tests" []
