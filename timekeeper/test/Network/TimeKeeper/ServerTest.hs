{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Network.TimeKeeper.ServerTest where
import Test.Framework
import Network.TimeKeeper.Server
import Network.TimeKeeper.Protocol

import Data.Maybe
import Data.Either
import Control.Monad.Free
import qualified Data.Map as M

prop_simple_test = noError (
    serverConnection ("addr"::String) `must`
      [ \(ReceiveAny c) -> Right (c (Left$ Put (Path []) (Just "test123"))) -- put a value at root
      , \(GetState c) -> Right (c (Store mempty mempty))        -- on empty store
      , \(PutState s c) -> if storeData s == M.fromList [(Path [], "test123")] then Right c else Left "Store should contain only Root -> test123"
      ]
  )

prop_subscription_scenario = noError (
    serverConnection ("addr"::String) `must`
      [ \(ReceiveAny c) -> Right (c (Left$ Subscribe (Path ["a"])))
      , \(GetState c) -> Right (c (emptyStore))
      , \(PutState s c) -> if s == Store {storeData = M.fromList [], storeSubscriptions = M.fromList [(Path ["a"],["addr"])]} then Right c else error "Unexpected store state modification"
      , \(ReceiveAny c) -> Right (c (Left$ Put (Path ["a"]) (Just "someVal")))
      , \(GetState c) -> Right (c (Store {storeData = M.fromList [], storeSubscriptions = M.fromList [(Path ["a"],["addr"])]}))
      , \(PutState s c) -> if s == Store {storeData = M.fromList [(Path ["a"], "someVal")], storeSubscriptions = M.fromList [(Path ["a"],["addr"])]} then Right c else error "Unexpected store state modification"
      , \(SendTo "addr" ev c) -> if ev == Updated {eventPath = Path ["a"], oldValue = Nothing, newValue = Just "someVal"} then Right c else error (show ev)
      , \(ReceiveAny c) -> Right (c (Left$ Subscribe (Path ["a"])))
      ]
  )

prop_record_subscription = noError (
    serverConnection ("addr"::String) `must`
      [ \(ReceiveAny c) -> Right (c (Left$ Subscribe (Path ["a"])))
      , \(GetState c) -> Right (c (emptyStore))
      , \(PutState s c) -> if s == Store {storeData = M.fromList [], storeSubscriptions = M.fromList [(Path ["a"],["addr"])]} then Right c else error "Unexpected store state modification"
      , \(ReceiveAny c) -> Right (c (Left$ Put (Path ["a"]) (Just "someVal")))
      ]
  )

prop_send_to_subscription = noError (
    serverConnection ("addr"::String) `must`
      [ \(ReceiveAny c) -> Right (c (Left$ Put (Path ["a"]) (Just "someVal")))
      , \(GetState c) -> Right (c (Store {storeData = M.fromList [], storeSubscriptions = M.fromList [(Path ["a"],["someGuy"])]}))
      , \(PutState s c) -> if s == Store {storeData = M.fromList [(Path ["a"], "someVal")], storeSubscriptions = M.fromList [(Path ["a"],["someGuy"])]} then Right c else error "Unexpected store state modification"
      , \(SendTo "someGuy" ev c) -> if ev == Updated {eventPath = Path ["a"], oldValue = Nothing, newValue = Just "someVal"} then Right c else error (show ev)
      , \(ReceiveAny c) -> Right (c (Left$ Subscribe (Path ["a"])))
      ]
    )

prop_send_subpath_to_subscription = noError (
    serverConnection ("addr"::String) `must`
      [ \(ReceiveAny c) -> Right (c (Left$ Put (Path ["a","b"]) (Just "someVal")))
      , \(GetState c) -> Right (c (Store {storeData = M.fromList [], storeSubscriptions = M.fromList [(Path ["a"],["someGuy"])]}))
      , \(PutState s c) -> if s == Store {storeData = M.fromList [(Path ["a","b"], "someVal")], storeSubscriptions = M.fromList [(Path ["a"],["someGuy"])]} then Right c else error "Unexpected store state modification"
      , \(SendTo "someGuy" ev c) -> if ev == Updated {eventPath = Path ["a","b"], oldValue = Nothing, newValue = Just "someVal"} then Right c else error (show ev)
      , \(ReceiveAny c) -> Right (c (Left$ Subscribe (Path ["a"])))
      ]
    )


mapLeft f (Left l) = Left (f l)
mapLeft _ (Right r) = Right r

must :: Free f a -> [forall b. f b -> Either e b] -> Either (Maybe e) a
must (Pure a) handlers = return a
must (Free f) (h:hs) = let r' = Just `mapLeft` h f
                       in do r <- r'
                             must r hs
must (Free f) [] = Left Nothing

noError (Right _) = True       -- terminated with some value
noError (Left Nothing) = True  -- no error, not terminated
noError _ = False              -- some error
