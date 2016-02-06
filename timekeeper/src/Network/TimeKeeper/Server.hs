-- | This module defines the logic of the server in a declarative way,
-- using a custom monad.

module Network.TimeKeeper.Server (
  -- * Connection handler
  serverConnection,
  -- * Effects
  Store(..), emptyStore,
  ConnectionM,
  ConnectionEffect(..),
  updateStore
  ) where

import Network.TimeKeeper.Protocol
import Control.Monad
import Control.Monad.Free
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text
import Data.Text (Text)
import Data.Foldable(for_)
import Data.List (inits, delete, isPrefixOf, group)
import Data.Maybe (maybeToList, fromMaybe)

-- | The state of the server
data Store addr = Store { storeData :: Map Path Text
                        , storeSubscriptions :: Map Path [addr]
                        }
                  deriving (Eq, Show)
emptyStore = Store mempty mempty

-- | The effects required by the core logic of the server.
-- 
-- The functor instance allows this data type to be used with a
-- 'Free' monad, in order to add a notion of sequential computation.
data ConnectionEffect addr cont = ReceiveAny (Either Action Event -> cont)
                                | Reply Event cont
                                | SendTo addr Event cont
                                | GetState (Store addr -> cont)
                                | PutState (Store addr) (Maybe Action) cont
                                | AtomicModifyState (Store addr -> Store addr) cont
                                deriving Functor

-- | A monad for the effects required by the core logic of the server.
type ConnectionM addr a = Free (ConnectionEffect addr) a

-- | Lifts an effect into the 'Free' monad
liftFree :: Functor f => f a -> Free f a
liftFree = liftF

-- | Receive an event in the 'ConnectionM' monad
receive = liftFree (ReceiveAny id)

-- | Reply to the current connection in the 'ConnectionM' monad
reply ev = liftFree (Reply ev ())

-- | Send an event to a client
sendTo addr ev = liftFree (SendTo addr ev ())

-- | Retrieve the state in the 'ConnectionM' monad
getState = liftFree (GetState id)

-- | Change the state in the 'ConnectionM' monad
putState s a = liftFree (PutState s a ())

-- | Atomically modify the state in the 'ConnectionM' monad
modifyState f = liftFree (AtomicModifyState f ())

-- | The connection handling logic, in the declarative 'ConnectionM' monad.
serverConnection :: Eq addr => addr -> ConnectionM addr ()
serverConnection addr = forever $ do
  event <- receive
  case event of
    Left (Put path newValue) -> do
      store <- getState
      let newStore = updateStore store path newValue
      putState newStore $ Just $ Put path newValue

      let oldValue = M.lookup path (storeData store)
      broadcast path (Updated path oldValue newValue) newStore

    Left (Get path) -> do
      store <- getState
      let maybeVal = M.lookup path (storeData store)
      reply (ValueIs path maybeVal)

    Left (GetChildren path) -> do
      children <- getChildren path
      reply (ChildrenAre path children)

    Left (Subscribe path) -> do
      store <- getState
      let orEmpty = fromMaybe mempty
          update = M.alter (\subs -> Just (addr : orEmpty subs)) path
      putState (store { storeSubscriptions = update $ storeSubscriptions store }) Nothing

    Left (Unsubscribe path) -> do
      store <- getState
      let update = M.alter (\subs -> delete addr `fmap` subs) path
      putState (store { storeSubscriptions = update $ storeSubscriptions store }) Nothing

    _ -> error "Not implemented yet"
    
-- | Put a new value at a given place in the store
updateStore :: Store a -> Path -> Maybe Text -> Store a
updateStore store path newValue = store { storeData = update newValue (storeData store) }
    where update Nothing = M.delete path
          update (Just val) = M.insert path val

-- | Send event to subscribed clients
broadcast :: Path -> Event -> Store addr -> ConnectionM addr ()
broadcast path event store = 
  let subs = storeSubscriptions store
      Path pathElems = path
  in sequence_ $ do p <- inits pathElems
                    addr <- join $ maybeToList $ M.lookup (Path p) subs
                    return $ sendTo addr event

-- | Get the storeData field from the Store                    
getStoreData :: ConnectionM addr (Map Path Text)
getStoreData = storeData `fmap` getState

getDescendants :: Path -> ConnectionM addr [(Path, Text)]
getDescendants (Path path) = flip fmap getStoreData $ \storeData' ->
  let
       (pre, post) = M.split (Path path) storeData'
       postList = M.toList post
       isDescendant (Path p, _) = path `isPrefixOf` p
       descendants = takeWhile isDescendant postList
  in descendants
  
getChildren :: Path -> ConnectionM addr [NodeName]
getChildren (Path path) = flip fmap (getDescendants (Path path)) $ \desc ->
  let childStream = do -- a non-unique but sorted list of children
        let depth = length path
        (Path p, _) <- desc
        let subpath = drop depth p
        safeHead subpath
      sorted2unique = map head . group -- nub for sorted list
  in sorted2unique childStream

safeHead :: [a] -> [a]
safeHead (h:_) = [h]
safeHead x = x
