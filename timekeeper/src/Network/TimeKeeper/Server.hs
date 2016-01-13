-- | This module defines the logic of the server in a declarative way,
-- using a custom monad.

module Network.TimeKeeper.Server (
  -- * Connection handler
  serverConnection,
  -- * Effects
  Store(..), emptyStore,
  ConnectionM,
  ConnectionEffect(..)
  ) where

import Network.TimeKeeper.Protocol
import Control.Monad
import Control.Monad.Free
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text
import Data.Text (Text)
import Data.Foldable(for_)
import Data.List (inits)
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
                                | PutState (Store addr) cont
                                  -- TODO | AtomicModifyState
                                deriving Functor

-- | A monad for the effects required by the core logic of the server.
type ConnectionM addr a = Free (ConnectionEffect addr) a

-- | Lifts an effect into the 'Free' monad
liftFree :: Functor f => f a -> Free f a
liftFree action = Free (fmap Pure action)

-- | Receive an event in the 'ConnectionM' monad
receive = liftFree (ReceiveAny id)

-- | Reply to the current connection in the 'ConnectionM' monad
reply ev = liftFree (Reply ev ())

-- | Send an event to a client
sendTo addr ev = liftFree (SendTo addr ev ())

-- | Retrieve the state in the 'ConnectionM' monad
getState = liftFree (GetState id)

-- | Change the state in the 'ConnectionM' monad
putState s = liftFree (PutState s ())

-- | The connection handling logic, in the declarative 'ConnectionM' monad.
serverConnection :: addr -> ConnectionM addr ()
serverConnection addr = forever $ do
  event <- receive
  case event of
    Left (Put path newValue) -> do
      store <- getState
      let update = case newValue of
                     Nothing -> M.delete path
                     Just val -> M.insert path val
          newStore = store { storeData = update (storeData store) }
      putState newStore

      let oldValue = M.lookup path (storeData store)
      broadcast path (Updated path oldValue newValue) newStore

    Left (Get path) -> do
      store <- getState
      let maybeVal = M.lookup path (storeData store)
      reply (ValueIs path maybeVal)

    Left (Subscribe path) -> do
      store <- getState
      let orEmpty = fromMaybe mempty
          update = M.alter (\subs -> Just (addr : orEmpty subs)) path
      putState (store { storeSubscriptions = update $ storeSubscriptions store })
      
    _ -> error "Not implemented yet"

-- | Send event to subscribed clients
broadcast :: Path -> Event -> Store addr -> ConnectionM addr ()
broadcast path event store = 
  let subs = storeSubscriptions store
      Path pathElems = path
  in sequence_ $ do p <- inits pathElems
                    addr <- join $ maybeToList $ M.lookup (Path p) subs
                    return $ sendTo addr event

