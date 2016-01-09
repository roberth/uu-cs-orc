module Network.TimeKeeper.Server where

import Network.TimeKeeper.Protocol
import Control.Monad
import Control.Monad.Free
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text
import Data.Foldable(for_)

data Store addr = Store { storeData :: Map Path Text
                        , storeSubscriptions :: Map Path [addr]
                        }
emptyStore = Store mempty mempty

data ConnectionEffect addr cont = ReceiveAny (Either Action Event -> cont)
                             | Reply Event cont
                             | Broadcast Path Event cont
                             | GetState (Store addr -> cont)
                             | PutState (Store addr) cont
                             -- TODO | AtomicModifyState
                             deriving Functor

liftFree :: Functor f => f a -> Free f a
liftFree action = Free (fmap Pure action)

receive            = liftFree (ReceiveAny id)
reply ev           = liftFree (Reply ev ())
broadcast path ev  = liftFree (Broadcast path ev ())
getState           = liftFree (GetState id)
putState s         = liftFree (PutState s ())

type ConnectionM addr = Free (ConnectionEffect addr)

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
      broadcast path (Updated path oldValue newValue)

    Left (Get path) -> do
      store <- getState
      let maybeVal = M.lookup path (storeData store)
      reply (ValueIs path maybeVal)

    _ -> error "Not implemented yet"
