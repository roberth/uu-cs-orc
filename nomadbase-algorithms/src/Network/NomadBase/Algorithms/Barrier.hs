-- | Implements a barrier on NomadBase.
module Network.NomadBase.Algorithms.Barrier where
import Prelude hiding ((/))
import Network.TimeKeeper.Protocol
import Network.TimeKeeper.Protocol((/),slash)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Free

data ChanEff r t c = Send t c
                   | Receive (r -> c)
                   deriving (Functor)

type ChanM r t = Free (ChanEff r t)

transmit t = liftF (Send t ())
receive = liftF (Receive id)

-- | Wait for a barrier
--
-- Inputs:
--  * a path that serves as a barrier
--  * the number of nodes to wait for the barrier
--  * a unique name for this host
--  * a server connection (implicit in @m@)
barrier :: Path -> Int -> Text -> ChanM Event Action ()
barrier basePath nNodes name = do
  let barrierBase = basePath `slash` Path [":barrier"]
      myPath = barrierBase `slash` Path [name]

  -- these two should be atomic
  transmit (Subscribe barrierBase)
  transmit (GetChildren barrierBase)

  let wait = do
        r <- receive
        case r of
          ChildrenAre p c | length c >= nNodes ->
            return ()
          Updated path Nothing (Just _) -> do
            transmit (GetChildren barrierBase)
            wait
          _ ->
            wait
  wait
