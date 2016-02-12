-- | Implements a barrier on NomadBase.
module Network.NomadBase.Algorithms.Barrier where
import Prelude hiding ((/))
import Network.TimeKeeper.Protocol
import Network.TimeKeeper.Protocol((/),slash)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Freer

data Send t a where
  Send :: t -> Send t ()

data Receive r a where
  Receive :: Receive r r

type Chan r t l = (Member (Send t) l, Member (Receive r) l)

type Client a = Chan Event Action a
type Server a = Chan Action Event a

-- type ChanM r t = Eff '[ChanEff r t]

transmit :: Member (Send t) e => t -> Eff e ()
transmit = send . Send

receive :: Member (Receive r) e => Eff e r
receive = send Receive

-- | Wait for a barrier
--
-- Inputs:
--  * a path that serves as a barrier
--  * the number of nodes to wait for the barrier
--  * a unique name for this host
--  * a server connection (implicit in @m@)
barrier :: Client l => Path -> Int -> Text -> Eff l ()
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
