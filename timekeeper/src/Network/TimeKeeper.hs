module Network.TimeKeeper (server) where
import qualified Network.TimeKeeper.Sequential as S
import qualified Network.TimeKeeper.Concurrent as C
import qualified Network.TimeKeeper.Distributed as D

server :: IO ()
server = D.main
