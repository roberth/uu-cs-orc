module Network.TimeKeeper (server) where
import qualified Network.TimeKeeper.Sequential as S
import qualified Network.TimeKeeper.Concurrent as C

server :: IO ()
server = C.main
