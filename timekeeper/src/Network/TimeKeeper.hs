module Network.TimeKeeper (server) where
import qualified Network.TimeKeeper.Sequential as S

server :: IO ()
server = S.main
