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
      [ \(ReceiveAny c) -> Right (c (Left$ Put Root (Just "test123"))) -- put a value at root
      , \(GetState c) -> Right (c (Store mempty mempty))        -- on empty store
      , \(PutState s c) -> if storeData s == M.fromList [(Root, "test123")] then Right c else Left "Store should contain only Root -> test123"
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
