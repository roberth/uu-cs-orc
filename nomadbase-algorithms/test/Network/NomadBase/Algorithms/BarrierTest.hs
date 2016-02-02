{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Network.NomadBase.Algorithms.BarrierTest where

import Network.NomadBase.Algorithms.Barrier
import Network.TimeKeeper.Protocol
import Network.TimeKeeper.Server
import qualified Test.Framework
import Control.Monad.Free
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import Data.Either (rights, lefts)
import Data.List (permutations, uncons)
import Control.Monad (join, void)
import Control.Monad.State (runStateT, StateT, lift, put, get, modify, mapStateT)
import Test.Framework
import Data.Text(Text)

a `isPermutationOf` b = S.fromList a == S.fromList b

runChan1 :: ChanM lr rl l -> ChanM rl lr r ->
            [(ChanM lr rl l, ChanM rl lr r)]
runChan1 l@(Pure _)  r@(Pure _)  = [(l, r)]
runChan1 l@(Pure _)  r           = [(l, r)]
runChan1 l           r@(Pure _)  = [(l, r)]
runChan1 (Free (Send t cl)) (Free (Receive cr)) =
  runChan1 cl (cr t)
runChan1 (Free (Receive cl)) (Free (Send t cr)) =
  runChan1 (cl t) cr

prop_True = isok where
  prog1 = do
    receive
    transmit "Test1"
    transmit "Test2"
    receive
  prog2 = do
    transmit True
    r <- receive
    r' <- receive
    transmit (r == "Test1" && r' == "Test2")
  results = runChan1 prog1 prog2
  allSatisfy = flip all
  isok = results `allSatisfy` \case
    (Pure True, Pure ()) -> True
    _ -> False

-- | Monad for algorithms that may send and receive,
--
-- The receiving effect may refuse to process a message,
-- which should be interpreted as that the message was not
-- sent to the instance of the computation.
data SChanEff r t c = SSend t c
                    | SReceive (r -> Maybe c)
                    deriving (Functor)
type SChanM r t = Free (SChanEff r t)

type SChanReceive r t c = r -> Maybe (SChanM r t c)

data MessageSystem r t a = 
  MessageSystem
  { transit :: [r]
    -- | List of clients
    --
    -- a ->         Given a message,
    -- Maybe        a client may not be able to receive it
    -- ChanM a a    or a client may transmit and receive messages of the type a
    -- r            or be done with result type r
    --
    -- A client may not be able to receive a message, because
    --  * It has the wrong destination
    --  * It has the wrong type
    --  * It is supposed to be received in order
  , clients :: [SChanReceive r t a]
  , results :: [a]
  } deriving Functor

instance (Show a, Show r) => Show (MessageSystem r t a) where
  show m = "Transit: " ++ show (transit m) ++
           "\nResults" ++ show (results m) ++
           "\n"

emptySystem = MessageSystem { transit = [], clients = [], results = [] }

bimapChan :: (t -> t') -> (r' -> r) ->
             Free (ChanEff r t) a -> Free (ChanEff r' t') a
bimapChan f f' (Pure a) = Pure a
bimapChan f f' (Free (Send t c)) = Free (Send (f t) (bimapChan f f' c))
bimapChan f f' (Free (Receive g)) = Free (Receive (bimapChan f f' . g . f'))


bimapSChan :: (t -> t') -> (r' -> r) ->
             SChanM r t a -> SChanM r' t' a
bimapSChan f f' (Pure a) = Pure a
bimapSChan f f' (Free (SSend t c)) = Free (SSend (f t) (bimapSChan f f' c))
bimapSChan f f' (Free (SReceive g)) = Free (SReceive (fmap (bimapSChan f f') . g . f'))

bimapSChan' :: (r' -> Maybe r) -> (t -> t') ->
             SChanM r t a -> SChanM r' t' a
bimapSChan' f f' (Pure a) = Pure a
bimapSChan' f f' (Free (SSend t c)) = Free (SSend (f' t) (bimapSChan' f f' c))
bimapSChan' f f' (Free (SReceive c)) = Free (SReceive (bimapSChanReceive f f' c))

select :: (r' -> Maybe r) -> SChanM r t a -> SChanM r' t a
select f = bimapSChan' f id

bimapSChanReceive :: (r' -> Maybe r) ->
                     (t -> t') ->
                     SChanReceive r t a ->
                     SChanReceive r' t' a
bimapSChanReceive f f' recv = \msg' -> do msg <- f msg'
                                          r <- recv msg
                                          let x = bimapSChan' f f' r
                                          return$ x

bimapMessage f' f sys = MessageSystem
  { transit = map f' $ transit sys
  , clients = map (bimapSChanReceive f f') $ clients sys
  , results = results sys
  }

instance Monoid (MessageSystem r t a) where
  mempty = emptySystem
  mappend a b = MessageSystem
    { transit = transit a ++ transit b
    , clients = clients a ++ clients b
    , results = results a ++ results b
    }

addClient :: SChanM msg msg a -> MessageSystem msg msg a ->
              MessageSystem msg msg a
addClient schan sys = send schan sys
  where
    send (Free (SSend msg c)) sys =
      let sys' = sys { transit = msg : transit sys }
      in send c sys'

    send (Free (SReceive cont)) sys =
      sys { clients = cont : clients sys }

    send (Pure result) sys =
      sys { results = result : results sys }

mergeSystems :: MessageSystem r1 t1 a -> MessageSystem r2 t2 a ->
                (r1 -> r) ->
                (r2 -> r) ->
                (r -> Maybe r1) ->
                (r -> Maybe r2) ->
                (t1 -> t) ->
                (t2 -> t) ->
                MessageSystem r t a
mergeSystems s1 s2 injr1 injr2 proj1 proj2 injt1 injt2 =
  MessageSystem
  { transit = map injr1 (transit s1)
  , clients = map (bimapSChanReceive proj1 injt1) (clients s1)
  , results = results s1
  } `mappend`
  MessageSystem
  { transit = map injr2 (transit s2)
  , clients = map (bimapSChanReceive proj2 injt2) (clients s2)
  , results = results s2
  }

-- | Like mergeSystems but deletes 'transit' messages.
mergeSystems_ :: MessageSystem r1 t1 a -> MessageSystem r2 t2 a ->
                (r -> Maybe r1) ->
                (r -> Maybe r2) ->
                (t1 -> t) ->
                (t2 -> t) ->
                MessageSystem r t a
mergeSystems_ s1 s2 proj1 proj2 injt1 injt2 =
  MessageSystem
  { transit = []
  , clients = map (bimapSChanReceive proj1 injt1) (clients s1)
  , results = results s1
  } `mappend`
  MessageSystem
  { transit =[]
  , clients = map (bimapSChanReceive proj2 injt2) (clients s2)
  , results = results s2
  }

zipIndex = zip (iterate (+1) 0)

-- | Represents a way to remove an element from a list
data Take a = Take { taken :: a
                   , takeIndex :: Int
                   , notTaken :: [a]
                   } deriving (Eq, Show)
takes :: [a] -> [Take a]
takes = f 0 []
  where f n done (a : as) = take : more where
          take = Take a n (reverse done ++ as)
          more = f (n + 1) (a : done) as
        f n done [] = []

prop_takes_example_empty = takes [] === ([] :: [Take Int])
prop_takes_example_1 = takes "a" === [Take 'a' 0 []]
prop_takes_example_2 = takes "ab" === [Take 'a' 0 "b", Take 'b' 1 "a"]
prop_takes_example_3 = takes "abc" === [Take 'a' 0 "bc", Take 'b' 1 "ac", Take 'c' 2 "ab"]

-- | A list of possible scheduling choices
runnable :: MessageSystem msg msg a -> [((Take msg, Take (SChanReceive msg msg a)), SChanM msg msg a)]
runnable msgSys = do
  m@(Take msg msgIx otherMsgs) <- takes (transit msgSys)
  c@(Take client clientIx otherClients) <- takes (clients msgSys)
  acceptedMessageRepr <- maybeToList$ client msg
  return ((m, c), acceptedMessageRepr)

allExecutions :: MessageSystem msg msg a -> [MessageSystem msg msg a]
allExecutions msgSys = do
  choice <- runnable msgSys
  let ((Take _ _ remainingMsgs,
        Take cl _ remainingClients), newState) = choice
  let
      msgSys' = msgSys { transit = remainingMsgs, clients = remainingClients }
      newSys = addClient newState msgSys'

  if null (transit newSys)
    then return newSys
    else allExecutions newSys

allExecutions' :: MessageSystem msg msg a -> [MessageSystem msg msg a]
allExecutions' msgSys =
  let rnbl = runnable msgSys
  in case rnbl of
    [] -> [msgSys]
    rnbls ->
      do choice <- rnbls
         let ((Take _ _ remainingMsgs,
               Take cl _ remainingClients), newState) = choice
         let
           msgSys' = msgSys { transit = remainingMsgs, clients = remainingClients }
           newSys = addClient newState msgSys'

         if null (transit newSys)
           then return newSys
           else allExecutions' newSys

data Void
foldVoid :: Void -> a
foldVoid = error "impossible"

producer :: ChanM Void Int ()
producer = do
  transmit 1
  transmit 2
  transmit 3

consumer :: ChanM Int Void [String]
consumer = do
  x <- receive
  y <- receive
  z <- receive
  return $ map show [x, y, z]

chanMToSChanM :: ChanM r t a -> SChanM r t a
chanMToSChanM = \case
  Pure x -> Pure x
  Free (Send t c) -> Free$ SSend t (chanMToSChanM c)
  Free (Receive g) -> Free$ SReceive (\x -> Just (chanMToSChanM . g $ x))

initialize chan = addClient chan emptySystem

producer' = chanMToSChanM producer
consumer' = chanMToSChanM consumer

producer'' = Left <$> bimapSChan' (const Nothing) id producer'
consumer'' = Right <$> bimapSChan' (Just) (foldVoid) (consumer')

sys = initialize producer'' `mappend` initialize consumer''

prop_example_system =
  (`all` allExecutions sys)$ \msgSystem ->
    lefts (results msgSystem) == [()] &&
    (`all` rights (results msgSystem)) (isPermutationOf ["1", "2", "3"])

selectAddr myAddr (addr, msg) = if addr == myAddr then Just msg
                            else Nothing

chanServer :: Eq addr => addr -> ConnectionM addr () -> ServerM addr ()
chanServer addr (Pure a) = return a
chanServer addr (Free f) = (run f) where
  continue = chanServer addr

  run (ReceiveAny c) = do
    command <- liftF $ mkRight $ SReceive (selectAddr addr)
    continue (c command)

  run (PutState newSt c) = do
    liftF $ mkLeft $ StatePut newSt id
    continue c

  run (GetState c) = do
    s <- liftF $ mkLeft $ StateGet id
    continue (c s)

  run (AtomicModifyState f c) = do
    s <- liftF $ mkLeft $ StateGet id
    liftF $ mkLeft $ StatePut (f s) id
    continue c

  run (SendTo addr event c) = do
    liftF $ mkRight $ SSend (addr, event) id
    continue c

  run (Reply event c) = do
    liftF $ mkRight $ SSend (addr, event) id
    continue c

newtype Coproduct f g a = Coproduct (Either (f a) (g a))
instance (Functor f, Functor g) => Functor (Coproduct f g) where
  fmap f = Coproduct . coproduct
    (Left . fmap f) (Right . fmap f)

mkLeft :: f a -> Coproduct f g a
mkLeft = Coproduct . Left

mkRight :: g a -> Coproduct f g a
mkRight = Coproduct . Right

coproduct :: (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g (Coproduct e) = either f g e

data State s a = StateGet (s -> a) | StatePut  s a
               deriving Functor

type ServerM addr = Free (Coproduct
                          (State (Store addr))
                          (SChanEff (addr, Either Action Event) (addr, Event))
                         )
                   
head' = fmap fst . uncons

serverFor :: Eq addr =>
             [addr] ->
             (addr -> ServerM addr ()) ->
             SChanM (addr, Either Action Event) (addr, Event) ()
serverFor addrs connection = (
  do (_, state) <- init
     join $ liftF $ SReceive $ runServerOnInput state)
  where
    addHandlers = (`mapM_` addrs) $ \myAddr -> do
      -- Run the new connection until blocked by receive
      -- (which typically happens first :) )
      let selector r@(addr, msg) | addr == myAddr = Just r
          selector _ = Nothing
      mapStateT (select selector) $ runServerThread $ connection myAddr
    init = runStateT addHandlers (emptyServerState emptyStore)

data ServerState s i o = ServerState
                   { serverSharedState :: s
                   , serverHandlers ::
                     [ i -> Maybe (Free (Coproduct
                                         (State s)
                                         (SChanEff i o)
                                        ) ())
                     ]
                   }

emptyServerState :: s -> ServerState s i o
emptyServerState s = ServerState { serverSharedState = s
                                 , serverHandlers = []
                                 }

runServerOnInput :: ServerState s r t -> SChanReceive r t ()
runServerOnInput st i = do
  (handled, notTakenHandlers) <- head' $ do
    handler <- takes $ serverHandlers st
    let maybeHandled = taken handler i
    handled <- maybeToList maybeHandled
    return (handled, notTaken handler)
  Just $ do (maybeResult, state) <- runStateT (runServerThread handled) st
            void . liftF . SReceive . runServerOnInput $ state

runServerThread :: Free (Coproduct (State s) (SChanEff r t)) () ->
             StateT (ServerState s r t) (Free (SChanEff r t)) ()
runServerThread (Pure x) = return ()
runServerThread (Free (Coproduct (Left (StateGet c))))   =
        runServerThread . c . serverSharedState =<< get
runServerThread (Free (Coproduct (Left (StatePut s c)))) = do
        st <- get
        put (st { serverSharedState = s })
        runServerThread c
runServerThread (Free (Coproduct (Right (SSend s c)))) = do
        lift (liftF (SSend s id))
        runServerThread c
runServerThread (Free (Coproduct (Right (SReceive c)))) = do
        modify $ \st -> st { serverHandlers = c : serverHandlers st }
        return ()


testServer :: [Text] -> SChanM (Text, Either Action Event) (Text, Either a Event) ()
testServer addrs = bimapSChan (\(name, ev) -> (name, Right ev)) id server where
  server :: SChanM (Text, Either Action Event) (Text, Event) ()
  server = serverFor addrs (\addr -> chanServer addr (serverConnection addr))

testBarrier name = barrier2
  where
    match :: (Text, Either a Event) -> Maybe Event
    match (_, Right ev) = Just ev
    match _ = Nothing

    barrier2 :: SChanM (Text, Either a Event) (Text, Either Action e) ()
    barrier2 = bimapSChan' match (\action -> (name, Left action)) barrier1
    
    barrier1 :: SChanM Event Action ()
    barrier1 = chanMToSChanM barrier'
    
    barrier' :: ChanM Event Action ()
    barrier' = barrier (Path ["myApp", "webserversAvailable"]) 2 name

barrierSystem addrs = foldr addClient emptySystem $
                      testServer addrs : map testBarrier addrs
