{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Network.NomadBase.Algorithms.BarrierTest where

import Network.NomadBase.Algorithms.Barrier
import qualified Network.TimeKeeper.Protocol as Protocol
import Network.TimeKeeper.Server
import qualified Test.Framework
import Control.Monad.Freer
import Control.Monad.Free
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import Data.Either (rights, lefts)
import Data.List (permutations, uncons)
import Control.Monad (join, void)
import Test.Framework
import Data.Text(Text)
import Control.Monad.Freer.Internal
import Data.Void
import Data.Functor.Contravariant
import Data.Profunctor
import Data.Open.Union
import Control.Monad
import Prelude hiding (log)

a `isPermutationOf` b = S.fromList a == S.fromList b

--runChan1 :: (Member (Send lr) e1, Member (Receive lr) e2) => Eff e1 l -> Eff e2 r ->
--            [(Eff e1 l, Eff e2 r)]
runChan1 :: Eff '[Send lr, Receive rl] l -> Eff '[Send rl, Receive lr] r -> [(Eff '[Send lr, Receive rl] l, Eff '[Send rl, Receive lr] r)]
runChan1 l@(Val _)  r@(Val _)  = [(l, r)]
runChan1 l@(Val _)  r           = [(l, r)]
runChan1 l           r@(Val _)  = [(l, r)]
runChan1 (E ul ql) (E ur qr) | (Right (Send t)) <- decomp ul, Left ur' <- decomp ur, (Right Receive) <- decomp ur' =
    runChan1 (qApp ql ()) (qApp qr t)
runChan1 (E ul ql) (E ur qr) | Left ul' <- decomp ul, Right Receive <- decomp ul', (Right (Send t)) <- decomp ur =
    runChan1 (qApp ql t) (qApp qr ())
runChan1 _ _ = error "runChan1 does not support queueing."

prop_True = isok where
  prog1 :: Eff '[Send String, Receive Bool] Bool
  prog2 :: Eff '[Send Bool, Receive String] ()
  prog1 = do
    _ :: Bool <- receive
    transmit ("Test1" :: String)
    transmit ("Test2" :: String)
    x :: Bool <- receive
    return x
  prog2 = do
    transmit True
    r :: String <- receive
    r' :: String <- receive
    transmit (r == "Test1" && r' == "Test2")
  results = runChan1 prog1 prog2
  allSatisfy = flip all
  isok = results `allSatisfy` \case
    (Val True, Val ()) -> True
    _ -> False

-- | Monad for algorithms that may send and receive,
--
-- The receiving effect may refuse to process a message,
-- which should be interpreted as that the message was not
-- sent to the instance of the computation.

-- Note that the GADT approach does not work here because the
-- primitive receive handler may choose to refuse the message.
data SReceive r a = SReceive (r -> Maybe a)

sReceive f = send $ SReceive f

send' = send . Send

instance Profunctor SReceive where
  dimap f g (SReceive x) = SReceive (fmap g . x . f)

type SChan r t l = (Member (Send t) l, Member (SReceive r) l)

select :: (r' -> Maybe r) -> Eff (SReceive r ': l) a -> Eff (SReceive r' ': l) a
select f = substituteEffect return $ \(SReceive r) q ->
  sReceive (f >=> r) >>= q

-- | For using the Receive effect in an SReceive context.
selectAll :: Eff (Receive r ': l) a -> Eff (SReceive r ': l) a
selectAll = substituteEffect return $ \(Receive) q ->
  sReceive Just >>= q

mapSend :: (t -> t') -> Eff (Send t ': l) a -> Eff (Send t' ': l) a
mapSend f = substituteEffect return $ \(Send t) q ->
  send' (f t) >>= q

weakenEff :: Eff l a -> Eff (any ': l) a
weakenEff (Val x) = Val x
weakenEff (E u q) = E (weaken u) (tsingleton (qComp q weakenEff))
    
substituteEffect :: (a -> Eff (t' ': r) w) ->
                (forall v. t v -> Arr (t' ': r) v w -> Eff (t' ': r) w) ->
                Eff (t ': r) a -> Eff (t' ': r) w
substituteEffect ret h = loop
  where
    loop (Val x)   = ret x
    loop (E u' q)  = case decomp u' of
      Right x -> h x k
      Left u -> E (weaken u) (tsingleton k)
      where k = qComp q loop

keepEffect :: (forall b. Eff r b -> Eff r' b) ->
                Eff (t ': r) a -> Eff (t ': r') a
keepEffect f = loop
  where
    loop (Val x)   = Val x
    loop (E u' q)  = case decomp u' of
      Right x -> E (inj x) (tsingleton k)
      Left u -> do xx <- weakenEff $ f (E u (tsingleton Val))
                   k xx
      where k = qComp q loop


-- | Unwraps @t@ from an open union of types @'[t]@
prj1 :: Union '[t] v -> t v
prj1 x = case decomp x of
  Right t -> t
  Left _ -> error "Unexpected type in union" -- will not occur

-- The weaken should not be necessary, but is required to compile...
-- Library bug?
inj' :: a x -> Union (b ': a ': l) x
inj' x = weaken (inj x)

swapEff :: Eff (a ': b ': l) x -> Eff (b ': a ': l) x
swapEff = \case
  Val x -> Val x
  E u q -> case decomp u of
    Right a -> E (inj' a) (tsingleton (q `qComp` swapEff))
    Left u' -> case decomp u' of
      Right b -> E (inj b) (tsingleton (q `qComp` swapEff))
      Left x -> E (weaken (weaken x)) (tsingleton (q `qComp` swapEff))

onInner f = swapEff . f . swapEff

data MessageSystem m r a = 
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
    --  * Some other reason
  , clients :: [([String], r -> Maybe (m a))]

    -- | The results are all pure, but can be augmented with for example
    -- a log.
  , results :: [([String], a)]
  } deriving Functor

instance (Show a, Show r) => Show (MessageSystem m r a) where
  show ms = "Transit: " ++ show (transit ms) ++
           "\nResults" ++ show (results ms) ++
           "\nClient logs: " ++ show (map fst (clients ms)) ++
           "\n"

instance Monoid (MessageSystem m t a) where
  mempty = emptySystem
  mappend a b = MessageSystem
    { transit = transit a ++ transit b
    , clients = clients a ++ clients b
    , results = results a ++ results b
    }

emptySystem = MessageSystem { transit = [], clients = [], results = [] }

  
{-
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
-}

addClient :: (l ~ '[SReceive t, Send t, Logging]) =>
             Eff l a ->
             MessageSystem (Eff l) t a ->
               MessageSystem (Eff l) t a
addClient eff = addClient' eff []

addClient' :: (l ~ '[SReceive t, Send t, Logging]) =>
             Eff l a ->
             [String] ->
             MessageSystem (Eff l) t a ->
               MessageSystem (Eff l) t a
addClient' (Val x) log sys = sys { results = (log, x) : results sys }
addClient' (E u q) log sys = case decomp u of
  Right (SReceive c) -> sys { clients = (log, (fmap (q `qApp`) . c)) : clients sys }
  Left u' -> case decomp u' of
    Right (Send msg) ->
      let sys' = sys { transit = msg : transit sys }
      in addClient' (qApp q ()) log sys'
    Left u' -> case prj1 u' of
      (Log msg) -> addClient' (qApp q ()) (log ++ [msg]) sys

{-
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
-}
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
        f _ _ [] = []

prop_takes_example_empty = takes [] === ([] :: [Take Int])
prop_takes_example_1 = takes "a" === [Take 'a' 0 []]
prop_takes_example_2 = takes "ab" === [Take 'a' 0 "b", Take 'b' 1 "a"]
prop_takes_example_3 = takes "abc" === [Take 'a' 0 "bc", Take 'b' 1 "ac", Take 'c' 2 "ab"]


-- | A list of possible scheduling choices
runnable :: MessageSystem m msg a ->
            [( ( Take msg
               , Take ([String], msg -> Maybe (m a)))
             , ([String], m a))]
runnable msgSys = do
  m@(Take msg _ _) <- takes (transit msgSys)
  c@(Take (log, client) _ _) <- takes (clients msgSys)
  acceptedMessageRepr <- maybeToList$ client msg
  let log' = log -- TODO: accept more logging
  return ((m, c), (log', acceptedMessageRepr))
  

allExecutions :: (l ~ '[SReceive msg, Send msg, Logging]) =>
                 MessageSystem (Eff l) msg a -> [MessageSystem (Eff l) msg a]
allExecutions msgSys = do
  choice <- runnable msgSys
  let ((Take _ _ remainingMsgs,
        Take _ _ remainingClients), (log, newState)) = choice
  let
      msgSys' = msgSys { transit = remainingMsgs, clients = remainingClients }
      newSys = addClient' newState log msgSys'

  if null (transit newSys)
    then return newSys
    else allExecutions newSys

allExecutions' ::
  (l ~ '[SReceive msg, Send msg, Logging]) =>
  MessageSystem (Eff l) msg a ->
    [MessageSystem (Eff l) msg a]
allExecutions' msgSys =
  let rnbl = runnable msgSys
  in case rnbl of
    [] -> [msgSys]
    rnbls ->
      do choice <- rnbls
         let ((Take _ _ remainingMsgs,
               Take _ _ remainingClients), (log, newState)) = choice
         let
           msgSys' = msgSys { transit = remainingMsgs, clients = remainingClients }
           newSys = addClient' newState log msgSys'

         if null (transit newSys)
           then return newSys
           else allExecutions' newSys

{-
producer :: Chan Void Int ()
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
consumer'' = Right <$> bimapSChan' (Just) (absurd) (consumer')

sys = initialize producer'' `mappend` initialize consumer''

prop_example_system =
  (`all` allExecutions sys)$ \msgSystem ->
    lefts (results msgSystem) == [()] &&
    (`all` rights (results msgSystem)) (isPermutationOf ["1", "2", "3"])


 -- -}

data ServerState s i o = ServerState
                   { serverSharedState :: s
                   , serverHandlers ::
                     [ i -> Maybe (Eff '[State s, SReceive i, Send o] ())
                     ]
                   }

emptyServerState :: s -> ServerState s i o
emptyServerState s = ServerState { serverSharedState = s
                                 , serverHandlers = []
                                 }

type SChanReceive i o a = i -> Maybe (Eff '[SReceive i, Send o] a)

type ServerM addr = Eff '[ State (Store addr)
                         , SReceive (addr, Either Protocol.Action Protocol.Event)
                         , Send (addr, Protocol.Event)
                         ]

serverFor :: Eq addr =>
             [addr] ->
             (addr -> ServerM addr ()) ->
             Eff '[ SReceive (addr, Either Protocol.Action Protocol.Event)
                  , Send (addr, Protocol.Event)
                  ] ()
serverFor addrs connection = (
  do (_, state) <- init
     join $ sReceive $ runServerOnInput state)
  where
    addHandlers = (`mapM_` addrs) $ \myAddr -> do
      -- Run the new connection until blocked by receive
      -- (which typically happens first :) )
      let selector r@(addr, msg) | addr == myAddr = Just r
          selector _ = Nothing
      onInner (select selector) $ runServerThread $ connection myAddr
    init = runState addHandlers (emptyServerState emptyStore)

                                      
runServerOnInput :: ServerState s i o -> SChanReceive i o ()
runServerOnInput st i = do
  (handled, notTakenHandlers) <- head' $ do
    handler <- takes $ serverHandlers st
    let maybeHandled = taken handler i
    handled <- maybeToList maybeHandled
    return (handled, notTaken handler)
  Just $ do (maybeResult, state) <- runState (runServerThread handled) st
            join . sReceive . runServerOnInput $ state

runServerThread :: forall s r t.
                   Eff ('[State s,                   SReceive r, Send t]) () ->
                   Eff ('[State (ServerState s r t), SReceive r, Send t]) ()
runServerThread (Val x) = Val x
runServerThread (E u q) = case decomp u of
  Right Get -> do
    ss :: ServerState s r t <- get
    let x = serverSharedState ss
    runServerThread (q `qApp` x)
  Right (Put s) -> do
    ss :: ServerState s r t <- get
    let x' = ss { serverSharedState = s }
    runServerThread (q `qApp` ())
  Left u' -> case decomp u' of
    Right (SReceive f) -> do
      ss :: ServerState s r t <- get
      put $ ss { serverHandlers = fmap (q `qApp`) . f : serverHandlers ss }
      -- done
    Left u'' -> do
      x <- E (weaken (weaken u'')) (tsingleton Val)
      runServerThread (q `qApp` x)


--------------------
----- SPECIFIC -----

data Proxy t = Proxy

{-
logSends :: forall t l a.
            ( Member (Logging) l
            , Member (Send t) l
            , Show t
            ) =>
            Proxy t ->
            Eff l a -> Eff l a
logSends Proxy = interpose return (\(Send (x :: t)) q -> do
  log ("Sending" ++ show x)
  q ()
  )
-}

onlyLeft (addr, Left l) = Just (addr, Left l)
onlyLeft _ = Nothing

testServer :: forall addr.
              Eq addr =>
              [addr] ->
              Eff '[ SReceive (addr, Either Protocol.Action Protocol.Event)
                   , Send (addr, Either Protocol.Action Protocol.Event)
                   ] ()
testServer addrs = select onlyLeft $ onInner (mapSend $ \(name, ev) -> (name, Right ev)) server where
  server = serverFor addrs (\addr -> chanServer addr (serverConnection addr))

testBarrier :: Int ->
               Text ->
               Eff '[ SReceive (Text, Either a Protocol.Event)
                    , Send (Text, Either Protocol.Action b)
                    ] ()
testBarrier n name = select match
                 . selectAll
                 . onInner (mapSend (\action -> (name, Left action)))
                 $ barrier' where
    match :: (Text, Either a Protocol.Event) -> Maybe Protocol.Event
    match (_, Right ev) = Just ev
    match _ = Nothing

    barrier' :: Chan Protocol.Event Protocol.Action l => Eff l ()
    barrier' = barrier (Protocol.Path ["myApp", "webserversAvailable"]) n name

addClient'' (label, m) sys = addClient' m [label] sys

barrierSystem addrs = foldr addClient'' emptySystem $
                      ("This is the server.", f (testServer addrs)) :
                        map (\addr -> ( "This is: " ++ show addr
                                      , f (testBarrier (length addrs) addr))) addrs
  where f = logSends . keepEffect (keepEffect (weakenEff))
        logSends = id {- interpose return (\(Send (x :: (Text, Either Protocol.Action Protocol.Event) )) q -> do
                                        log ("Sending" ++ show x)
                                        q ()
                                    ) -}


chanServer :: ( Eq addr
              , Member (State (Store addr)) l
              , Member (SReceive (addr, Either Protocol.Action Protocol.Event)) l
              , Member (Send (addr, Protocol.Event)) l
              ) => addr -> ConnectionM addr () -> Eff l ()
chanServer addr (Pure a) = return a
chanServer addr (Free f) = (on f) where
  continue = chanServer addr

  on (ReceiveAny c) = do
    command <- sReceive $ \case
      (someAddr, msg) | someAddr == addr -> Just msg
      _ -> Nothing
    continue (c command)

  on (PutState newSt c) = do
    put newSt
    continue c

  on (GetState c) = do
    s <- get
    continue (c s)

  on (AtomicModifyState f c) = do
    s <- get
    put (f s)
    continue c

  on (SendTo addr event c) = do
    send' (addr, event)
    continue c

  on (Reply event c) = do
    send' (addr, event)
    continue c

head' = fmap fst . uncons

-------------------
----- Logging -----

data Logging a where
  Log :: String -> Logging ()

log = send . Log

-----------------
----- State -----

-- Taken from freer

-- | Strict State effects: one can either Get values or Put them
data State s v where
  Get :: State s s
  Put :: !s -> State s ()

-- | Retrieve state
get :: Member (State s) r => Eff r s
get = send Get

-- | Modify state
put :: Member (State s) r => s -> Eff r ()
put s = send (Put s)

-- | Handler for State effects
runState :: Eff (State s ': r) w -> s -> Eff r (w,s)
runState (Val x) s = return (x,s)
runState (E u q) s = case decomp u of
  Right Get      -> runState (qApp q s) s
  Right (Put s') -> runState (qApp q ()) s'
  Left  u'       -> E u' (tsingleton (\x -> runState (qApp q x) s))
