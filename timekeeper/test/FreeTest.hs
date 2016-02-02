-- | Test code for monads that are defined using 'Free'
--
-- See 'FreeTest'.
module FreeTest
       ( FreeTest
         -- * Invoke test
       , must
         -- * Handling effects
       , expect, expect_
       , nondet, nondet_
         -- * Test end
       , continue
       ) where
import Control.Monad.Free

data FreeTestDsl f a = Handler (forall b. f b -> [(b, a)])
                     | Stop
                     deriving Functor

-- | Monad for testing a monad that is defined using a free monad.
--
-- 'f' is the same functor as the argument to 'Free'.
--
-- This monad has the responsibility of handling effects using
-- combinators like 'expect' and 'nondet'.
--
-- The functions passed to 'expect' and 'nondet' will use asynchronous
-- exceptions like 'error' in order to make the test fail.
newtype FreeTest f a = FreeTest (Free (FreeTestDsl f) a)
                     deriving (Monad, Applicative, Functor)

-- | Test a free monad using the test monad.
--
-- All nondeterminism will be tested.
--
-- Return values will be checked using '=='.
must :: Eq a => Free f a -> FreeTest f a -> Bool
must f (FreeTest t) = mst f t where
 mst (Free f) (Free Stop) = True
 mst (Pure a) (Pure a') = a == a'
 mst (Pure a) _ = error "Effect expected"
 mst (Free f) (Free (Handler h)) =
   and $ do
     (unit, test) <- h f
     return$ mst unit test
 mst (Free f) _ = error "Unexpected effect"

-- | (Internal) Lifts an effect into the 'FreeTest' monad
lift :: FreeTestDsl f a -> FreeTest f a
lift action = FreeTest$ liftF action

-- | Ignore any further effects in the unit under test.
--
-- Inserting @continue@ at the end of a test means that it is ok for
-- production code to continue. This is useful, for example, when
-- testing code that is supposed to loop after performing some
-- actions.
continue :: FreeTest f a
continue = lift Stop

-- | Lets the test check the effect that is returned by the unit under
-- test. Errors can be reported asynchronously using 'error'.
--
-- In order for the test to continue, the continuation, of type @b@
-- must be returned.
--
-- A term of this type can normally be found in the constructor of
-- @f b@.
expect_ :: (forall b. f b -> b) -> FreeTest f ()
expect_ h = expect ((, ()) . h)

-- | Like 'expect_', but can be used to return a value from a
-- query-like effect to the test.
expect :: (forall b. f b -> (b, a)) -> FreeTest f a
expect h = lift (Handler (return . h))

-- | Like 'expect_', but non-deterministic.
--
-- This will be tested by testing all possible branches.
--
-- Note that the number of paths that require testing is likely
-- exponential in the number of effects tested.
nondet_ :: (forall b. f b -> [b]) -> FreeTest f ()
nondet_ h = lift (Handler (map (, ()) . h))

-- | Like 'expect', but non-deterministic. See 'nondet_'.
nondet :: (forall b. f b -> [(b, a)]) -> FreeTest f a
nondet h = lift (Handler h)
