
Algorithms package
===

This package contains an implementation of the barrier algorithm, similar to the one found in the zookeeper documentation.

Because it is written in pure code, it can be tested without side effects.


Test implementation
===

In order to test this code, the following has to be dealt with
 * Communication with a server
    - Server is modeled from a client handler perspective
    - Multiple client handlers operate on the same state
 * Nondeterminism caused by the network
    - [] monad, but Monte Carlo like QuickCheck may be needed if the system states are too many
    - Modeling the network

Anticipating working with many effects I have tried using the extensible-effects[1] library. Having to deal with Typeable, and a representation of effects that was new to me, I suspected I might have been better off with a custom monad or a monad expressed using a free monad[2].
Because the free monad approach is less complex by itself, it was quite obvious how the effects should be transformed, because they were expressed as plain Haskell values. With extensible-effects, I found myself looking for conversion functions that simply did not exist.

While simple tests like sending a message from one simulated node to another worked well, I did not succeed in testing the Barrier. In order to understand the problem that was causing the test to fail, I added logging functionality, but only after refactoring the code to use a more modern library for extensible effects, called 'freer'.
'freer' has roughly the same API as extensible-effects, which is slightly lacking in my experience. 'freer' does improve over 'free', by handling the projections and injections of the open union of effects, and not requiring a functor instance for the effect representation. It also claims decent performance.

The limitations of 'freer' I discovered were:
 - Problems with overlapping Member instances.
   (for example `Member (State s) l => Eff l a` declares that the computation of type `Eff l a` may access a state of type `s`, but having this constraint on a function can cause overlapping instances)
 - Missing functions for modifying effects other than the effect at the head of @l@
 - E u q  naming and protocol
 - No custom handlers possible for built-in effects. State effect can only be handled by built-in runState.

Porting the code to 'freer' succeeded. Type clarity improved, but reuse or modularity of handlers did not improve much (though slightly :) ).

TO DO:
Finish logging work (interpose might work, but for specific type only)


[1] Oleg Kiselyov, Amr Sabry, Cameron Swords
Extensible Effects: An Alternative to Monad Transformers
http://okmij.org/ftp/Haskell/extensible/exteff.pdf

[2] Wouter Swierstra
Data types a la carte