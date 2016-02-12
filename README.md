
# NomadBase

_(formerly TimeKeeper)_

This repository contains the result of a mini-project at Universiteit Utrecht.

The goal was to familiarize ourselves with concurrency and parallellism in Haskell, by exploring 'orchestration'.

In this project, we have built a data store with subscriptions inspired by Apache ZooKeeper, that could be used to communicate information about a cluster.

The code for this part of the project can be found in the `timekeeper` directory. Documentation is provided in the `timekeeper/src` directory.

Also, in order to validate the concept and implementation, we have made an initial effort to thoroughly test a 'barrier' synchronisation primitive built on the protocol. More information can be found in the `nomadbase-algorithms` readme.

Furthermore, we have documented plans for improving the resilience of the system.
