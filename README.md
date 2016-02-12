
# NomadBase

_(formerly TimeKeeper)_

This repository contains the result of a mini-project at Universiteit Utrecht.

The goal was to familiarize ourselves with concurrency and parallellism in Haskell, by exploring 'orchestration'.

In this project, we have built a data store with subscriptions inspired by Apache ZooKeeper, that could be used to communicate information about a cluster.

The code for this part of the project can be found in the `timekeeper` directory. Documentation is provided in the `timekeeper/src` directory.

Also, in order to validate the concept and implementation, we have made an initial effort to thoroughly test a 'barrier' synchronisation algorithm built on the protocol. More information can be found in the `nomadbase-algorithms` readme.

We had plans to make the system robust such that it would keep functioning upon verius crashes or, when failures were to sufear, maintain a state from which the system would be able to recover upon reconnection of failing parts of the system. This was supposed to be realised with some extra measures:
    -Desired behaviour on Client crash:
        -No action should be taken apart form noticing the leader, so no deletion of client requests/submissions either.
    -Desired behaviour on Client recovery:
        -The server that handled this client should accept the reconnection, notify the leader and serve the client as usual.
    -Desired behaviour on Server crash:
        -The basterd clients must be found and distributed over the other server.
        -Half completed transactions should be processed again from client initiative.
    -Desired behaviour on Server recovery:
        -Reassign it's original clients, new open requests should be handled by the substitute server not the original one.
    -Desired behaviour on Leader crash:
        -Servers should serve their clients as well as possible
        -Servers should try and find the other servers, once more than half of all servers is discovered consensus should be established on assigning a new leader.
        -All half completed requests should be merged after assigning a new leader, the leader breaks ties might they occur.
    -Desired behaviour on Leader recovery:
        -If the leader can find more than half of it's servers it should continue as the leader and handle bastard clients as usual.
        -If a new leader was already established the old leader becomes a server to which the new leader can assign clients.

    -Knowledge needed of each component in order to live up to the desired behaviour:
        -Clients must know about the leader.
        -Clients must store requests that have not received 'finished' confirmations.
        -Servers must know about other servers, the leader and their direct clients.
        -Servers must store client requests in a temp log until the transaction is confirmed to be finished (since the leader needs to process the request and this could fail).
        -Leader must be aware of every client and server.

Unfortunately we did not get to the robustness part and this is left for another team to implement.
