# Distributed Systems in Erlang

## Healthy Properties

Properties of a sound distributed system:

* **Peer based**: no leaders, masters, special nodes or central services
* **Asynchronous**: resilient, expect failure, simple, loosely coupled
* **Easy to debug**: easy to interrogate, easy to determine state

## Sick Systems

Properties of a distributed system prone to failure:

* **Not peer based**: masters, central systems, special nodes, shared state, shared clock time
* **Synchronous communication**: transaction oriented systems
* **Synchronous**: reliant on time constraints
* **Hard to debug**: opaque, migrating process, monolithic

## Fallacies of Distributed Systems

Don't take any of these things for granted:

1. The network is reliable
2. Latency is zero
3. Bandwith is infinite
4. The network is secure
5. Topology doesn't change
6. There is one administrator
7. Transport cost is zero
8. The network is homogeneous

## Traditional Communication

* **Threads** communicate through **shared memory** using some **synchronisation primitives**
* Impossible to spread threads and share memory across the wire

Erlang takes another approach:

* Share nothing
* Copy everything
* Message passing between Process IDs

### Location Transparency

Erlang has the same message passing syntax whether the recipient is local or remote: `Pid ! message`

## Nodes and Clusters

Each `erl` shell fires up a node.  Each VM is a node.

    # Run node in the background
    $ erl -detached
    $ ps aux | grep beam
    xavier         74695   0.0  0.2  2490292  13952 s001  S+   11:27AM   0:00.14 /usr/local/Cellar/erlang/R15B03-1/lib/erlang/erts-5.9.3.1/bin/beam.smp -- -root /usr/local/Cellar/erlang/R15B03-1/lib/erlang -progname erl -- -home /Users/xavier --

Clusters are collection of nodes.  Nodes have transitive knowledge of each others.

    $ erl -sname a
    ...
    (a@voigtkampff)1> node().
    a@voigtkampff
    (a@voigtkampff)2> nodes().
    []
    (a@voigtkampff)3> net_adm:ping('b@voigtkampff').
    pong
    nodes().
    (a@voigtkampff)4> nodes().
    [b@voigtkampff]
    (a@voigtkampff)5>

Each cluster is identified by a cookie.  Nodes from a cluster may not ping nodes belonging to another cluster.

    auth:get_cookie().
    auth:set_cookie(atom).

To send a message to a node:

    {server, node} ! msg

Sending messages to all registered nodes:

    lists:foreach(fun(Node) -> {example, Node} ! self() end, nodes()).

## The EPMD Port Mapper Daemon

This process maps no names to ports the node listens to.
It listens on port 4369 and is unique per physical machine.
