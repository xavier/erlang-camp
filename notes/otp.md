# OTP

Framework for writing idiomatic Erlang.

## Building Blocks

1. First we build the **gen-server**
2. Second we wrap it in an **application**
3. Lastly we add in **supervision** and **fault tolerance**

## Interprocess Communication

> Share nothing, copy everything

Each process has a **mailbox**, an asynchronous queue which can be searched.
No guarantee that the messages are delivered in the order they were sent.

Short of crashing the VM, processes cannot interfere with each others.

## Server Basics

Server as a state machine.

* A server is about input, state, output
* Transition function: input * state -> state'
* Output function: input * state -> output

A long-running server is just tail-recursive function whose accumulator is the state.

`register(atom, Pid)` gives a name to the process (unique per node)

`receive` pulls a message from the mailbox using a pattern

## Behaviors

Generics (container)

* starting the server
* initializing, registering
* carrying state in a loop

Specifics (implementation)

* API
* business logic

The API exposed by the module uses `container` to setup and communicate with the server process:

    container:start()
    container:send(process, message)

By **convention**, the container invokes the implementation using the following functions:

    init() ->
      {ok, initial_state}

    handle_msg(pattern, state) ->
      {ok, new_state}

## Supervision


