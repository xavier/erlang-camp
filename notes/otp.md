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

A timeout can be defined:

    receive
      {foo, Msg} ->
        Msg
      after 1000
        not_found
    end.

Sending and receiving messages:

    1> Pid = spawn(fun() -> receive From -> From ! gotit end end).
    <0.35.0>
    2> Pid ! self().
    <0.31.0>
    3> receive X -> X end.
    gotit
    4>

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

## The gen-server interface

* `init/1` to initialize the server
* `handle_call/3` for synchronous messaging
* `handle_cast/2` for asynchronous messaging (send and pray)

Implementing our server using gen-server:

    gen_server:cast(Pid, {store, Key, Value})
    -> handle_cast({store, Key, Value}, State)

    % We don't need to pass From
    gen_server:cast(Pid, {lookup, Key})
    -> handle_cast({lookup, Key}, State)

## Application

An **Application** is a a collection of modules which together perform some kind of functionality.

* Active: they start/top, they have a state (e.g. gen_server)
* Library: no state (e.g. utilities, ...)

Applications are grouped together into **releases**.

Directory layout:

* `application_name[-version]`
  * `doc`
  * `ebin` (required, created by `rebar compile`)
  * `include`
  * `priv`
  * `src` (required)


The compilation process creates `ebin/<app-name>.app` which describes how to start up the application.

## Supervision

Supervisors define:

* hierarchy
* restart strategy

Restart strategies:

* one for one: if child dies, restart it automatically
* one for all: if child dies, restart all children automatically
* rest for all: if child dies, all children started after him will be restarted

It's possible to limit the number of restarts

`start_link` means "I'm **linking** this new process to my current process"

Summary of the whole process:

1. `twit.app` start the application (via `application:start(twit)`).
2. The `twit_app` module invokes the supervisor module
3. The `twit_sup` starts up the server
4. The `twit_server` is fired up


`appmon:start().` fires up a Tk interface to monitor the processes.

## Logging

    error_logger:error_msg(Format) -> ok
    error_logger:error_msg(Format, Data) -> ok

    error_logger:warning_msg(Format) -> ok
    error_logger:warning_msg(Format, Data) -> ok

    error_logger:info_msg(Format) -> ok
    error_logger:info_msg(Format, Data) -> ok

`application:start(sasl).` logs events on the console with additional information.

## gen_events

* `handle_event/2`
* `gen_event:add_handler/3`
* `gen_event:delete_handler/3`

Can be used e.g. to override the logging for some events matching a given pattern.