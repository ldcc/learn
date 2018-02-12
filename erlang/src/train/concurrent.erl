-module(concurrent).
-export([po1/1, pi1/0, start_local/0]).
-export([po2/2, pi2/0, start_po/1, start_pi/0]).
-export([po3/2, pi3/0, start_dis/1]).

%% concurrent
po1(0) ->
    pi1 ! finished,
    io:format("~w finished~n", [po1]);
po1(N) ->
    pi1 ! {po1, self()},
    receive
        continues -> io:format("~w continues~n", [po1])
    end,
    po1(N - 1).

pi1() ->
    receive
        {po1, Process} -> 
            io: format("~w continues~n", [pi1]),
            Process ! continues,
            pi1();
        finished      -> 
            io:format("~w finished~n", [pi1])
    end.

start_local() ->
    register(pi1, spawn(concurrent, pi1, [])),
    spawn(concurrent, po1, [3]).

%% distributed programming
po2(0, Pi_Node) ->
    {pi2, Pi_Node} ! finished,
    io:format("~w finished~n", [po2]);
po2(N, Pi_Node) ->
    {pi2, Pi_Node} ! {po2, self()},
    receive
        continues -> io:format("~w continues~n", [po2])
    end,
    po2(N - 1, Pi_Node).

pi2() ->
    receive
        {po2, Process} -> 
            io: format("~w continues~n", [pi2]),
            Process ! continues,
            pi2();
        finished      -> 
            io:format("~w finished~n", [pi2])
    end.

start_pi() ->
    register(pi2, spawn(concurrent, pi2, [])).

start_po(Pi_Node) ->
    spawn(concurrent, po2, [3, Pi_Node]).

%% node controlled
po3(0, Pi_Node) ->
    {pi3, Pi_Node} ! finished,
    io:format("~w finished~n", [po3]);
po3(N, Pi_Node) ->
    {pi3, Pi_Node} ! {po3, self()},
    receive
        continues -> io:format("~w continues~n", [po3])
    end,
    po3(N - 1, Pi_Node).

pi3() ->
    receive
        {po3, Process} -> 
            io: format("~w continues~n", [pi3]),
            Process ! continues,
            pi3();
        finished      -> 
            io:format("~w finished~n", [pi3])
    end.

start_dis(Pi_Node) ->
    register(pi3, spawn(concurrent, pi3, [])),
    io:format("~w--------------~n~w--------------~n", [Pi_Node, node()]),
    spawn(Pi_Node, concurrent, po3, [3, node()]).
