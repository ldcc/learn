%%%-------------------------------------------------------------------
%%% @author ldcc
%%% @copyright (C) 2018, <ldcc>
%%% Created : 28. 二月 2018 9:51
%%%-------------------------------------------------------------------
-module(interface).
-author("ldcc").

%% API
-export([quux/1, behaviour_info/1]).

-callback foo(Foo :: term()) ->
    {ok, Result :: term()}.

-callback bar(Bar :: term()) ->
    {ok, Result :: term()}.

-callback baz(Baz :: term()) ->
    {ok, Result :: term()}.

-callback qux(Qux :: term()) ->
    {ok, Result :: term()}.

-callback quux(Quux :: term()) ->
    {ok, Result :: term()}.

-optional_callbacks([quux/1]).

quux(Quux) ->
    {ok, Quux}.

behaviour_info(all_callbacks) ->
    [{foo, 1}, {bar, 1}, {baz, 1}, {qux, 1}, {quux, 1}];
behaviour_info(callbacks) ->
    [{foo, 1}, {bar, 1}, {baz, 1}, {qux, 1}];
behaviour_info(optional_callbacks) ->
    [{quux, 1}];
behaviour_info(_) ->
    undefined.

