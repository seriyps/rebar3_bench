%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2019, Sergey
%%% @doc
%%% Benchmarks for OTP stdlib key-value stores
%%% @end
%%% Created :  7 Sep 2019 by Sergey <me@seriyps.ru>

-module(bench_kv).

-export([bench_maps/1,
         maps/2,
         bench_dict/1,
         dict/2,
         bench_orddict/1,
         orddict/2,
         bench_gb_tree/1,
         gb_tree/2,
         bench_keyfind/1,
         keyfind/2,
         bench_proplists/1,
         proplists/2,
         bench_ets/1,
         ets/2]).


bench_maps({input, _}) ->
    maps:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

maps(M, _) ->
    {maps:find(-1, M),
     maps:find(5000, M),
     maps:find(1000000, M)}.


bench_dict({input, _}) ->
    dict:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

dict(D, _) ->
    {dict:find(-1, D),
     dict:find(5000, D),
     dict:find(1000000, D)}.


bench_gb_tree({input, _}) ->
    gb_trees:from_orddict(
      [{K, K} || K <- lists:seq(1, 50000)]).

gb_tree(T, _) ->
    {gb_trees:lookup(-1, T),
     gb_trees:lookup(5000, T),
     gb_trees:lookup(1000000, T)}.


bench_orddict({input, _}) ->
    orddict:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

orddict(D, _) ->
    {orddict:find(-1, D),
     orddict:find(5000, D),
     orddict:find(1000000, D)}.


bench_keyfind({input, _}) ->
    [{K, K} || K <- lists:seq(1, 50000)].

keyfind(L, _) ->
    {lists:keyfind(-1, 1, L),
     lists:keyfind(5000, 1, L),
     lists:keyfind(1000000, 1, L)}.


bench_proplists({input, _}) ->
    [{K, K} || K <- lists:seq(1, 50000)].

proplists(L, _) ->
    {proplists:get_value(-1, L),
     proplists:get_value(5000, L),
     proplists:get_value(1000000, L)}.


bench_ets(init) ->
    ets:new(?MODULE, [private]);
bench_ets({stop, Tab}) ->
    ets:delete(Tab);
bench_ets({input, Tab}) ->
    [ets:insert(Tab, {K, K}) || K <- lists:seq(1, 50000)],
    ok.

ets(_, Tab) ->
    {ets:lookup(Tab, -1),
     ets:lookup(Tab, 5000),
     ets:lookup(Tab, 1000000)}.
