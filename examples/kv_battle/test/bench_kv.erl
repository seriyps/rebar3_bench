%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2019, Sergey
%%% @doc
%%% Benchmarks for OTP stdlib key-value stores
%%% @end
%%% Created :  7 Sep 2019 by Sergey <me@seriyps.ru>

-module(bench_kv).

-export([maps/1,
         bench_maps/2,
         dict/1,
         bench_dict/2,
         orddict/1,
         bench_orddict/2,
         gb_tree/1,
         bench_gb_tree/2,
         keyfind/1,
         bench_keyfind/2,
         proplists/1,
         bench_proplists/2,
         ets/1,
         bench_ets/2]).


maps({input, _}) ->
    maps:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_maps(M, _) ->
    {maps:find(-1, M),
     maps:find(5000, M),
     maps:find(1000000, M)}.


dict({input, _}) ->
    dict:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_dict(D, _) ->
    {dict:find(-1, D),
     dict:find(5000, D),
     dict:find(1000000, D)}.


gb_tree({input, _}) ->
    gb_trees:from_orddict(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_gb_tree(T, _) ->
    {gb_trees:lookup(-1, T),
     gb_trees:lookup(5000, T),
     gb_trees:lookup(1000000, T)}.


orddict({input, _}) ->
    orddict:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_orddict(D, _) ->
    {orddict:find(-1, D),
     orddict:find(5000, D),
     orddict:find(1000000, D)}.


keyfind({input, _}) ->
    [{K, K} || K <- lists:seq(1, 50000)].

bench_keyfind(L, _) ->
    {lists:keyfind(-1, 1, L),
     lists:keyfind(5000, 1, L),
     lists:keyfind(1000000, 1, L)}.


proplists({input, _}) ->
    [{K, K} || K <- lists:seq(1, 50000)].

bench_proplists(L, _) ->
    {proplists:get_value(-1, L),
     proplists:get_value(5000, L),
     proplists:get_value(1000000, L)}.


ets(init) ->
    ets:new(?MODULE, [private]);
ets({stop, Tab}) ->
    ets:delete(Tab);
ets({input, Tab}) ->
    [ets:insert(Tab, {K, K}) || K <- lists:seq(1, 50000)],
    ok.

bench_ets(_, Tab) ->
    {ets:lookup(-1, Tab),
     ets:lookup(5000, Tab),
     ets:lookup(1000000, Tab)}.
