%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2019, Sergey
%%% @doc
%%% Benchmarks for OTP stdlib key-value stores
%%% @end
%%% Created :  7 Sep 2019 by Sergey <me@seriyps.ru>

-module(bench_kv).

-export([maps/1,
         bench_maps/1,
         dict/1,
         bench_dict/1,
         orddict/1,
         bench_orddict/1,
         gb_tree/1,
         bench_gb_tree/1,
         keyfind/1,
         bench_keyfind/1,
         proplists/1,
         bench_proplists/1]).


maps(input) ->
    maps:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_maps(M) ->
    {maps:find(-1, M),
     maps:find(5000, M),
     maps:find(1000000, M)}.


dict(input) ->
    dict:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_dict(D) ->
    {dict:find(-1, D),
     dict:find(5000, D),
     dict:find(1000000, D)}.


gb_tree(input) ->
    gb_trees:from_orddict(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_gb_tree(T) ->
    {gb_trees:lookup(-1, T),
     gb_trees:lookup(5000, T),
     gb_trees:lookup(1000000, T)}.


orddict(input) ->
    orddict:from_list(
      [{K, K} || K <- lists:seq(1, 50000)]).

bench_orddict(D) ->
    {orddict:find(-1, D),
     orddict:find(5000, D),
     orddict:find(1000000, D)}.


keyfind(input) ->
    [{K, K} || K <- lists:seq(1, 50000)].

bench_keyfind(L) ->
    {lists:keyfind(-1, 1, L),
     lists:keyfind(5000, 1, L),
     lists:keyfind(1000000, 1, L)}.


proplists(input) ->
    [{K, K} || K <- lists:seq(1, 50000)].

bench_proplists(L) ->
    {proplists:get_value(-1, L),
     proplists:get_value(5000, L),
     proplists:get_value(1000000, L)}.
