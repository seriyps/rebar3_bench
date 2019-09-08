-module(rebar3_bench).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_bench_prv:init(State),
    {ok, State1}.
