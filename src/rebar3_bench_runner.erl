%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2019, Sergey
%%% @doc
%%% Run benchmarks
%%% @end
%%% Created :  7 Sep 2019 by Sergey <me@seriyps.ru>

-module(rebar3_bench_runner).

-export([run/3]).
-export([do_run/5]).

-export_type([sample/0, opts/0]).

-define(NS, 1000000000).
-define(HEAP_SIZE_MB, 5).

-type sample() :: #{memory => float(),
                    reductions => float(),
                    wall_time => float()}.

-type opts() :: #{duration => pos_integer(),
                  samples => pos_integer(),
                  warmup_duration => pos_integer(),
                  log_fun => fun( (string(), [any()]) -> any() )}.

-spec run(module(), atom(), opts()) -> [sample()].
run(Mod, Fun, Opts0) ->
    Opts = maps:merge(
             #{duration => 10,
               samples => 100,
               warmup_duration => 3,
               log_fun => fun io:format/2},
             Opts0),
    Ref = make_ref(),
    %% TODO: make it configurable?
    HeapSize = ?HEAP_SIZE_MB * 1024 * 1024 div erlang:system_info(wordsize),
    Pid = proc_lib:spawn_opt(?MODULE, do_run, [self(), Ref, Mod, Fun, Opts],
                            [link,
                             {priority, high},
                             {min_heap_size, HeapSize}
                            ]),
    receive
        {result, Pid, Ref, Result} ->
            Result
    end.

do_run(From, Ref, Mod, Fun, Opts) ->
    Res = with_setup(
            fun(St) ->
                    do_run(Mod, Fun, St, Opts)
            end, Mod, Fun),
    From ! {result, self(), Ref, Res}.

do_run(Mod, Fun, St, Opts) ->
    %% warmup
    log(Opts, "Warmup for ~ws~n", [maps:get(warmup_duration, Opts)]),
    Input = input(Mod, Fun, St),
    WarmupRuns = warmup(Mod, Fun, Input, St, Opts),
    log(Opts, "Bench function called ~p times during warmup~n", [WarmupRuns]),
    %% run
    NPerSample = decide_sample_n_runs(WarmupRuns, Opts),
    MaxDurationNs = maps:get(duration, Opts),
    NSamples = maps:get(samples, Opts),
    log(Opts, "Will run for ~ws: ~w samples, ~w iterations each~n",
        [MaxDurationNs, NSamples, NPerSample]),
    Start = erlang:monotonic_time(),
    Res = run_n_samples(Mod, Fun, Input, St, NPerSample, NSamples, []),
    Runtime = erlang:monotonic_time() - Start,
    log(Opts, "Real run time: ~wms~n",
        [erlang:convert_time_unit(Runtime, native, millisecond)]),
    Res.

%% Calls
%% `State = Mod:OptsFun(init)' before and
%% `Mod:OptsFun({stop, State})' after F
with_setup(F, Mod, Fun) ->
    St = opts_call(Mod, Fun, init, []),
    try F(St)
    after
        opts_call(Mod, Fun, {stop, St}, [])
    end.

input(Mod, Fun, St) ->
    opts_call(Mod, Fun, {input, St}, []).

opts_call(Mod, Name, Arg, Default) ->
    F = opts_fun(Mod, Name),
    try F(Arg)
    catch error:R when R == undef;
                       R == function_clause ->
            Default
    end.

opts_fun(Mod, Fun) ->
    "bench_" ++ NameS = atom_to_list(Fun),
    Name = list_to_atom(NameS),
    fun Mod:Name/1.


%% == Warmup ==
%% Try to warmup CPU/memory for 3 seconds & collect data to adjust chunk sizes
warmup(Mod, Fun, Input, St, #{duration := MaxDurationS,
                              warmup_duration := WarmupDurationS,
                              samples := NSamples} = Opts) ->
    %% Trying to adjust N calls per run to make one run_n in 10ms
    SeedIters = 50,
    Start = erlang:monotonic_time(),
    #{wall_time := PerIter} = run_n(Mod, Fun, Input, St, SeedIters),
    TotalRuntime = erlang:monotonic_time() - Start,
    %% If we can't run benchmark at least 10 times during warmup, warmup
    %% duration should be increased
    ((PerIter * 10) < erlang:convert_time_unit(WarmupDurationS, second, native))
        orelse error(too_short_warmup),
    BenchRuntime = PerIter * SeedIters,
    Overhead = TotalRuntime - BenchRuntime,
    %% We want one warmup loop to run for the same time as single benchmark
    %% sample, but not more than 0.5s
    DesiredMs = min(500, MaxDurationS * 1000 div NSamples),
    Desired = erlang:convert_time_unit(DesiredMs, millisecond, native),
    TimeToRun = Desired - Overhead,
    ChunkSize = max(10, round(TimeToRun / PerIter)),
    log(Opts, "TotalRuntime: ~p  "
              "PerIter: ~p  "
              "Overhead: ~p  "
              "TimeToRun: ~p  "
              "ChunkSize: ~p~n",
              [TotalRuntime, PerIter, Overhead, TimeToRun, ChunkSize]),
    erlang:send_after(WarmupDurationS * 1000, self(), warmup_end),
    warmup_loop(Mod, Fun, Input, St, 0, ChunkSize).

warmup_loop(Mod, Fun, Input, St, N, PerIter) ->
    receive
        warmup_end ->
            N
    after 0 ->
            run_n(Mod, Fun, Input, St, PerIter),
            warmup_loop(Mod, Fun, Input, St, N + PerIter, PerIter)
    end.

decide_sample_n_runs(WarmupRuns, #{duration := MaxDuration,
                                   warmup_duration := WarmupDuration,
                                   samples := NSamples}) ->
    %% WarmupRuns - how many times we managed to call the function during 3s
    %% warmup, including overhead
    MaxDurationNs = MaxDuration * ?NS,
    MaxSampleDurationNs = MaxDurationNs / NSamples,
    WarmupDurationNs = erlang:convert_time_unit(
                          WarmupDuration, second, nanosecond),
    OneCallDurationNs = WarmupDurationNs / WarmupRuns,
    round(MaxSampleDurationNs / OneCallDurationNs).

%% == Main run ==
%% Run `run_n` collecting `Sample` samples
run_n_samples(_Mod, _Fun, _Input, _St, _NPerSample, 0, Acc) ->
    Acc;
run_n_samples(Mod, Fun, Input, St, NPerSample, Sample, Acc0) ->
    Acc = [run_n(Mod, Fun, Input, St, NPerSample) | Acc0],
    run_n_samples(Mod, Fun, Input, St, NPerSample, Sample - 1, Acc).

%% Run inner tight loop by calling Mod:Fun(Input) N times and taking
%% measurements before and after.
run_n(Mod, Fun, Input, St, N) ->
    garbage_collect(self()),
    StartProcInfo = proc_collect(),
    F = fun Mod:Fun/2,
    Start = erlang:monotonic_time(),
    ok = do_run_n(F, Input, St, N),
    End = erlang:monotonic_time(),
    EndProcInfo = proc_collect(),
    diff(N,
         StartProcInfo#{wall_time => Start},
         EndProcInfo#{wall_time => End}).

%% Inner tight loop
do_run_n(_, _, _, 0) ->
    ok;
do_run_n(F, Input, St, N) ->
    F(Input, St),
    do_run_n(F, Input, St, N - 1).

%% == Helpers ==
proc_collect() ->
    maps:from_list(
      process_info(self(), [memory, reductions])).

diff(N, ProcStart, ProcEnd) ->
    maps:map(
      fun(K, V) ->
              (V - maps:get(K, ProcStart)) / N
      end, ProcEnd).

log(#{log_fun := L}, Fmt, Args) ->
    L(Fmt, Args).
