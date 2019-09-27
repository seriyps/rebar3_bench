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
    Opts1 = maps:merge(
              #{duration => 10,
                samples => 100,
                warmup_duration => 3,
                log_fun => fun io:format/2},
              Opts0),
    %% We use native timeunits inside
    Opts = maps:map(fun(K, V) when K == duration;
                                   K == warmup_duration ->
                            erlang:convert_time_unit(V, second, native);
                       (_, V) -> V
                    end, Opts1),
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
    log(Opts, "Warmup for ~ws~n",
        [erlang:convert_time_unit(
           maps:get(warmup_duration, Opts), native, second)]),
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
warmup(Mod, Fun, Input, St, #{duration := MaxDuration,
                              warmup_duration := WarmupDuration,
                              samples := NSamples} = _Opts) ->
    MinLoops = 10,
    %% Desired single run_n time:
    %% - To be able to run run_n at least 10 times during warmup (i.e., if
    %%   warmup duration is 3s, it will be 3 / 10 = 0.3s
    %% - To have the same run_n time as it will be in bench run (i.e., if we
    %%   want to collect 100 samples during 10s, it will be 10 / 100 = 0.1s
    Desired = min(WarmupDuration div MinLoops,
                  MaxDuration div NSamples),
    MaxSeedSteps = 5,
    WarmupChunkSize = warmup_seed(Mod, Fun, Input, St, Desired, 1, MaxSeedSteps),
    erlang:send_after(
      erlang:convert_time_unit(WarmupDuration, native, millisecond),
      self(), warmup_end),
    warmup_loop(Mod, Fun, Input, St, 0, WarmupChunkSize).

warmup_seed(Mod, Fun, Input, St, DesiredRuntime, ChunkSize, I) ->
    Start = erlang:monotonic_time(),
    #{wall_time := _PerIter} = run_n(Mod, Fun, Input, St, ChunkSize),
    TotalRuntime = erlang:monotonic_time() - Start,
    %% Following calculation is not perfect, because TotalRuntime
    %% depends not only on ChunkSize, but it also have some constant overhead
    %% (measurements; GC time may depend on chunk size, but not necessarily
    %% linearly)
    PerIter = TotalRuntime div ChunkSize,
    Diff = DesiredRuntime - TotalRuntime,
    %% If Diff > 0 - we will increase ChunkSize; if Diff < 0 we decrease
    ChunkSizeDiff = Diff div PerIter,
    NewChunkSize = ChunkSize + ChunkSizeDiff,
    %% io:format("Desired: ~p; Real: ~p; Diff: ~p; ChunkSize: ~p\n",
    %%           [DesiredRuntime, TotalRuntime,
    %%            Diff, ChunkSize]),
    (NewChunkSize > 0) orelse
        error({too_small_batch, "TIP: Try to increase warmup or bench runtime"}),
    case should_recurse(ChunkSize, ChunkSizeDiff, I) of
        true ->
            warmup_seed(Mod, Fun, Input, St, DesiredRuntime, NewChunkSize, I - 1);
        false ->
            NewChunkSize
    end.

warmup_loop(Mod, Fun, Input, St, N, PerIter) ->
    receive
        warmup_end ->
            N
    after 0 ->
            run_n(Mod, Fun, Input, St, PerIter),
            warmup_loop(Mod, Fun, Input, St, N + PerIter, PerIter)
    end.

should_recurse(_, _, 0) ->
    %% Too many attempts
    false;
should_recurse(Size, Diff, _) ->
    %% Diff from desired is more than 5%
    DiffPercent = (100 * abs(Diff) / Size),
    %% io:format("Size: ~p, Diff: ~p; Percent: ~p~n", [Size, Diff, DiffPercent]),
    DiffPercent > 5.

decide_sample_n_runs(WarmupRuns, #{duration := MaxDuration,
                                   warmup_duration := WarmupDuration,
                                   samples := NSamples}) ->
    %% WarmupRuns - how many times we managed to call the function during 3s
    %% warmup, including overhead
    MaxSampleDuration = MaxDuration / NSamples,
    OneCallDuration = WarmupDuration / WarmupRuns,
    round(MaxSampleDuration / OneCallDuration).

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
    F = fun Mod:Fun/2,
    StartProcInfo = proc_collect(),
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
