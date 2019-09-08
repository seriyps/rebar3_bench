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

-define(WARMUP_MS, 3000).
-define(NS, 1000000000).

-type sample() :: #{memory => float(),
                    reductions => float(),
                    wall_time => float()}.

-type opts() :: #{duration => pos_integer(),
                  samples => pos_integer(),
                  log_fun => fun( (string(), [any()]) -> any() )}.

-spec run(module(), atom(), opts()) -> [sample()].
run(Mod, Fun, Opts) ->
    Ref = make_ref(),
    Pid = proc_lib:spawn_opt(?MODULE, do_run, [self(), Ref, Mod, Fun, Opts],
                            [link,
                             {priority, high}%% ,
                             %% {min_heap_size, 1024}
                            ]),
    receive
        {result, Pid, Ref, Result} ->
            Result
    end.

do_run(From, Ref, Mod, Fun, Opts) ->
    %% warmup
    log(Opts, "Warmup for ~ws~n", [round(?WARMUP_MS / 1000)]),
    Input = input(Mod, Fun),
    WarmupRuns = warmup(Mod, Fun, Input, Opts),
    log(Opts, "Bench function called ~p times during warmup~n", [WarmupRuns]),
    %% run
    NPerSample = decide_sample_n_runs(WarmupRuns, Opts),
    MaxDurationNs = maps:get(duration, Opts, 10),
    NSamples = maps:get(samples, Opts, 100),
    log(Opts, "Will run for ~ws: ~w samples, ~w iterations each~n",
        [MaxDurationNs, NSamples, NPerSample]),
    Start = erlang:monotonic_time(),
    Res = run_n_samples(Mod, Fun, Input, NPerSample, NSamples, []),
    Runtime = erlang:monotonic_time() - Start,
    log(Opts, "Real run time: ~wms~n",
        [erlang:convert_time_unit(Runtime, native, millisecond)]),
    From ! {result, self(), Ref, Res}.

input(Mod, Fun) ->
    "bench_" ++ NameS = atom_to_list(Fun),
    Name = list_to_atom(NameS),
    try Mod:Name(input)
    catch error:undef ->
            []
    end.

%% == Warmup ==
%% Try to warmup CPU/memory for 3 seconds & collect data to adjust chunk sizes
warmup(Mod, Fun, Input, Opts) ->
    %% Trying to adjust N calls per run to make one run_n in 10ms
    Start = erlang:monotonic_time(),
    #{wall_time := PerIter} = run_n(Mod, Fun, Input, 50),
    Runtime = erlang:monotonic_time() - Start,
    BenchRuntime = PerIter * 10,
    Overhead = Runtime - BenchRuntime,
    Desired = erlang:convert_time_unit(10, millisecond, native),
    TimeToRun = Desired - Overhead,
    ChunkSize = max(10, round(TimeToRun / PerIter)),
    log(Opts, "Runtime: ~p  "
              "PerIter: ~p  "
              "Overhead: ~p  "
              "TimeToRun: ~p  "
              "ChunkSize: ~p~n",
              [Runtime, PerIter, Overhead, TimeToRun, ChunkSize]),
    erlang:send_after(?WARMUP_MS, self(), warmup_end),
    warmup_loop(Mod, Fun, Input, 0, ChunkSize).

warmup_loop(Mod, Fun, Input, N, PerIter) ->
    receive
        warmup_end ->
            N
    after 0 ->
            run_n(Mod, Fun, Input, PerIter),
            warmup_loop(Mod, Fun, Input, N + PerIter, PerIter)
    end.

decide_sample_n_runs(WarmupRuns, Opts) ->
    %% WarmupRuns - how many times we managed to call the function during 3s
    %% warmup, including overhead
    MaxDurationNs = maps:get(duration, Opts, 10) * ?NS,
    NSamples = maps:get(samples, Opts, 100),
    MaxSampleDurationNs = MaxDurationNs / NSamples,
    WarmupDurationNs = erlang:convert_time_unit(
                          ?WARMUP_MS, millisecond, nanosecond),
    OneCallDurationNs = WarmupDurationNs / WarmupRuns,
    round(MaxSampleDurationNs / OneCallDurationNs).

%% == Main run ==
%% Run `run_n` collecting `Sample` samples
run_n_samples(_Mod, _Fun, _Input, _NPerSample, 0, Acc) ->
    Acc;
run_n_samples(Mod, Fun, Input, NPerSample, Sample, Acc0) ->
    Acc = [run_n(Mod, Fun, Input, NPerSample) | Acc0],
    run_n_samples(Mod, Fun, Input, NPerSample, Sample - 1, Acc).

%% Run inner tight loop by calling Mod:Fun(Input) N times and taking
%% measurements before and after.
run_n(Mod, Fun, Input, N) ->
    garbage_collect(self()),
    StartProcInfo = proc_collect(),
    Start = erlang:monotonic_time(),
    F = fun Mod:Fun/1,
    do_run_n(F, Input, N),
    End = erlang:monotonic_time(),
    EndProcInfo = proc_collect(),
    diff(N,
         StartProcInfo#{wall_time => Start},
         EndProcInfo#{wall_time => End}).

%% Inner tight loop
do_run_n(_, _, 0) ->
    ok;
do_run_n(F, St, N) ->
    F(St),
    do_run_n(F, St, N - 1).

%% == Helpers ==
proc_collect() ->
    maps:from_list(
      process_info(self(), [memory, reductions])).

diff(N, ProcStart, ProcEnd) ->
    maps:map(
      fun(K, V) ->
              (V - maps:get(K, ProcStart)) / N
      end, ProcEnd).

log(Opts, Fmt, Args) ->
    L = maps:get(log_fun, Opts, fun io:format/2),
    L(Fmt, Args).
