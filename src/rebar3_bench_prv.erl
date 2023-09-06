-module(rebar3_bench_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, bench).
-define(DEPS, [compile, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {profiles, [test]},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 bench"}, % How to use the plugin
            {opts, opts()},               % list of options understood by the plugin
            {short_desc, "Run microbenchmarks from callback modules."},
            {desc, "Run microbenchmarks from callback modules"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State0) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State0),
    Cover = proplists:get_value(cover, CliOpts, false),
    State = maybe_cover_compile(State0, Cover),
    BaselineFile = dump_file(proplists:get_value(baseline, CliOpts), State),
    Baseline = try_restore(BaselineFile, []),
    rebar_api:debug("Baselines loaded from file '~ts': ~w",
                    [BaselineFile, length(Baseline)]),
    Benches = find_benches(State, CliOpts),
    Samples = run_benches(Benches, Baseline, CliOpts),
    ok = maybe_write_coverdata(State, Cover),
    DumpFile = dump_file(proplists:get_value(save_baseline, CliOpts), State),
    case Cover of
        false ->
            rebar_api:debug("Writing sample data to '~ts'", [DumpFile]),
            dump(DumpFile, Samples);
        true ->
            rebar_api:debug("Not writing sample data - cover mode enabled", [])
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("fmt_err(~p)", [Reason]).

%% Private

opts() ->
    [{dir, $d, "dir", {string, "test"},
      "directory where the benchmark tests are located"},
     {module, $m, "module", string,
      "name of one or more modules to run (comma-separated)"},
     {benches, $b, "bench", string,
      "name of benchmark to run within a specified module (comma-separated)"},
     {duration, $t, "duration", {integer, 10},
      "duration of single benchmark, in seconds"},
     {samples, $n, "num-samples", {integer, 100},
      "number of samples to collect and analyze"},
     {confidence, undefined, "confidence", {integer, 95},
      "confidence level: 80, 90, 95, 98, 99"},
     {cover, $c, "cover", {boolean, false},
      "run benchmarks in coverage mode (no measurements are made), generate cover data"},
     {parameter, $p, "parameter", {string, "wall_time"},
      "which parameter to measure: wall_time, memory, reductions"},
     {no_stats, undefined, "no-stats", {boolean, false},
      "do not produce summary stats"},
     {save_baseline, undefined, "save-baseline", {string, "_tip"},
      "save benchmark data to file with this name"},
     {baseline, undefined, "baseline", {string, "_tip"},
      "use data stored in file as baseline"}].

run_benches(Benches, Baseline, OptsL) ->
    Opts0 = maps:from_list(OptsL),
    Opts = Opts0#{log_fun => fun rebar_api:debug/2},
    lists:map(
      fun({Mod, Fun}) ->
              rebar_api:info("Testing ~s:~s()", [Mod, Fun]),
              Samples = rebar3_bench_runner:run(Mod, Fun, Opts),
              rebar_api:debug("Raw samples:~n~p", [Samples]),
              case maps:get(cover, Opts, false) of
                  false ->
                      case maps:get(no_stats, Opts, false) of
                          true ->
                              noop;
                          false ->
                              Stats = stats(Mod, Fun, Samples, Baseline, Opts),
                              report(Stats, Opts)
                      end;
                  true ->
                      %% Don't print the report when `cover' option is provided
                      noop
              end,
              {{Mod, Fun}, Samples}
      end, Benches).

maybe_cover_compile(State, false) ->
    State;
maybe_cover_compile(State0, true) ->
    rebar_api:info("Running in cover mode (cover_enabled=true, no measurements)", []),
    State1 = rebar_state:set(State0, cover_enabled, true),
    ok = rebar_prv_cover:maybe_cover_compile(State1),
    State1.

maybe_write_coverdata(_State, false) ->
    ok;
maybe_write_coverdata(State, true) ->
    rebar_api:info("Cover data collected. Use `rebar3 cover` to analyze 'bench' section.", []),
    rebar_prv_cover:maybe_write_coverdata(State, ?PROVIDER).


%%
%% Stats calculation

stats(Mod, Fun, Samples, [], Opts) ->
    CI = maps:get(confidence, Opts, 95) * 1.0,
    Name = atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun),
    Param = parameter(maps:get(parameter, Opts, "wall_time")),
    RawSamples = [maps:get(Param, S) || S <- Samples],
    Vitals = vitals(Name, RawSamples, CI),
    with_outliers(RawSamples, Vitals);
stats(Mod, Fun, Samples, Baselines, Opts) ->
    Stats = stats(Mod, Fun, Samples, [], Opts),
    case proplists:get_value({Mod, Fun}, Baselines) of
        undefined ->
            Stats;
        Baseline ->
            with_baseline(Mod, Fun, Stats, Baseline, Opts)
    end.


parameter("wall_time") -> wall_time;
parameter("wall-time") -> wall_time;
parameter("memory") -> memory;
parameter("reductions") -> reductions.


vitals(Name, RawSamples, CI) ->
    Ds = eministat_ds:from_list(Name, RawSamples),
    case eministat_ds:variance(Ds) of
        V when V < 0.001 ->
            %% TODO: use some more stable "variance" algorithm
            rebar_api:warn("Data points are too close; "
                           "naive variance: ~p "
                           "2pass variance: ~p",
                           [V, variance_2pass(RawSamples)]);
        _ ->
            ok
    end,
    Bootstrapped = eministat_resample:resample([mean, std_dev], 10000, Ds),
    #{ds => Ds,
      n => length(RawSamples),
      min => eministat_ds:min(Ds),
      max => eministat_ds:max(Ds),
      percentiles =>
          maps:from_list([{N, eministat_ds:percentile(N / 100, Ds)}
                          || N <- [25, 50, 75]]),
      bootstrapped =>
          maps:from_list(eministat_resample:bootstrap_bca(CI / 100, Ds, Bootstrapped))}.

variance_2pass(RawSamples) ->
    N = length(RawSamples),
    SumX = lists:sum(RawSamples),
    Mean = SumX / N,
    Sum2 = lists:foldl(
             fun(X, Sum2) ->
                     Sum2 + (X - Mean) * (X - Mean)
             end, 0, RawSamples),
    Sum2 / (N - 1.0).

with_outliers(RawSamples,
              #{percentiles :=
                    #{25 := Perc25,
                      75 := Perc75}} = Vitals) ->
    IQR = Perc75 - Perc25,
    Ps = lists:sort(RawSamples),
    OutliersLow = eministat_resample:count(fun(P) -> P < (Perc25 - 1.5 * IQR) end, Ps),
    OutliersHigh =  eministat_resample:count(fun(P) -> P > (Perc75 + 1.5 * IQR) end, Ps),
    Vitals#{outliers => {OutliersLow, OutliersHigh}}.

with_baseline(Mod, Fun, #{ds := Ds} = Stats, Baseline, Opts) ->
    CI = maps:get(confidence, Opts, 95),
    BStats = #{ds := BDs} = stats(Mod, Fun, Baseline, [], Opts),
    Relative = eministat_analysis:relative(Ds, BDs, valid_ci(CI)),
    Stats#{baseline => BStats,
           relative => Relative}.

%%
%% Reporting

report(#{baseline := BStats, relative := Relative} = Stats, Opts) ->
    report_diff(Stats, BStats, Relative, Opts);
report(Stats, Opts) ->
    report_one(Stats, Opts).

report_one(Stats, Opts) ->
    Param = parameter(maps:get(parameter, Opts, "wall_time")),
    #{n := N,
      min := Min,
      max := Max,
      percentiles :=
          #{25 := Perc25,
            50 := Median,
            75 := Perc75},
      bootstrapped :=
          #{mean := #{pt := Mean},
            std_dev := #{pt := StdDev}},
      outliers := {OutliersLow, OutliersHigh}
     } = Stats,
    %% StdDev = eministat_ds:std_dev(Dataset),
    OutliersAll = OutliersLow + OutliersHigh,
    {OutVar, Severity} = eministat_analysis:outlier_variance(Mean, StdDev, N),
    rebar_api:info(
      "Stats for ~s~n"
      "Min:            ~ts~n"
      "25 percentile:  ~ts~n"
      "Median:         ~ts~n"
      "75 percentile:  ~ts~n"
      "Max:            ~ts~n"
      "Outliers:       Lo: ~w; Hi: ~w; Sum: ~w~n"
      "Outlier variance: ~13g (~s)~n"
      "> Bootstrapped~n"
      "Mean:           ~ts~n"
      "Std deviation:  ~ts~n",
      [Param,
       unit(Min, Param), unit(Perc25, Param), unit(Median, Param),
       unit(Perc75, Param), unit(Max, Param),
       OutliersLow, OutliersHigh, OutliersAll, OutVar, Severity,
       unit(Mean, Param), unit(StdDev, Param)
      ]),
    warn_severity(Severity).

report_diff(Stats, BStats, Relative, Opts) ->
    Param = parameter(maps:get(parameter, Opts, "wall_time")),
    #{n := N,
      min := Min,
      max := Max,
      percentiles :=
          #{25 := Perc25,
            50 := Median,
            75 := Perc75},
      bootstrapped :=
          #{mean := #{pt := Mean},
            std_dev := #{pt := StdDev}},
      outliers := {OutliersLow, OutliersHigh}
     } = Stats,
    #{min := BMin,
      max := BMax,
      percentiles :=
          #{25 := BPerc25,
            50 := BMedian,
            75 := BPerc75},
      bootstrapped :=
          #{mean := #{pt := BMean}}
     } = BStats,
    %% StdDev = eministat_ds:std_dev(Dataset),
    OutliersAll = OutliersLow + OutliersHigh,
    {OutVar, Severity} = eministat_analysis:outlier_variance(Mean, StdDev, N),
    rebar_api:info(
      "Stats for ~s~n"
      "Min:              ~ts (~ts)~n"
      "25 percentile:    ~ts (~ts)~n"
      "Median:           ~ts (~ts)~n"
      "75 percentile:    ~ts (~ts)~n"
      "Max:              ~ts (~ts)~n"
      "Outliers:         Lo: ~w; Hi: ~w; Sum: ~w~n"
      "Outlier variance: ~10g (~s)~n"
      "> Bootstrapped~n"
      "Mean:             ~ts (~ts)~n"
      "Std deviation:    ~ts~n"
      "> Relative~n"
      "~ts~n",
      [Param,
       unit(Min, Param), udiff(BMin, Min, Param),
       unit(Perc25, Param), udiff(BPerc25, Perc25, Param),
       unit(Median, Param), udiff(BMedian, Median, Param),
       unit(Perc75, Param), udiff(BPerc75, Perc75, Param),
       unit(Max, Param), udiff(BMax, Max, Param),
       OutliersLow, OutliersHigh, OutliersAll, OutVar, Severity,
       unit(Mean, Param), udiff(BMean, Mean, Param),
       unit(StdDev, Param),
       format_relative(Relative, Param)
      ]),
    warn_severity(Severity).

warn_severity(severe) ->
    rebar_api:warn(
      "Outlier variance is too high! Benchmark result might be "
      "non-representative. Try to repeat the benchmarks in more "
      "calm environment (no heavy background OS tasks) or "
      "run benchmark for a longer time", []);
warn_severity(_) ->
    ok.

format_relative({no_difference, CI}, _) ->
    io_lib:format("No difference found at ~.1f% confidence level", [CI]);
format_relative({difference, #{confidence_level := CI,
                               difference := {D, E},
                               difference_pct := {DP, EP},
                               pooled_s := SPool}}, Unit) ->
    io_lib:format(
      "Difference at ~.1f confidence~n"
      " ~ts ± ~ts~n"
      " ~8.2f%  ± ~5.2f%~n"
      " (Student's t, pooled s = ~g)",
      [CI,
       unit(D, Unit), unit(E, Unit),
       DP, EP,
       SPool]).


udiff(Base, Val, Unit) ->
    Diff = Val - Base,
    Percent = abs(100 * Diff / Base),
    Sign = case Diff < 0 of
               true -> $-;
               false -> $+
           end,
    [Sign, unit(abs(Diff), Unit) | io_lib:format(" / ~4.1f%", [Percent])].

unit(Native, wall_time) ->
    Ns = erlang:convert_time_unit(round(Native), native, nanosecond),
    Abs = abs(Ns),
    if Abs < 10000 ->
            io_lib:format("~8.2fns", [float(Ns)]);
       Abs < 10000000 ->
            io_lib:format("~8.2fμs", [Ns / 1000]);
       true ->
            io_lib:format("~8.2fms", [Ns / 1000000])
    end;
unit(Words, memory) ->
    WS = erlang:system_info(wordsize),
    Bytes = Words * WS,
    Abs = abs(Bytes),
    if Abs < 1024 ->
            io_lib:format("~4wB", [round(Bytes)]);
       Abs < 1024 * 1024 ->
            io_lib:format("~4wKiB", [round(Bytes / 1024)]);
       true ->
            io_lib:format("~4wMiB", [round(Bytes / (1024 * 1024))])
    end;
unit(Reds, reductions) ->
    Abs = abs(Reds),
    if Abs < 1000 ->
            integer_to_list(round(Reds));
       Abs < 1000000 ->
            io_lib:format("~4wk", [round(Reds / 1000)]);
       true ->
            io_lib:format("~4wm", [round(Reds / 1000000)])
    end.


%%
%% Discovery

find_benches(State, Opts) ->
    Dir = proplists:get_value(dir, Opts, "test"),
    Mods = maybe_parse_csv(proplists:get_value(module, Opts, any)),
    Benches = maybe_parse_csv(proplists:get_value(benches, Opts, any)),
    Found = find_benches(State, Dir, Mods, Benches),
    rebar_api:debug("Found: ~p", [Found]),
    {ModsFound0, BenchesFound0} = lists:unzip(Found),
    ModsFound = [atom_to_list(Mod) || Mod <- ModsFound0],
    BenchesFound = [atom_to_list(Bench) || Bench <- BenchesFound0],
    Benches =/= any andalso
        [throw({bench_not_found, Bench, Mods})
         || Bench <- Benches, not lists:member(Bench, BenchesFound)],
    Mods =/= any andalso
        [throw({module_not_found, Mod, Benches})
         || Mod <- Mods, not lists:member(Mod, ModsFound)],
    Found.

maybe_parse_csv(Atom) when is_atom(Atom) -> Atom;
maybe_parse_csv(Data) ->
    case is_atom_list(Data) of
        true -> [atom_to_list(D) || D <- Data];
        false -> parse_csv(Data)
    end.

is_atom_list([]) -> true;
is_atom_list([H|T]) when is_atom(H) -> is_atom_list(T);
is_atom_list(_) -> false.

parse_csv(IoData) ->
    re:split(IoData, ", *", [{return, list}]).

find_benches(State, Dir, Mods, Benches) ->
    rebar_api:debug("Dir: ~p", [Dir]),
    rebar_api:debug("Mods: ~p", [Mods]),
    rebar_api:debug("Benches: ~p", [Benches]),
    %% Fetch directories and app configs
    RawDirs = [{{rebar_app_info:name(App),
                 filename:join([rebar_app_info:out_dir(App), Dir])},
                filename:join(rebar_app_info:dir(App), Dir)}
               || App <- rebar_state:project_apps(State),
                  not rebar_app_info:is_checkout(App)],
    %% Pick a root test directory for umbrella apps
    UmbrellaDir =
        [{{<<"root">>,
           filename:join(rebar_dir:base_dir(State), "bench_"++Dir)},
         P} || P <- [make_absolute_path(filename:join([".", Dir]))],
               not lists:member(P, [D || {_,D} <- RawDirs])],
    TestDirs = RawDirs ++ UmbrellaDir,
    rebar_api:debug("SearchDirs: ~p", [TestDirs]),
    %% Keep directories with benches in them
    Dirs = [{App, TestDir}
            || {App, TestDir} <- TestDirs,
               {ok, Files} <- [file:list_dir(TestDir)],
               lists:any(fun(File) -> bench_suite(Mods, File) end, Files)],
    rebar_api:debug("Dirs: ~p", [Dirs]),
    [Bench || {_, TestDir} <- Dirs,
             {ok, Files} <- [file:list_dir(TestDir)],
             File <- Files,
             bench_suite(Mods, File),
             Bench <- benches(Benches, module(File))].


make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.

bench_suite(Mods, File) ->
    Mod = filename:basename(File, ".erl"),
    filename:extension(File) =:= ".erl"
    andalso
    ((Mods =:= any andalso lists:prefix("bench_", Mod))
     orelse
     (Mods =/= any andalso lists:member(Mod, Mods))).


benches(any, Mod) ->
    [{Mod, Bench} || {Bench, 2} <- Mod:module_info(exports), bench_prefix(Bench)];
benches(Benches, Mod) ->
    [{Mod, Bench} || {Bench, 2} <- Mod:module_info(exports),
                    lists:member(atom_to_list(Bench), Benches)].

bench_prefix(Atom) ->
    lists:prefix("bench_", atom_to_list(Atom)).


module(File) ->
    list_to_atom(filename:basename(File, ".erl")).


%%
%% Bench data loader/dumper

-define(DUMP_FMT_VERSION, [0, 1, 0]).
-define(DUMP_010_FIELDS, [wall_time, memory, reductions]).

-type dump_sample() :: {{module(), atom()}, rebar3_bench_runner:sample()}.

-spec dump(file:name_all(), [dump_sample()]) -> ok.
dump(FileName, RawSamples) ->
    ok = filelib:ensure_dir(FileName),
    ok = file:write_file(FileName, encode_dump(RawSamples)).

-spec try_restore(file:name_all(), Default) -> [dump_sample()] | Default when
      Default :: any().
try_restore(FileName, Default) ->
    try restore(FileName)
    catch throw:_ ->
            Default
    end.

restore(FileName) ->
    case file:read_file(FileName) of
        {ok, Bin} ->
            decode_dump(Bin);
        {error, enoent} ->
            throw(enoent)
    end.

-spec dump_file(undefined | string(), rebar_state:t()) -> file:name_all().
dump_file(undefined, State) ->
    dump_file("_tip", State);
dump_file(Name, State) ->
    filename:join(bench_dir(State), Name).

bench_dir(State) ->
    filename:join([rebar_dir:base_dir(State), "bench"]).


encode_dump(Runs) ->
    CompactRuns = lists:map(
      fun({{M, F}, Samples}) ->
              %% Convert list of maps to list of lists
              CompactSamples =
                  lists:map(
                    fun(Sample) ->
                            [maps:get(K, Sample) || K <- ?DUMP_010_FIELDS]
                    end, Samples),
              {M, F, CompactSamples}
      end, Runs),
    term_to_binary(
      {?DUMP_FMT_VERSION,
       CompactRuns}).

decode_dump(Bin) ->
    convert_dump(binary_to_term(Bin)).

convert_dump({?DUMP_FMT_VERSION, CompactRuns}) ->
    lists:map(fun({M, F, CompactSamples}) ->
                      {{M, F},
                       lists:map(
                         fun(Values) ->
                                 maps:from_list(lists:zip(?DUMP_010_FIELDS, Values))
                         end, CompactSamples)}
              end, CompactRuns).

%% FIXME:
valid_ci(80) -> 1;
valid_ci(90) -> 2;
valid_ci(95) -> 3;
valid_ci(98) -> 4;
valid_ci(99) -> 5;
valid_ci(_) -> error(badarg).
