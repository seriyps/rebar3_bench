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
do(State) ->
    {CliOpts, _} = rebar_state:command_parsed_args(State),
    Benches = find_benches(State, CliOpts),
    run_benches(Benches, CliOpts),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("fmt_err(~p)", [Reason]).

%% Private

opts() ->
    [{dir, $d, "dir", string,
      "directory where the benchmark tests are located (defaults to \"test\")"},
     {module, $m, "module", string,
      "name of one or more modules to run (comma-separated)"},
     {benches, $b, "bench", string,
      "name of benchmark to run within a specified module (comma-separated)"},
     {duration, $t, "duration", integer,
      "duration of single benchmark (default is 10s)"},
     {samples, $s, "samples", integer,
      "number of samples to collect and analyze (default is 100)"},
     {confidence, $c, "confidence", integer,
      "confidence level: 80, 90, 95, 98, 99 (default is 95)"},
     {parameter, $p, "parameter", string,
      "which parameter to measure: wall_time, memory, reductions (default wall_time)"}].

run_benches(Benches, OptsL) ->
    Opts0 = maps:from_list(OptsL),
    Opts = Opts0#{log_fun => fun rebar_api:debug/2},
    lists:foreach(
      fun({Mod, Fun}) ->
              rebar_api:info("Testing ~s:~s()", [Mod, Fun]),
              Samples = rebar3_bench_runner:run(Mod, Fun, Opts),
              Stats = stats(Mod, Fun, Samples, Opts),
              report(Mod, Fun, Stats, Opts)
      end, Benches).

report(_Mod, _Fun, Stats, Opts) ->
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
      "Bootstrapped~n"
      "Mean:           ~ts~n"
      "Std deviation:  ~ts~n"
      "Outliers:      ~w/~w = ~w~n"
      "Outlier variance: ~13g (~s)",
      [Param,
       unit(Min, Param), unit(Perc25, Param), unit(Median, Param),
       unit(Perc75, Param), unit(Max, Param), unit(Mean, Param),
       unit(StdDev, Param),
       OutliersLow, OutliersHigh, OutliersAll, OutVar, Severity]),
    (severe == Severity)
        andalso rebar_api:warn(
                  "Outlier variance is too high! Benchmark results might be "
                  "non-representative. Try to repeat the benchmarks in more "
                  "calm environment (no heavy background OS tasks) or "
                  "run benchmark for a longer time", []).

stats(Mod, Fun, Samples, Opts) ->
    CI = maps:get(confidence, Opts, 95) * 1.0,
    Param = parameter(maps:get(parameter, Opts, "wall_time")),
    Name = atom_to_list(Mod) ++ ":" ++ atom_to_list(Fun),
    RawSamples = [maps:get(Param, S) || S <- Samples],
    rebar_api:debug("Raw ~s samples: ~p", [Param, RawSamples]),
    Vitals = vitals(Name, RawSamples, CI),
    with_outliers(RawSamples, Vitals).

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


unit(Native, wall_time) ->
    Ns = erlang:convert_time_unit(round(Native), native, nanosecond),
    if Ns < 10000 ->
            io_lib:format("~7.2fns", [float(Ns)]);
       Ns < 10000000 ->
            io_lib:format("~7.2fμs", [Ns / 1000]);
       Ns < 10000000000 ->
            io_lib:format("~7.2fms", [Ns / 1000000])
    end;
unit(Words, memory) ->
    WS = erlang:system_info(wordsize),
    Bytes = Words * WS,
    if Bytes < 1024 ->
            io_lib:format("~4wb", [round(Bytes)]);
       true ->
            io_lib:format("~4wkb", [round(Bytes / 1024)])
    end;
unit(Reds, reductions) ->
    if Reds < 1000 ->
            integer_to_list(round(Reds));
       Reds < 1000000 ->
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
    compile_dirs(State, Dir, Dirs),
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

compile_dirs(State, _TestDir, Dirs) -> % [{App, Dir}]
    %% Set up directory -- may need to unlink then re-link
    %% copy contents into directory
    %% call the compiler
    [begin
       rebar_api:debug("Compiling ~s for benchmark", [AppName]),
       setup(State, OutDir),
       compile(State, AppName, Dir, OutDir)
     end || {{AppName, OutDir}, Dir} <- Dirs],
    rebar_api:debug("App compiled", []).

setup(_State, OutDir) ->
    filelib:ensure_dir(filename:join([OutDir, "dummy.beam"])).

compile(State, AppName, Src, Out) ->
    Opts = case AppName of
        <<"root">> -> rebar_state:opts(State);
        _ -> rebar_app_info:opts(find_app(AppName, State))
    end,
    rebar_api:debug("Compiling files in ~s to ~s", [Src, Out]),
    NewOpts = lists:foldl(fun({K, V}, Dict) -> rebar_opts:set(Dict, K, V) end,
                          Opts,
                          [{src_dirs, ["."]}]),
    IncludeOpts = add_includes(NewOpts, State),
    rebar_erlc_compiler:compile(IncludeOpts, Src, ec_cnv:to_list(Out)).

find_app(AppName, State) ->
    [App] = [App || App <- rebar_state:project_apps(State),
                    rebar_app_info:name(App) =:= AppName],
    App.

add_includes(NewOpts, State) ->
    Includes = lists:flatmap(fun app_includes/1, rebar_state:project_apps(State)),
    dict:append_list(erl_opts, Includes, NewOpts).

app_includes(App) ->
    Opts = rebar_app_info:opts(App),
    Dir = rebar_app_info:dir(App),
    [{i, filename:join(Dir, Src)} || Src <- rebar_dir:all_src_dirs(Opts, ["src"], [])]
    ++
    [{i, filename:join(Dir, "include")},
     {i, Dir}]. % not sure for that one, but mimics the rebar3_erlc_compiler


benches(any, Mod) ->
    [{Mod, Bench} || {Bench,1} <- Mod:module_info(exports), bench_prefix(Bench)];
benches(Benches, Mod) ->
    [{Mod, Bench} || {Bench,1} <- Mod:module_info(exports),
                    lists:member(atom_to_list(Bench), Benches)].

bench_prefix(Atom) ->
    lists:prefix("bench_", atom_to_list(Atom)).


module(File) ->
    list_to_atom(filename:basename(File, ".erl")).
