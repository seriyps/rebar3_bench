rebar3 bench
============

Rebar3 microbenchmark plugin

Was inspired by Haskell [criterion](https://hackage.haskell.org/package/criterion) and
Rust's [criterion.rs](https://crates.io/crates/criterion).
Parts of implementation were borrowed from [rebar3 proper](https://hex.pm/packages/rebar3_proper) plugin.
It relies on [eministat](https://hex.pm/packages/eministat) for statistical calculations.

Use
---

Add the plugin to your rebar config:

```
{project_plugins, [rebar3_bench]}.
```

Add `bench_*` modules to `"test"` directory of your application:

```
my_app/
├── README.md
├── rebar.config
├── src
└── test
    ├── bench_codec.erl
    ├── bench_kv.erl
    └── bench_calc.erl
```

Each module should have one or more exported `bench_*` functions accompanied by optional benchmark
options functions:

```
-export([reverse/1,
         bench_reverse/2,
         sort/1,
         bench_sort/2]).

reverse({input, _}) ->
    lists:seq(1, 1000).

bench_reverse(List, _) ->
    lists:reverse(List).


sort({input, _}) ->
    lists:seq(1, 1000).

bench_sort(List, _) ->
    lists:sort(List).


server(init) ->
    % init is called only once in the same process where benchmark will be running
    % at the very beginning of benchmark.
    % Value returned from this callback will be passed to server({input, State}),
    % bench_server(_, State) and server({stop, State})
    my_server:start();
server({input, _Server}) ->
    % This callback is called after `init` to generate benchmark input data.
    % Returned value will be passed to bench_server(Input, _)
    binary:copy(<<1>>, 1024);
server({stop, Server}) ->
    % Called only once at the very end of benchmark
    my_server:stop(Server).

bench_server(BinaryInput, Server) ->
    my_server:send(BinaryInput, Server),
    BinaryInput = my_server:recv(Server).
```

Options function will be normally called once before and after benchmark and `bench_` function will be
called lots of times.
Each benchmark is executed in a separate process with `{priority, high}, {min_heap_size, 5mb}`.
Garbage-collection is forced before time measurement is started, but if your benchmark
function produces a lot of heap data and benchmark duration (`-t` option) is high enough, it's
still possible that garbage collection will be triggered during benchmark execution.

It's importand that input and benchmark are as deterministic as possible. Try to not depend on
use of random generators, time or other side effects.

Then just call your plugin directly in an existing application (it will auto-discover your benchmarks):

```
$ rebar3 bench
===> Testing bench_kv:bench_maps()
===> Stats for wall_time
Min:              133.00ns
25 percentile:    133.00ns
Median:           134.00ns
75 percentile:    135.00ns
Max:              174.00ns
Outliers:       Lo: 0; Hi: 8; Sum: 8
Outlier variance:      0.316545 (moderate)
> Bootstrapped
Mean:             135.00ns
Std deviation:      5.00ns
```

Please, refer to [eministat README](https://github.com/jlouis/eministat#description-of-the-output)
to find out what does the output mean.

This will collect 100 samples, dump them to special directory under `_build` and calculates
statistics over them.

If you make your optimizations and run the same command again, it will
again collect 100 samples using your new code, calculates statistics, comparing
results with previous dump and will try to statistically prove that there exists
significant difference between results of two runs - if there is an improvement or
regression:

```
$ rebar3 bench
===> Testing bench_kv:bench_maps()
===> Stats for wall_time
Min:                133.00ns (+    0.00ns /  0.3%)
25 percentile:      134.00ns (+    1.00ns /  0.4%)
Median:             134.00ns (-    0.00ns /  0.1%)
75 percentile:      135.00ns (-    7.00ns /  4.9%)
Max:                146.00ns (-    0.00ns /  0.1%)
Outliers:         Lo: 0; Hi: 5; Sum: 5
Outlier variance: 9.90000e-3 (unaffected)
> Bootstrapped
Mean:               135.00ns (-    2.00ns /  1.6%)
Std deviation:        2.00ns
> Relative
Difference at 95.0 confidence
    -2.00ns ±     1.00ns
    -1.60%  ±  0.77%
 (Student's t, pooled s = 3.21558)
```

The most important field to pay attention is `Mean`: `135.00ns (-    2.00ns /  1.6%)` means
that the runtime of the single call to benchmarked function decreased by 2ns (1.6%), which
is a very minor improvement.

However, in the section below `>Relative` the statistical proof says, that this improvement is
not caused by random fluctuations, but is quite stable - "proven".

Another important data point is `Outlier variance`. If the value is more than ~0.5, it means
that the runtime of benchmarked function varies a lot between calls. So you need to either
simplify this function, remove side-effects from it or increase the benchmark runtime to
smoothen the influence of side-effects.

### Example workflow

You can use `--dump-baseline` and `--baseline` to manage benchmark samples dumps.

```
git checkout master
# run benchmark and store results in "master" file
rebar3 bench --save-baseline master
git checkout optimizations
# run benchmark and compare results with data from "master" file
rebar3 bench --baseline master

# Do some optimizations

# run again and compare with "master" data to see if optimizations do work
rebar3 bench --baseline master
```

### Cover mode

It might be interesting to see which lines of code your benchmark actually covers and what are
the most "hot" code paths. You can run your benchmark with "cover" enabled:
`rebar3 bench --cover`. It will run your benchmarks and will write coverage data to rebar3
standard destination, you can use `rebar3 cover` to see the report.
We don't do any measurements calculate statistics or write baseline files in `cover` mode,
because cover-compiled code is much slower.

### Available options

```
$ rebar3 help bench
Run microbenchmarks from callback modules
Usage: rebar3 bench [-d [<dir>]] [-m <module>] [-b <benches>]
                    [-t [<duration>]] [-n [<samples>]]
                    [--confidence [<confidence>]] [-c [<cover>]]
                    [-p [<parameter>]]
                    [--save-baseline [<save_baseline>]]
                    [--baseline [<baseline>]]

  -d, --dir          directory where the benchmark tests are located 
                     [default: test]
  -m, --module       name of one or more modules to run (comma-separated)
  -b, --bench        name of benchmark to run within a specified module 
                     (comma-separated)
  -t, --duration     duration of single benchmark, in seconds [default: 10]
  -n, --num-samples  number of samples to collect and analyze [default: 
                     100]
  --confidence       confidence level: 80, 90, 95, 98, 99 [default: 95]
  -c, --cover        run benchmarks in coverage mode (no measurements are 
                     made), generate cover data [default: false]
  -p, --parameter    which parameter to measure: wall_time, memory, 
                     reductions [default: wall_time]
  --save-baseline    save benchmark data to file with this name [default: 
                     _tip]
  --baseline         use data stored in file as baseline [default: _tip]
```
