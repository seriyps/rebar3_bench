rebar3 bench
============

Rebar3 microbenchmark plugin

Was inspired by Rust's [criterion.rs](https://crates.io/crates/criterion).
Parts of implementation were borrowed from [rebar3 proper](https://hex.pm/packages/rebar3_proper) plugin.
It relies on [eministat](https://hex.pm/packages/eministat) for statistical calculations.

Use
---

Add the plugin to your rebar config:

```
{plugins, [rebar3_bench]}.
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
         bench_reverse/1,
         sort/1,
         bench_sort/1]).

reverse(input) ->
    lists:seq(1, 1000).

bench_reverse(List) ->
    lists:reverse(List).

sort(input) ->
    lists:seq(1, 1000).

bench_sort(List) ->
    lists:sort(List).
```

Options function will be called once at the beginning of benchmark and `bench_` function will be
called lots of times.

It's importand that input and benchmark are as deterministic as possible. Try to not depend on
use of random generators, time or other side effects.

Then just call your plugin directly in an existing application (it will auto-discover your benchmarks):

```
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

This will collect 100 samples, dump them to special directory under `_build` and calculates
statistics over them.

If you make your optimizations and run the same command again, it will
again collect 100 samples using your new code, calculates statistics, comparing
results with previous dump and will try to statistically proove that there exists
significant difference between results of two runs - if there is an improvement or
regression:

```
$ rebar3 bench
===> Fetching rebar3_bench
===> Compiling rebar3_bench
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
 (Student's t, pooled s = 3.21558
```

You can use `--dump-baseline` and `--baseline` to manage benchmark samples dumps.
Example workflow:

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

Avaliable options
-----------------

```
$ rebar3 help bench
Run microbenchmarks from callback modules
Usage: rebar3 bench [-d <dir>] [-m <module>] [-b <benches>]
                    [-t <duration>] [-n <samples>] [-c <confidence>]
                    [-p <parameter>] [--save-baseline <save_baseline>]
                    [--baseline <baseline>]

  -d, --dir          directory where the benchmark tests are located
                     (defaults to "test")
  -m, --module       name of one or more modules to run (comma-separated)
  -b, --bench        name of benchmark to run within a specified module
                     (comma-separated)
  -t, --duration     duration of single benchmark (default is 10s)
  -n, --num-samples  number of samples to collect and analyze (default is
                     100)
  -c, --confidence   confidence level: 80, 90, 95, 98, 99 (default is 95)
  -p, --parameter    which parameter to measure: wall_time, memory,
                     reductions (default wall_time)
  --save-baseline    save benchmark data to file with this name (default
                     '_tip')
  --baseline         use data stored in file as baseline (default '_tip')
```
