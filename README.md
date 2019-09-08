rebar3_bench
=====

Rebar3 microbenchmark plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        rebar3_bench
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 bench
    ===> Fetching rebar3_bench
    ===> Compiling rebar3_bench
    <Plugin Output>
