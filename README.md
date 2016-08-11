rebar3\_protobuffs\_plugin
==========================

rebar3 protobuffs compiler.

This is a conversion of the rebar2 *rebar_protobuffs_compiler.erl* module (https://github.com/rebar/rebar/blob/master/src/rebar_protobuffs_compiler.erl) into a rebar3 plugin. The plugin was primarily developed to compile the Erlang Riak Client, which uses  *riak_pb* which needs a protobuffer compiler.

The plugin compiles *proto* files using the *erlang protobuffs* compiler (https://github.com/basho/erlang_protobuffs.git).

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_protobuffs_plugin, {git, "git://github.com/cmkarlsson/rebar3_protobuffs_plugin"}}
    ]}.

Compile proto files using the plugin directly:

    $ rebar3 protobuffs compile
    ===> Fetching rebar3_protobuffs_plugin
    ===> Compiling rebar3_protobuffs_plugin
    <Plugin Output>

Clean beam and generated hrl files using the plugin directly:

    $ rebar3 protobuffs clean


You may want to add a *provider_hook* to your rebar config to automatically compile proto files before any other code:

    {provider_hooks, [{pre, [{compile, {protobuffs, compile}},
                             {clean, {protobuffs, clean}}]}]}.

Then the plugin is invoked when you run:

    $ rebar3 compile

and:

    $ rebar3 clean

respectively


The original use case was to build proto files with riak_pb as a dependency. This can be achieved by adding the following overrides rule to your rebar config:

    {overrides, [
        {add, riak_pb, [
            {plugins, [
                { rebar3_protobuffs_plugin,
                  {git, "git://github.com/cmkarlsson/rebar3_protobuffs_plugin", {tag, "0.1.1"}}
                }]
            },
            {provider_hooks, [{pre, [{compile, {protobuffs, compile}}]}]}
        ]}
    ]}.

Then:

    $ rebar3 compile


Should use the plugin on the riak_pb application to compile the proto files before the actual compilation step.

Limitations
-----------

* The *clean* provider does not work when used in overrides

TODO
----
* Add GDP compiler
* Add tests
