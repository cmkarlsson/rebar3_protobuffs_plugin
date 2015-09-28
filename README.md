rebar3_protobuffs_plugin
=====

rebar3 protobuffs compiler

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_protobuffs_plugin, ".*", {git, "git@host:user/rebar3_protobuffs_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_protobuffs_plugin
    ===> Fetching rebar3_protobuffs_plugin
    ===> Compiling rebar3_protobuffs_plugin
    <Plugin Output>
