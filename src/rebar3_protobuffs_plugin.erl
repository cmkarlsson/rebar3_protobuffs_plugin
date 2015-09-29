-module(rebar3_protobuffs_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_pb_prv_compile:init(State),
    {ok, State2} = rebar3_pb_prv_clean:init(State1),
    {ok, State2}.
