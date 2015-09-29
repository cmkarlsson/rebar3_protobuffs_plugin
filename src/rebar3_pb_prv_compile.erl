%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%% Copyright (c) 2015 Martin Karlsson (martin@admitsolutions.co.nz)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar3_pb_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'compile').
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, protobuffs},     
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 protobuffs compile"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "rebar3 protobuffs compiler"},
            {desc, "rebar3 protobuffs compiler"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.proto\$"),
         proto_compile(AppInfo, FoundFiles)
     end || AppInfo <- Apps],

    {ok, State}.


proto_compile(AppInfo, ProtoFiles) ->
    %% Check for protobuffs library -- if it's not present, fail
    %% since we have.proto files that need building
    case protobuffs_is_present() of
        true ->
            %% Build a list of output files - { Proto, Beam, Hrl }
            Targets = [{Proto, beam_file(Proto), hrl_file(Proto)} ||
                          Proto <- ProtoFiles],

            %% Compile each proto file
            compile_each(AppInfo, Targets);
        false ->
            rebar_api:error("Protobuffs library not present in code path!",
                   []),
            rebar_api:abort()
    end.

protobuffs_is_present() ->
    code:which(protobuffs_compile) =/= non_existing.

beam_file(Proto) ->
    filename:basename(Proto, ".proto") ++ "_pb.beam".

hrl_file(Proto) ->
    filename:basename(Proto, ".proto") ++ "_pb.hrl".

needs_compile(HomeDir, Proto, Beam) ->
    ActualBeam = filename:join([HomeDir, "ebin", filename:basename(Beam)]),
    ProtoFile = filename:join([HomeDir, "src", Proto]),
    filelib:last_modified(ActualBeam) < filelib:last_modified(ProtoFile).

compile_each(_, []) ->
    ok;
compile_each(AppInfo, [{Proto, Beam, _Hrl} | Rest]) ->
    AppHome = rebar_app_info:out_dir(AppInfo),
    case needs_compile(AppHome, Proto, Beam) of
        true ->
            rebar_api:console("Compiling ~s", [Proto]),
            ErlOpts = rebar_opts:erl_opts(rebar_app_info:opts(AppInfo)),
            case catch(protobuffs_compile:scan_file(Proto,
                                              [{compile_flags, ErlOpts},
                                               {imports_dir, [filename:join(AppHome, "src")]},
                                               {output_ebin_dir, [filename:join(AppHome, "ebin")]},
                                               {output_include_dir, [filename:join(AppHome, "include")]}
                                              ])) of
                ok ->
                    rebar_api:info("Successfully compiled: ~s", [Proto]),
                    ok;
                Other ->
                    rebar_api:error("Protobuffs compilation of ~s failed: ~p",
                           [Proto, Other]),
                    rebar_api:abort()
            end;
        false ->
            ok
    end,
    compile_each(AppInfo, Rest).


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
