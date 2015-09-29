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
-module(rebar3_pb_prv_clean).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'clean').
-define(DEPS, [app_discovery, install_deps]).

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
            {example, "rebar3 protobuffs clean"}, % How to use the plugin
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
         proto_clean(AppInfo, FoundFiles)
     end || AppInfo <- Apps],

    {ok, State}.

proto_clean(AppInfo, ProtoFiles) ->
    %% Get a list of generated .beam and .hrl files and then delete them
    AppHome = rebar_app_info:outdir(AppInfo),
    BeamFiles = [fq_beam_file(AppHome, F) || F <- ProtoFiles],
    HrlFiles = [fq_hrl_file(AppHome, F) || F <- ProtoFiles],
    Targets = BeamFiles ++ HrlFiles,
    delete_each(Targets).

fq_beam_file(AppHome, Proto) ->
    filename:join([AppHome, "ebin", filename:basename(Proto, ".proto") ++ "_pb.beam"]).

fq_hrl_file(AppHome, Proto) ->
    filename:join([AppHome, "include", filename:basename(Proto, ".proto") ++ "_pb.hrl"]).

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            rebar_api:error("Failed to delete ~s: ~p", [File, Reason])
    end,
    delete_each(Rest).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
