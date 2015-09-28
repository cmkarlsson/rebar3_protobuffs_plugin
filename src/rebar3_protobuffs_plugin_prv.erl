-module('rebar3_protobuffs_plugin_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'rebar3_protobuffs_plugin').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar3_protobuffs_plugin"}, % How to use the plugin
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
         Opts = rebar_app_info:opts(AppInfo),
         OutDir = rebar_app_info:out_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "proto_files"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.proto\$"),

         CompileFun = fun(Source, Opts1) ->
                              proto_compile(Opts1, Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.

proto_compile(_, Source, OutDir) ->
    {ok, Binary} = file:read_file(Source),
    OutFile = filename:join([OutDir, "priv", filename:basename(Source)]),
    filelib:ensure_dir(OutFile),
    rebar_api:info("Writing out ~s", [OutFile]),
    file:write_file(OutFile, Binary).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
