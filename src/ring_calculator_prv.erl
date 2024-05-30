-module(ring_calculator_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, ring_calculator).
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
        {example, "rebar3 ring_calcular --ring_size 32 --nodes 8 --locations 3 "}, % How to use the plugin
        {opts, [{ring_size, $s, "ring_size", integer, "size of riak ring (default 64)"},
                {nodes, $n, "nodes", integer, "number of available riak nodes"},
                {locations, $l, "locations", integer, "number of physical locations you can run the nodes on"},
                {verbose, $v, "verbose", boolean, "print computation details"}
               ]},                   % list of options understood by the plugin
        {short_desc, "Compute Riak ring configurations"},
        {desc, "Compute Riak ring configurations based upon the number "
               "of nodes one has available, the number of different locations "
               "and the prefered ring size. In summary, it provides the "
               "best possible target n-val for the nodes and the best possible "
               "location val given your available nodes and locations."
        }
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {KVs, _} = rebar_state:command_parsed_args(State),
    RingSize = proplists:get_value(ring_size, KVs, 64),
    NrNodes = proplists:get_value(nodes, KVs, 8),
    Locs = proplists:get_value(locations, KVs, 4),
    Verbose = proplists:get_value(verbose, KVs, false),
    ring_calculator:nvals(RingSize, NrNodes, Locs, Verbose),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

