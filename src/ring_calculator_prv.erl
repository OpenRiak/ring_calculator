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
    calc_nvals(RingSize, NrNodes, Locs, Verbose),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

calc_nvals(RingSize, NrMachines, NrLocations, Verbose) when NrMachines > 0, NrLocations > 0 ->
  LVals = lists:seq(1, min(NrMachines, NrLocations)),
  TargetNVals = lists:seq(1, NrMachines),
  Sols = lists:flatten([ solutions(RingSize, LVal, TargetNVals, NrMachines, Locs, Verbose)
                        || LVal <- LVals, %% NVal <- TargetNVals,
                           Locs <- lists:seq(1, NrLocations) ]),
  case Sols of
      [] ->
         io:format("No solution~n");
      _ ->
         {BestLVal, BestNVal, Config, Ring} =
              lists:last(lists:sort([ Sol || {_, _, _, _} = Sol <- Sols ])),
         io:format("~nBest solution:~nTarget Nval ~p~nLocation val ~p~nConfig ~p~nRing: ~s~n",
                   [BestNVal, BestLVal, Config, ring_calculator_ring:show(Ring, {BestNVal, BestLVal})])
  end.

solutions(RingSize, LVal, [NVal | NVals], NrMachines, NrLocations, Verbose) when 0 < LVal, LVal =< NrLocations ->
  %% spread machines evenly
  Config = spread_over_locations(lists:duplicate(NrLocations, 0), NrMachines),
  [ io:format("Config ~p ", [Config]) || Verbose ],
  Solution = ring_calculator_ring:solve(RingSize, Config, {NVal, LVal}),
  ZeroViolations = ring_calculator_ring:zero_violations(Solution, {NVal, LVal}),
  [ io:format("Target Nval: ~p, Locaction val: ~p Success: ~p~n", [NVal, LVal, ZeroViolations ]) || Verbose ],
  if ZeroViolations ->
         [ io:format(".") || not Verbose ],
         [{LVal, NVal, Config, Solution} | solutions(RingSize, LVal, NVals, NrMachines, NrLocations, Verbose) ];
     true ->
         [ io:format("x") || not Verbose ],
         []
  end;
solutions(_RingSize, _LVal, _, _NrMachines, _NrLocations, _Verbose) ->
  [].

spread_over_locations(Config, 0) ->
  Config;
spread_over_locations([Hd | Tl], NrMachines) ->
  spread_over_locations(Tl++[Hd + 1], NrMachines - 1).
