-module(ring_calculator).

-export([init/1, nvals/3, nvals/4]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = ring_calculator_prv:init(State),
    {ok, State1}.

-spec nvals(RingSize :: pos_integer(), NrNodes :: pos_integer(), NrLocations :: pos_integer()) -> ok.
nvals(RingSize, NrMachines, NrLocations) ->
  nvals(RingSize, NrMachines, NrLocations, false).

-spec nvals(RingSize :: pos_integer(), NrNodes :: pos_integer(),
            NrLocations :: pos_integer(), Verbose :: boolean()) -> ok.
nvals(RingSize, NrMachines, NrLocations, Verbose) when NrMachines > 0, NrLocations > 0 ->
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
