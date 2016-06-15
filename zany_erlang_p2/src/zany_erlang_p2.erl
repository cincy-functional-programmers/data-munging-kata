-module(zany_erlang_p2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    [ Path | _ ] = Args,
    io:format("Path: ~p~n", [Path]),

    {ok, Binary} = file:read_file(Path),

    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    FilteredLines = lists:filter(fun filter_by_number/1, Lines),
    ParsedLines = parse_lines(FilteredLines),
    
    {TeamName, _, _} = find_min_goal_diff(ParsedLines),

    io:format("Team: ~p~n", [TeamName]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

filter_by_number(Line) ->
  re:run(Line, "[0-9]+") =/= nomatch.

parse_lines([]) ->
  [];
parse_lines([Head | Tail]) ->
  [parse_line(Head) | parse_lines(Tail)].

parse_line(Line) ->
  [_, TeamName, _, _, _, _, GoalsFor, GoalsAgainst, _] = tl(re:split(Line, "[\s.-]+", [trim])),

  {TeamName, binary_to_integer(GoalsFor), binary_to_integer(GoalsAgainst)}.

find_min_goal_diff([Team]) ->
  Team;
find_min_goal_diff([Head | Tail]) ->
  GoalDiff = goal_diff(Head),
  LowestTeam = find_min_goal_diff(Tail),
  LowestGoalDiff = goal_diff(LowestTeam),
  if
    GoalDiff < LowestGoalDiff ->
      Head;
    true ->
      LowestTeam
  end.

goal_diff({_, GoalsFor, GoalsAgainst}) ->
  GoalsFor - GoalsAgainst.
