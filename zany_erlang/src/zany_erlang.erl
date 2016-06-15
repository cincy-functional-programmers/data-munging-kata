-module(zany_erlang).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    [ Path | _ ] = Args,
    io:format("Args: ~p~n", [Path]),

    {ok, Binary} = file:read_file(Path),

    Lines = binary:split(Binary, <<"\n">>, [global]),

    StrippedLines = tl(tl(lists:droplast(lists:droplast(Lines)))),

    Accumulated = parse_lines(StrippedLines),

    SortedAccum = lists:sort(fun sorter/2, Accumulated),
    [Day, _, _] = hd(SortedAccum),

    io:format("Day Number ~p~n", [ Day ]),

    erlang:halt(0).

sorter(A, B) ->
  temp_diff(A) < temp_diff(B).

temp_diff([_, Max, Min]) ->
  Max - Min.

parse_lines([]) ->
  [];
parse_lines([Head | Tail]) ->
  [parse_line(Head) | parse_lines(Tail)].

parse_line(Line) ->
  lists:map(fun binary_to_integer/1, lists:sublist(tl(re:split(Line, "[*\s]+")), 3) ).


