-module(tarry).

-export([main/0, testSpawned/2]).

readInput(Lines) ->
  case io:get_line("") of
    % End of file, reverse the input for correct order
    eof -> lists:reverse(Lines);
    Line ->
      % Split each line on spaces and new lines
      Nodes = string:tokens(Line, " \n"),
      % Check next line and add nodes to the result
      readInput([Nodes | Lines])
  end.

main() ->
    Out = readInput([]),
    spawnNodes(Out).
%    io:format("~p~n", [Out]).

spawnNodes([Head | Tail])->
    % Head will be the initiator node, tail is the list to build graph from
    % Use a list composition to get the name of each node (i.e. first elem of each sublist)
    NodeNames = [lists:sublist(Node, 1, 1) || Node <- Tail],
    % The neighbours of each will be the rest of the elements of each sublist
    %Neighbours = 
    [dropFirst(SubList) || SubList <- Tail],
    % Pids = 
    [spawn(tarry, testSpawned, [Name, Name =:= Head]) || Name <- NodeNames].


% Confirm that a node is spawned correctly
testSpawned(Node, Flag)->
    io:format("Node ~p with id ~p, initiator: ~p ~n", [self(), Node, Flag ]).


% To make it look nicer when dropping the first and keeping in order
dropFirst(List)->
    lists:reverse(lists:droplast(lists:reverse(List))).
