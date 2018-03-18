-module(tarry).

-export([main/0, handleSpawned/2, getNeighbours/2]).

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
    % Spawn the nodes from the output
    spawnNodes(Out).
    %    io:format("~p~n", [Out]).

spawnNodes([Head | Tail])->
    % Head will be the initiator node, tail is the list to build graph from
    % Use a list composition to get the name of each node (i.e. first elem of each sublist)
    NodeNames = [lists:sublist(Node, 1, 1) || Node <- Tail],
    % The neighbours of each are the rest of the elements of each sublist
    Neighbours = [lists:reverse(lists:droplast(lists:reverse(SubList)))|| SubList <- Tail],
    % This spawns nodes and keeps Pids
    Pids = [spawn(tarry, handleSpawned, [Name, Name =:= Head]) || Name <- NodeNames],
    % Zip the Pids together with corresponding nodes
    NodeIDs = lists:zip(NodeNames, Pids),
    % Get a list of {PID, Neighbour-PID-list} tuples, giving a list of nodes that are neighbours for each node
    NeighbourIDs = [getNeighbours(N, NodeIDs)|| N <- lists:zip(NodeIDs, Neighbours)],
    % Still need to do something with this
    % Possibly [Pid ! Neighbours || {Pid, Neighbours} <- NeighbourIDs]
    % Need to work out how to handle that though
    io:format("~p~n", [NeighbourIDs]).

getNeighbours({{_, PID}, NeighboursForOne}, NodeIDs)->
    % Function to get the PID for a node
    FuncMap = fun(Node) -> element(2, lists:keyfind([Node], 1, NodeIDs)) end,
    % Return a tuple of the PID and the PIDs of its neighbours
    {PID, lists:map(FuncMap, NeighboursForOne)}.
		
% Confirm that a node is spawned correctly
handleSpawned(Node, Flag)->
    io:format("Node ~p with id ~s spawned, initiator: ~p ~n", [self(), Node, Flag ]).
