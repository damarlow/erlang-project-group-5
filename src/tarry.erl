-module(tarry).

-export([main/0, handleSpawned/1, getNeighbours/2]).

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
  Nodes = spawnNodes(Out),
  % Find the initial node from the first input line
  Initial = lists:keyfind(hd(Out), 1, Nodes),
  io:format("Initial node is ~p~n", [Initial]),
  % Get the PID of the initial node
  InitialPid = element(2, Initial),
  % Send the first message!
  InitialPid ! "Test".

spawnNodes([Initiator | Tail]) ->
  % Tail is the list to build graph from, Initiator is the node to start Tarry with
  % Use a list composition to get the name of each node (i.e. first elem of each sublist)
  NodeNames = [[lists:nth(1, Node)] || Node <- Tail],
  % The neighbours of each are the rest of the elements of each sublist
  Neighbours = [tl(SubList) || SubList <- Tail],
  % This spawns nodes and keeps Pids
  Pids = [spawn(tarry, handleSpawned, [Name]) || Name <- NodeNames],
  % Zip the Pids together with corresponding nodes
  NodeIDs = lists:zip(NodeNames, Pids),
  % Get a list of {PID, Neighbour-PID-list} tuples, giving a list of nodes that are neighbours for each node
  NeighbourIDs = [getNeighbours(N, NodeIDs) || N <- lists:zip(NodeIDs, Neighbours)],
  % Still need to do something with this
  [Pid ! NeighbourPids || {{_, Pid}, NeighbourPids} <- NeighbourIDs],
  % Need to work out how to handle that though
  io:format("Initiating Tarry with ~p~n", [Initiator]),
  NodeIDs.

getNeighbours({{Name, PID}, NeighboursForOne}, NodeIDs) ->
  % Function to get the PID for a node
  FuncMap = fun(Node) -> lists:keyfind([Node], 1, NodeIDs) end,
  % Return a tuple of the PID and the PIDs of its neighbours
  {{Name, PID}, lists:map(FuncMap, NeighboursForOne)}.

handleSpawned(Node) ->
  io:format("Node ~s with pid ~p spawned~n", [Node, self()]),
  receive
    Neighbours -> doTarry(Node, Neighbours)
  end.

doTarry(Node, Neighbours) ->
    io:format("Node ~s has neighbours ~p~n", [Node, Neighbours]),
    receive
        Msg -> io:format("Node ~p received message ~p~n", [Node, Msg])
    end.

% vim: et sw=2
