-module(tarry).

-export([start/0, handleSpawned/1]).

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

start() ->
  Out = readInput([]),
  % Spawn the nodes from the output
  Nodes = spawnNodes(tl(Out)),
  % Find the initial node from the first input line
  Initial = lists:keyfind(hd(Out), 1, Nodes),
  % Get the PID of the initial node
  InitialPid = element(2, Initial),
  % Send the first message!
  InitialPid ! { {"main", self()}, [] },
  receive
    { _, List } ->
      Names = lists:map(fun(X) -> element(1, X) end, List),
      String = lists:join(" ", lists:reverse(Names)),
      io:format("~s~n", [String])
  end.

spawnNodes(List) ->
  % List is the list to build graph from, Initiator is the node to start Tarry with
  % Use a list composition to get the name of each node (i.e. first elem of each sublist)
  NodeNames = [[lists:nth(1, Node)] || Node <- List],
  % The neighbours of each are the rest of the elements of each sublist
  Neighbours = [tl(SubList) || SubList <- List],
  % This spawns nodes and keeps Pids
  Pids = [spawn(tarry, handleSpawned, [Name]) || Name <- NodeNames],
  % Zip the Pids together with corresponding nodes
  NodeIDs = lists:zip(NodeNames, Pids),
  % Get a list of {PID, Neighbour-PID-list} tuples
  % giving a list of nodes that are neighbours for each node
  NeighbourIDs = [getNeighbours(N, NodeIDs) || N <- lists:zip(NodeIDs, Neighbours)],
  % Still need to do something with this
  [Pid ! NeighbourPids || {{_, Pid}, NeighbourPids} <- NeighbourIDs],
  % Need to work out how to handle that though
  NodeIDs.

getNeighbours({{Name, PID}, NeighboursForOne}, NodeIDs) ->
  % Function to get the PID for a node
  FuncMap = fun(Node) -> lists:keyfind([Node], 1, NodeIDs) end,
  % Return a tuple of the PID and the PIDs of its neighbours
  {{Name, PID}, lists:map(FuncMap, NeighboursForOne)}.

handleSpawned(Node) ->
  receive
    Neighbours -> doTarry(Node, Neighbours, [])
  end.

doTarry(Name, Neighbours, OldParent) ->
  receive
    % Empty list signifies that we are the initiator
    { Sender, Visited } ->
      % Assign parent to the Sender if we don't have one
      Parent = case OldParent of [] -> [Sender]; _ -> OldParent end,
      % Unvisited nodes are neighbours that haven't been visited already
      Unvisited = lists:subtract(Neighbours, Visited),
      % Next node is the next unvisited neighbour, otherwise the parent
      Next = case Unvisited of 
               [] -> hd(Parent);
               % Change the followng to '_ -> hd(Unvisited)' for determinism
               _  -> lists:nth(rand:uniform(length(Unvisited)), Unvisited)
             end,
      Self = { Name, self() },
      % Pass on the token to the next element
      element(2, Next) ! { Self, [ Self | Visited ] },
      doTarry(Name, Neighbours, Parent)
  end.

% vim: et sw=2
