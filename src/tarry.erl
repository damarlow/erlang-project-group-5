-module(tarry).

-export([main/0]).

readInput(Lines) ->
  case io:get_line("") of
    eof -> lists:reverse(Lines);
    Text ->
      Tokens = string:tokens(Text, " \n"),
      readInput([Tokens | Lines])
  end.

main() ->
  Out = readInput([]),
  io:format("~p~n", [Out]).