-module(tarry).
-export([main/0]).

main() ->
  case io:get_line("") of
    eof  -> ok;
    Line ->
      % This leaks memory so don't run it with a lot of data
      % tail recursive my arse... 🙄
      io:fwrite(Line),
      main()
  end.
