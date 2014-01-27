-module(uuid).
-export([generate/0]).

generate() ->
  Now = {_, _, Micro} = now(),
  Nowish = calendar:now_to_universal_time(Now),
  Nowsecs = calendar:datetime_to_gregorian_seconds(Nowish),
  Then = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  Prefix = io_lib:format("~14.16.0b", [(Nowsecs - Then) * 1000000 + Micro]),
  list_to_binary(Prefix ++ to_hex(crypto:rand_bytes(9))).

to_hex([]) ->
  [];
to_hex(Bin) when is_binary(Bin) ->
  to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
  [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.
