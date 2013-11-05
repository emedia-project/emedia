-module(eme_utils).

-export([starts_with/2, bin_to_num/1, get_date/0]).

starts_with(Original, Substr) ->
  Len = string:len(Substr),
  StartStr = string:substr(Original, 1, Len),
  string:equal(StartStr, Substr).

bin_to_num(Bin) ->
  N = binary_to_list(Bin),
  case string:to_float(N) of
    {error,no_float} -> list_to_integer(N);
    {F,_Rest} -> F
  end.

get_date() ->
  httpd_util:rfc1123_date(erlang:localtime()).
