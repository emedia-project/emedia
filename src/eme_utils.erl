-module(eme_utils).

-export([
    starts_with/2, 
    bin_to_num/1, 
    get_date/0,
    user_home/0,
    expand_path/1,
    normalize_path/1
  ]).

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

expand_path(Path) ->
  normalize_path(filename:absname(expand_home(Path))).

normalize_path(Path) ->
  normalize_path(filename:split(Path), []).
normalize_path([".."|T], []) ->
  normalize_path(T, []);
normalize_path([".."|T], [_|Acc]) ->
  normalize_path(T, Acc);
normalize_path(["."|T], Acc) ->
  normalize_path(T, Acc);
normalize_path([H|T], Acc) ->
  normalize_path(T, [H|Acc]);
normalize_path([], Acc) ->
  filename:join(lists:reverse(Acc)).

expand_home([$~|Rest]) ->
  user_home() ++ Rest;
expand_home(Path) -> Path.

user_home() ->
  case os:type() of
    {win32, _} -> get_windows_home();
    _ -> get_unix_home()
  end.

get_unix_home() ->
  os:getenv("HOME").

get_windows_home() ->
  filename:absname(
    case os:getenv("USERPROFILE") of
      false -> 
        get_windows_home(os:getenv("HOMEDRIVE"));
      Path -> Path
    end
  ).
get_windows_home(false) -> false;
get_windows_home(HomeDrive) -> get_windows_home(HomeDrive, os:getenv("HOMEPATH")).
get_windows_home(_, false) -> false;
get_windows_home(HomeDrive, HomePath) -> HomeDrive ++ HomePath.

