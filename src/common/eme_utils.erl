-module(eme_utils).

-export([
    starts_with/2, 
    bin_to_num/1, 
    get_date/0,
    user_home/0,
    expand_path/1,
    normalize_path/1,
    hash_file/1,
    timestamp/0,
    cmd/1,
    file_ext/1,
    sub/3,
    gsub/3
  ]).

%-define(BLOCKSIZE, 32768).
%-define(BLOCKSIZE, 10485760).
-define(BLOCKSIZE, 20971520).
-define(IN(X,Min,Max), X >= Min, X =< Max).

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

hash_file(File) ->
  case file:open(File, [binary,raw,read]) of
    {ok, P} -> hash_loop(P, crypto:hash_init(md5));
    Error -> Error
  end.

hash_loop(P, C) ->
  case file:read(P, ?BLOCKSIZE) of
    {ok, Bin} ->
      hash_loop(P, crypto:hash_update(C, Bin));
    eof ->
      file:close(P),
      {ok, digest2str(crypto:hash_final(C))}
  end.

digest2str(Digest) -> bin2str(binary_to_list(Digest)).

bin2str([H|T]) ->
  {H1, H2} = byte2hex(H),
  [H1,H2|bin2str(T)];
bin2str([]) ->
  [].

byte2hex(X) ->
  {nibble2hex(X bsr 4), nibble2hex(X band 15)}.

nibble2hex(X) when ?IN(X, 0, 9) -> X + $0;
nibble2hex(X) when ?IN(X, 10, 15) -> X - 10 + $a.

timestamp() ->
  {Mega,Sec,Micro} = os:timestamp(),
  (Mega*1000000+Sec)*1000000+Micro.

cmd(Cmd) ->
  Opt = [stream, exit_status, use_stdio,
    stderr_to_stdout, in, eof],
  P = open_port({spawn, Cmd}, Opt),
  get_cmd_data(P, []).

get_cmd_data(P, D) ->
  receive
    {P, {data, D1}} ->
      get_cmd_data(P, [D|D1]);
    {P, eof} ->
      port_close(P),
      receive
        {P, {exit_status, N}} ->
          {N, lists:reverse(D)}
      end
  end.

file_ext(File) ->
  [_|ExtWithoutDot] = string:to_lower(filename:extension(File)),
  ExtWithoutDot.

sub(Str,Old,New) ->
  Lstr = length(Str),
  Lold = length(Old),
  Pos  = string:str(Str,Old),
  if 
    Pos =:= 0 -> 
      Str;
    true      ->
      LeftPart = string:left(Str,Pos-1),
      RitePart = string:right(Str,Lstr-Lold-Pos+1),
      string:concat(string:concat(LeftPart,New),RitePart)
  end.

gsub(Str,Old,New) ->
  Acc = sub(Str,Old,New),
  subst(Acc,Old,New,Str).

subst(Str,_Old,_New, Str) -> Str;
subst(Acc, Old, New,_Str) ->
  Acc1 = sub(Acc,Old,New),
  subst(Acc1,Old,New,Acc).
