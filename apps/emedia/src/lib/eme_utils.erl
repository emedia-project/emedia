-module(eme_utils).

-export([
    get_date/0,
    hash_file/1,
    timestamp/0,
    file_ext/1
  ]).

%-define(BLOCKSIZE, 32768).
%-define(BLOCKSIZE, 10485760).
-define(BLOCKSIZE, 20971520).
-define(IN(X,Min,Max), X >= Min, X =< Max).

get_date() ->
  httpd_util:rfc1123_date(erlang:localtime()).

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

file_ext(File) ->
  [_|ExtWithoutDot] = string:to_lower(filename:extension(File)),
  ExtWithoutDot.

