-module(eme_server_root_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Index = filename:join([code:priv_dir(eme_server), "browser", "index.html"]),
  Data = list_to_bitstring(readlines(Index)),
  {ok, Req2} = cowboy_req:reply(200, [], Data, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

readlines(FileName) ->
  {ok, Device} = file:open(FileName, [read]),
  try get_all_lines(Device)
  after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.
