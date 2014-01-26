-module(emediaserver).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  ok = application:start(crypto),
  {ok, _} = application:ensure_all_started(cowboy),
  {ok, _Pid} = eme_config:start(),
  ok = application:start(emediaserver),
  {_S, _Pid1} = emediassdp:start(),
  {ok, _Pid2} = rootdevice:start(),
  {ok, _Pid3} = mediaserver:start(),
  media_db:start(),
  {ok, _Pid4} = media_scanner:start().
