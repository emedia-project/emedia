-module(emediaserver).

-export([start/0]).

start() ->
  ok = application:start(lager),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  {ok, _Pid} = eme_config:start(),
  ok = application:start(emediaserver),
  {_S, _Pid1} = emediassdp:start(),
  {ok, _Pid2} = rootdevice:start(),
  {ok, _Pid3} = mediaserver:start(),
  {ok, _Pid4} = media_scanner:start().
