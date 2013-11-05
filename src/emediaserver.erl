-module(emediaserver).

-export([start/0]).

start() ->
  ok = application:start(lager),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(emediaserver),
  {_S, _Pid} = emediassdp:start(),
  {ok, _Pid2} = rootdevice:start(),
  {ok, _Pid3} = mediaserver:start().
