-module(emedia).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  {ok, _} = application:ensure_all_started(cowboy),
  ok = application:start(mimetypes),
  ok = application:start(ffmpeg),
  emdb:start(),
  ok = application:start(emedia),
  ok = application:start(eme_config),
  ok = application:start(eme_media),
  ok = application:start(eme_ssdp).
