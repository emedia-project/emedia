-module(emediaserver).

-export([start/0, start/1]).

start() ->
  start([true]).

start([StartSSDP]) ->
  {ok, _} = application:ensure_all_started(lager),
  ok = application:start(crypto),
  {ok, _} = application:ensure_all_started(cowboy),
  ok = application:start(mimetypes),
  ok = application:start(emedia),
  ok = application:start(eme_config),
  ok = application:start(eme_db),
  ok = application:start(eme_scanner),
  case StartSSDP of
    true -> ok = application:start(eme_ssdp);
    _ -> lager:info("ssdp not started!")
  end,
  ok = application:start(eme_server).
