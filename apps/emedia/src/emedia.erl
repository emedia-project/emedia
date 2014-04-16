-module(emedia).

-export([start/0, start/1]).

start() ->
  start([true, true, false]).

start([StartSSDP, StartServer, StartObserver]) ->
  case StartObserver of
    true -> observer:start();
    _ -> ok
  end,
  {ok, _} = application:ensure_all_started(lager),
  ok = application:start(mimetypes),
  ok = application:start(ffmpeg),
  emdb:start(),
  ok = application:start(eme_config),
  ok = application:start(eme_db),
  ok = application:start(eme_media),
  case StartServer of
    true ->
      {ok, _} = application:ensure_all_started(cowboy),
      ok = application:start(emedia),
      case StartSSDP of
        true ->
          ok = application:start(eme_ssdp);
        _ -> ok
      end;
    _ -> ok
  end.
