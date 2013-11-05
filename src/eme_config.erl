-module(eme_config).

-export([get/1]).

get(tcp_port) ->
  case os:getenv("EMEDIASERVER_PORT") of
    false ->
      8080;
    Other ->
      Other
  end;
get(max_conn) ->
  case os:getenv("EMEDIASERVER_MAX_CONN") of
    false ->
      100;
    Other ->
      eme_utils:bin_to_num(Other)
  end;
get(services) ->
  [rootdevice, mediaserver].
