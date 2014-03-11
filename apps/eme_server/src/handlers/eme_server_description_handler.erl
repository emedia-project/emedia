-module(eme_server_description_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  Version = case application:get_key(emedia, vsn) of
    {ok, Vsn} -> list_to_binary(Vsn);
    _ -> <<"unknow">>
  end,
  {ok, Body} = description_dtl:render([
      {url_base, "http://" ++ eme_config:get(tcp_ip) ++ ":" ++ integer_to_list(eme_config:get(tcp_port))},
      {hostname, eme_config:get(hostname)},
      {udn, binary_to_list(eme_config:get(uuid))},
      {version, Version}
  ]),
  Headers = [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}],
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
