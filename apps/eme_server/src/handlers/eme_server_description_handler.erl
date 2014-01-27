-module(eme_server_description_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, Body} = description_dtl:render([
      {url_base, "http://" ++ rootdevice:get_ip_port()},
      {hostname, rootdevice:get_hostname()},
      {udn, binary_to_list(rootdevice:get_uuid())}
  ]),
  Headers = [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}],
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
