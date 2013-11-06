-module(emediaserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS, 100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Routes      = routes(),
  Dispatch    = cowboy_router:compile(Routes),
  Port        = eme_config:get(tcp_port), 
  TransOpts   = [{port, Port}],
  ProtoOpts   = [
    {env, [{dispatch, Dispatch}]}, 
    {onrequest, fun debug_hook/1},
    {onresponse, fun custom_404_hook/4}
  ],
  {ok, _}     = cowboy:start_http(http, eme_config:get(max_conn), TransOpts, ProtoOpts),
  lager:info("emediaserver started on port ~p", [Port]),
  emediaserver_sup:start_link().

stop(_State) ->
  ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
  [
    {'_', [
        {"/", emediaserver_root_handler, []},
        {"/description", emediaserver_description_handler, []},
        {"/service/content_directory", emediaserver_content_directory_handler, []},
        {"/service/connection_manager", emediaserver_connection_manager_handler, []},
        {"/static/[...]", cowboy_static, [
            {directory, {priv_dir, emediaserver, ["static"]}},
            {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
          ]}
      ]}
  ].

custom_404_hook(404, Headers, <<>>, Req) ->
  {Path, _} = cowboy_req:path(Req),
  {Method, _} = cowboy_req:method(Req),
  lager:error("************** 404 [~p ~p] *****************", [Method, Path]),
  case cowboy_req:body(Req) of
    {ok, Data, _Req2} -> lager:error("~p", [Data]);
    {error, _Reason} -> lager:error("No body!")
  end,
  Body = <<"404 Not Found.">>,
  Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
    {<<"content-length">>, integer_to_list(byte_size(Body))}),
  {ok, Req2} = cowboy_req:reply(404, Headers2, Body, Req),
  Req2;
custom_404_hook(_, _, _, Req) ->
  Req.

debug_hook(Req) ->
  %erlang:display(Req),
  Req.
