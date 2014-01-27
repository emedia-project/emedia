-module(eme_server_root_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(200, [], <<"
    <html>
      <body>
        <h1>eMedia Server</h1>
        <img src='/static/emedia.png' /> Hello
      </body>
    </html>
  ">>, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
