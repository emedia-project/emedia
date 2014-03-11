-module(eme_server_stream_media_handler).

-export([init/3]).
-export([rest_init/2]).
-export([content_types_provided/2]).
-export([stream_file/2]).

-include("../../../eme_db/include/eme_db.hrl").
-record(media_file, {
    path,
    mimetype, % {Type, SubType, '*'}
    size
  }).

init(_Transport, _Req, _Table) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Table) ->
  {MediaID, _} = cowboy_req:qs_val(<<"id">>, Req),
  M = eme_db:search_media_by_id(binary_to_list(MediaID)),
  #emedia_media{fullpath = Path, mimetype = Mimetype, size = Size} = M,
  [Type, SubType|_] = binary:split(Mimetype, <<"/">>),
  State = #media_file{path = Path, mimetype = {Type, SubType, '*'}, size = Size},
  {ok, Req, State}.

content_types_provided(Req, #media_file{mimetype = Mimetype} = State) ->
  {[{Mimetype, stream_file}], Req, State}.

stream_file(Req, State = #media_file{path = Path, size = Size}) ->
  Sendfile = fun (Socket, Transport) ->
      case Transport:sendfile(Socket, Path) of
        {ok, _} -> ok;
        {error, closed} -> ok;
        {error, etimedout} -> ok
      end
  end,
  {{stream, Size, Sendfile}, Req, State}.
