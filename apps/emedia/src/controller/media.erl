-module(media).

-export([get/2]).

-include("../../../eme_media/include/eme_db.hrl").

get(_Request, MediaID) ->
  M = eme_db:search_media_by_id(MediaID),
  #emedia_media{fullpath = Path} = M,
  paris_response:render_stream(Path).
