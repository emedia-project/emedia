-module(media).

-export([get/2]).

-include("../../../eme_db/include/eme_db.hrl").

get(Request, MediaID) ->
  M = eme_db:search_media_by_id(MediaID),
  #emedia_media{fullpath = Path} = M,
  Length = filelib:file_size(Path),
  {Offset, Size} = case paris_request:range(Request) of
    {Start} -> 
      if
        Length - Start < 0 -> {Length, 0};
        true -> {Start, Length - Start}
      end;
    {Start, End} -> 
      if 
        Start > Length -> {Length, 0};
        true -> {Start, End - Start}
      end
  end,
  lager:info("Request media ~p [~p-~p]", [Path, Offset, Size]),
  paris_response:render_stream(Path, Offset, Size).
