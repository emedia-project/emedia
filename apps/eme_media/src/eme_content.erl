-module(eme_content).

-include("../include/eme_db.hrl").

-export([scan/2]).

-define(FFPROBE_OPTIONS, "-v quiet -print_format json -show_format -show_streams").

scan(MediaType, Directory) ->
  Directory1 = case ucp:detect(Directory) of
    utf8 -> ucp:from_utf8(Directory);
    _ -> Directory
  end,
  case file:read_file_info(Directory1) of
    {ok, _} -> 
      lager:debug("Scan [~s] ~ts", [MediaType, Directory1]),
      do_scan(MediaType, Directory1);
    {error, Reason} -> 
      lager:debug("Can't scan [~s] ~ts : ~s", [MediaType, Directory1, Reason])
  end.

do_scan(MediaType, Directory) ->
  ScanTimestamp = eme_utils:timestamp(),
  FFProbe = eme_config:get(ffprobe_path),
  FFProbe1 = case ucp:detect(FFProbe) of
    utf8 -> ucp:from_utf8(FFProbe);
    _ -> FFProbe
  end,
  lists:foldl(fun(File, MediaType1) ->
      case file:read_file_info(File) of
        {ok, _} -> 
          update_media(Directory, File, MediaType1, ScanTimestamp, FFProbe1);
        {error, Reason} -> 
          lager:info("[SCAN] ~s : ~ts", [Reason, File])
      end,
      MediaType1
    end, MediaType, directory_content(Directory)).

update_media(Directory, File, MediaType, ScanTimestamp, _FFProbe) ->
  case mimetype_for_file_and_type(File, MediaType) of
    undefined -> 
      lager:debug("Ignore file ~ts, not type ~s", [File, MediaType]),
      ok;
    MimeType ->
      % ScanCommand = case ucp:detect(File) of
      %   utf8 -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ File ++ "\"";
      %   _ -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ ucp:to_utf8(File) ++ "\""
      % end,
      % {_RCod, _FileInfo} = eme_utils:cmd(ScanCommand), % TODO
      {ok, FileMD5} = eme_utils:hash_file(File),
      FileSize = filelib:file_size(File),
      Media = #emedia_media{
        id = binary_to_list(uuid:generate()),
        hash = FileMD5,
        type = MediaType,
        filename = filename:basename(File),
        fullpath = File,
        mimetype = MimeType,
        start_time = 0,
        duration = 0,
        size = FileSize,
        width = 0,
        height = 0,
        last_scan = ScanTimestamp
      },
      case eme_db:media_exist(Media) of
        true ->
          % TODO : Update ?
          lager:debug("EXIST : ~p", [Media]);
        false ->
          lager:debug("NOTEXIST : ~p", [Media]),
          ParentContainerItemID = organize(Directory, MediaType, Media),
          #emedia_item{id = ItemID} = case MediaType of
            "V" -> eme_db:add_item(filename:basename(File), "object.item.videoItem");
            "A" -> eme_db:add_item(filename:basename(File), "object.item.audioItem");
            "P" -> eme_db:add_item(filename:basename(File), "object.item.imageItem");
            _ -> throw(wrong_media_type)
          end,
          eme_db:add_items_link_by_ids(ParentContainerItemID, ItemID),
          eme_db:insert(Media#emedia_media{item_id = ItemID})
      end,
      ok
  end.

directory_content(Directory) ->
  lager:debug("scan ~ts", [Directory]),
  filelib:fold_files(Directory, ".*", true, fun (FileOrDirPath, Acc) -> [FileOrDirPath|Acc] end, []).

mimetype_for_file_and_type(File, MediaType) ->
  FileExt = eme_utils:file_ext(File),
  [MimeType|_] = mimetypes:extension(FileExt),
  get_mimetype(MediaType, MimeType).

get_mimetype("A", MimeType) ->
  case re:run(MimeType, "^audio/.*") of
    nomatch -> undefined;
    _ -> MimeType
  end;
get_mimetype("V", MimeType) ->
  case re:run(MimeType, "^video/.*") of
    nomatch -> undefined;
    _ -> MimeType
  end;
get_mimetype("P", MimeType) ->
  case re:run(MimeType, "^image/.*") of
    nomatch -> undefined;
    _ -> MimeType
  end;
get_mimetype(_, _) ->
  undefined.

organize(Directory, MediaType, #emedia_media{filename = File, fullpath = Path}) ->
  Path1 = eme_utils:sub(Path, Directory, ""),
  Path2 = eme_utils:sub(Path1, File, ""),
  Path3 = [X || X <- filename:split(Path2), X =/= "/"],
  get_container_id(MediaType, Path3).

get_container_id(ParentID, []) ->
  ParentID;
get_container_id(ParentID, [Container|Rest]) ->
  Child = [X || #emedia_item{title = Title} = X <- eme_db:get_item_childs_by_id(ParentID),
    Title =:= Container
  ], 
  case Child of
    [#emedia_item{id = ID}] -> 
      get_container_id(ID, Rest);
    [] -> 
      #emedia_item{id = ID} = eme_db:add_item(Container, "object.container"),
      eme_db:add_items_link_by_ids(ParentID, ID),
      get_container_id(ID, Rest);
    _ -> throw(duplicated_node)
  end.
