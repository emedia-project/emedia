-module(media_content).

-include("../include/database.hrl").

-export([scan/2]).

-define(FFPROBE_OPTIONS, "-v quiet -print_format json -show_format -show_streams").

scan(MediaType, Directory) ->
  Directory1 = case ucp:detect(Directory) of
    utf8 -> ucp:from_utf8(Directory);
    _ -> Directory
  end,
  case file:read_file_info(Directory1) of
    {ok, _} -> 
      lager:info("Scan [~s] ~ts", [MediaType, Directory1]),
      do_scan(MediaType, Directory1);
    {error, Reason} -> 
      lager:info("Can't scan [~s] ~ts : ~s", [MediaType, Directory1, Reason])
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
          update_media(File, MediaType1, ScanTimestamp, FFProbe1);
        {error, Reason} -> 
          lager:info("~s : ~ts", [Reason, File])
      end,
      MediaType1
    end, MediaType, directory_content(Directory)).

update_media(File, MediaType, ScanTimestamp, FFProbe) ->
  case mimetype_for_file_and_type(File, MediaType) of
    undefined -> 
      lager:info("Ignore file ~ts, not type ~s", [File, MediaType]),
      ok;
    MimeType ->
      ScanCommand = case ucp:detect(File) of
        utf8 -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ File ++ "\"";
        _ -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ ucp:to_utf8(File) ++ "\""
      end,
      {_RCod, _FileInfo} = eme_utils:cmd(ScanCommand), % TODO
      {ok, FileMD5} = eme_utils:hash_file(File),
      FileSize = filelib:file_size(File),
      Media = #emedia_media{
        item_id = binary_to_list(uuid:generate()),
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
      case media_db:media_exist(Media) of
        true ->
          % TODO : Update ?
          lager:info("EXIST : ~p", [Media]);
        false ->
          media_db:insert(Media),
          lager:info("NOTEXIST : ~p", [Media])
      end,
      ok
  end.

directory_content(Directory) ->
  lager:info("scan ~ts", [Directory]),
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

