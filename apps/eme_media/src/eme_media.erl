-module(eme_media).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("kernel/include/file.hrl").
-include("../../eme_db/include/eme_db.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
  file_monitor:set_interval(eme_config:get(scan_interval)),
  lists:foreach(fun(Path) ->
        file_monitor:automonitor(efile:expand_path(Path))
    end, eme_config:get(medias, [video, audio, image])),
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({file_monitor, _, {found, SFile, file, #file_info{size = FileSize, mtime = MTime}, _}}, State) ->
  File = binary_to_list(SFile),
  lager:debug("Add : ~p", [File]),
  case media_type(File) of
    {_, undefined, undefined} -> lager:debug("Ignore file ~p", [File]);
    {MimeType, Type, Root} -> 
      Filename = filename:basename(File),
      Filepath = [X || X <- filename:split(
              estring:sub(
                estring:sub(File, Root, ""), 
                Filename, ""
              )
            ), X =/= "/"],
      {ok, FileMD5} = eme_utils:hash_file(File), 
      Media = #emedia_media{
        id = binary_to_list(uuid:generate()),
        hash = FileMD5,
        type = parent_id(Type),
        filename = Filename,
        fullpath = File,
        mimetype = MimeType,
        start_time = 0,
        duration = 0,
        size = FileSize,
        mtime = MTime,
        width = 0,
        height = 0,
        last_scan = eme_utils:timestamp()
      },
      add_file(Media, Filepath)
  end,
  {noreply, State};
handle_info({file_monitor, _,{changed, SFile, file, #file_info{size = FileSize, mtime = MTime}, _}}, State) ->
  File = binary_to_list(SFile),
  case eme_db:search_media_by_path(File) of
    not_found -> ok;
    Media -> 
      {ok, FileMD5} = eme_utils:hash_file(File), 
      UpdateMedia = Media#emedia_media{
             hash = FileMD5,
             start_time = 0,
             duration = 0,
             size = FileSize,
             mtime = MTime,
             width = 0,
             height = 0,
             last_scan = eme_utils:timestamp()
          },
      eme_db:insert(UpdateMedia)
  end,
  {noreply, State};
handle_info({file_monitor, _, {error, SFile, file, enoent}}, State) ->
  File = binary_to_list(SFile),
  case eme_db:search_media_by_path(File) of
    not_found -> ok;
    Media -> eme_db:delete_media(Media, true)
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

acceptable_type(MT, File) ->
  lists:foldl(fun(Type, MediaType) ->
        lists:foldl(fun(Path, MediaType1) ->
              MediaType1 ++ case re:run(File, "^" ++ Path ++ ".*") of
                nomatch -> [];
                _ -> [{MT, Type, Path}]
              end
          end, MediaType, eme_config:get(medias, Type))
    end, [], [video, image, audio]).

media_type(File) ->
  FileExt = eme_utils:file_ext(File),
  [MimeType|_] = mimetypes:extension(FileExt),
  lists:foldl(fun({MimeType1, FileType, _} = TypeInfo, Result) ->
        case re:run(MimeType1, "^" ++ atom_to_list(FileType) ++ "/.*") of
          nomatch -> Result;
          _ -> TypeInfo
        end
    end, {MimeType, undefined, undefined}, acceptable_type(MimeType, File)).

add_file(Media = #emedia_media{
    filename = Filename,
    type = Type,
    last_scan = LastScan
  }, Filepath) ->
  case eme_db:media_exist(Media) of
    true ->
      lager:info("EXIST : ~p", [Media]);
    false ->
      lager:info("NOTEXIST : ~p", [Media]),
      ContainerID = get_container_id(Type, Filepath, LastScan),
      #emedia_item{id = ItemID} = eme_db:add_item(Filename, dlna_type(Type), LastScan),
      eme_db:add_items_link_by_ids(ContainerID, ItemID),
      eme_db:insert(Media#emedia_media{item_id = ItemID})
  end.

parent_id(video) -> "V";
parent_id(image) -> "P";
parent_id(audio) -> "A".

dlna_type("V") -> "object.item.videoItem";
dlna_type("A") -> "object.item.audioItem";
dlna_type("P") -> "object.item.imageItem".

get_container_id(ParentID, [], _) ->
  ParentID;
get_container_id(ParentID, [Container|Rest], LastScan) ->
  Child = [X || #emedia_item{title = Title} = X <- eme_db:get_item_childs_by_id(ParentID),
    Title =:= Container
  ], 
  case Child of
    [#emedia_item{id = ID}] -> 
      get_container_id(ID, Rest, LastScan);
    [] -> 
      #emedia_item{id = ID} = eme_db:add_item(Container, "object.container", LastScan),
      eme_db:add_items_link_by_ids(ParentID, ID),
      get_container_id(ID, Rest, LastScan);
    _ -> throw(duplicated_node)
  end.
