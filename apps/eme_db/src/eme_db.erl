-module(eme_db).

-behaviour(gen_server).

-include("../include/eme_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
    start_link/0,
    init/1,
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3,
    
    add_item/2,
    add_item/3,

    search_item_by_id/1,
    get_item_childs/1,
    get_item_childs_by_id/1,
    count_item_childs/1,

    add_items_link/2,
    add_items_link_by_ids/2,

    media_exist/1,

    insert/1
  ]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_item(Title, Class) ->
  add_item(binary_to_list(uuid:generate()), Title, Class).

add_item(ID, Title, Class) ->
  gen_server:call(?MODULE, {add_item, ID, Title, Class}).

search_item_by_id(ID) ->
  gen_server:call(?MODULE, {search_item_by_id, ID}).

get_item_childs(#emedia_item{id = ItemID}) ->
  get_item_childs_by_id(ItemID).

get_item_childs_by_id(ItemID) ->
  gen_server:call(?MODULE, {get_item_childs_by_id, ItemID}).

count_item_childs(#emedia_item{id = ItemID}) ->
  length(do(qlc:q([X || X <- mnesia:table(emedia_item_item),
        X#emedia_item_item.parent_id =:= ItemID
      ]))).

add_items_link(#emedia_item{id = ParentItemID}, #emedia_item{id = ChildItemID}) ->
  add_items_link_by_ids(ParentItemID, ChildItemID).

add_items_link_by_ids(ParentItemID, ChildItemID) ->
  gen_server:call(?MODULE, {add_items_link_by_ids, ParentItemID, ChildItemID}).

media_exist(#emedia_media{hash = Hash, fullpath = Fullpath}) ->
  gen_server:call(?MODULE, {media_exist, Hash, Fullpath}).

insert(Data) ->
  gen_server:call(?MODULE, {insert, Data}).

% Server

%% @hidden
init([]) -> init([node()]);
init(Nodes) ->
  Create = create_schema(Nodes),
  case mnesia:wait_for_tables([emedia_item, emedia_item_item, emedia_media], 20000) of
    {timeout, RemainingTabs} ->
      lager:info("-----------------------> Timeout!!!"),
      throw(RemainingTabs);
    ok ->
      ok
  end,
  case Create of
    created ->
      lager:info("insert initial items"),
      do_add_item("0", "eMedia Server", "object.container"),
      do_add_item("V", "Videos", "object.container"),
      do_add_items_link_by_ids("0", "V"),
      do_add_item("A", "Music", "object.container"),
      do_add_items_link_by_ids("0", "A"),
      do_add_item("P", "Photos", "object.container"),
      do_add_items_link_by_ids("0", "P"),
      {ok, db};
    _ -> {ok, db}
  end.

%% @hidden
terminate(_Reason, _Config) -> 
  ok.

%% @hidden
handle_cast(_Message, Config) -> 
  {noreply, Config}.

%% @hidden
handle_info(_Message, Config) -> 
  {noreply, Config}.

%% @hidden
code_change(_OldVersion, Config, _Extra) -> 
  {ok, Config}.

%% @hidden
handle_call({add_item, ID, Title, Class}, _From, Config) ->
  {reply, do_add_item(ID, Title, Class), Config};
handle_call({search_item_by_id, ID}, _From, Config) ->
  {reply, do_search_item_by_id(ID), Config};
handle_call({get_item_childs_by_id, ItemID}, _From, Config) ->
  Childs = do(qlc:q([X || X <- mnesia:table(emedia_item_item),
        X#emedia_item_item.parent_id =:= ItemID
      ])),
  Result = lists:foldl(fun(#emedia_item_item{child_id = ID}, Acc) ->
        Acc ++ [do_search_item_by_id(ID)]
    end, [], Childs),
  {reply, Result, Config};
handle_call({count_item_childs, #emedia_item{id = ItemID}}, _From, Config) ->
  Result = length(do(qlc:q([X || X <- mnesia:table(emedia_item_item),
        X#emedia_item_item.parent_id =:= ItemID
      ]))),
  {reply, Result, Config};
handle_call({add_items_link_by_ids, ParentItemID, ChildItemID}, _From, Config) ->
  {reply, do_add_items_link_by_ids(ParentItemID, ChildItemID), Config};
handle_call({media_exist, Hash, Fullpath}, _From, Config) ->
  M = #emedia_media{hash = Hash, fullpath = Fullpath, _ = '_'},
  F = fun() -> 
      mnesia:select(emedia_media, [{M, [], ['$_']}])
  end,
  Result = length(mnesia:activity(transaction, F)) > 0,
  {reply, Result, Config};
handle_call({insert, Data}, _From, Config) ->
  {reply, do_insert(Data), Config};
handle_call(_Message, _From, Config) ->
  {reply, error, Config}.

% Private

do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

uniq(Data) ->
  case length(Data) of
    0 -> undefined;
    1 -> [Result|_] = Data, Result;
    _ -> error
  end.

do_insert(Data) ->
  F = fun() -> mnesia:write(Data) end,
  mnesia:transaction(F).

do_add_item(ID, Title, Class) ->
  [_, Type|_] = string:tokens(Class, "."),
  Item = #emedia_item{
    id = ID,
    title = Title, 
    class = Class,
    type = Type
  },
  do_insert(Item),
  Item.

do_add_items_link_by_ids(ParentItemID, ChildItemID) ->
  ItemLink = #emedia_item_item{
    id = binary_to_list(uuid:generate()),
    parent_id = ParentItemID,
    child_id = ChildItemID
  },
  do_insert(ItemLink).

do_search_item_by_id(ID) ->
  uniq(do(qlc:q([X || X <- mnesia:table(emedia_item),
        X#emedia_item.id =:= ID
      ]))).

create_schema(Nodes) ->
  Schema = mnesia:create_schema(Nodes),
  mnesia:start(),
  case Schema of
    ok ->
      lager:info("create emedia schema."),
      mnesia:create_table(emedia_item,
        [{disc_copies, Nodes}, {attributes, record_info(fields, emedia_item)}]),
      mnesia:create_table(emedia_item_item,
        [{disc_copies, Nodes}, {attributes, record_info(fields, emedia_item_item)}]),
      mnesia:create_table(emedia_media,
        [{disc_copies, Nodes}, {attributes, record_info(fields, emedia_media)}]),
      created;
    {error,{_,{already_exists,_}}} ->
      lager:info("emedia schema already exist."),
      complete;
    {error, Reason} ->
      lager:error("Num... Something was wrong. ~p", [Reason]),
      throw(Reason)
  end.

