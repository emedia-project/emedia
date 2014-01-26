-module(media_db).

-include("../include/database.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
    start/0,
    start/1,
    
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

start() -> start([node()]).
start(Nodes) ->
  Create = create_schema(Nodes),
  case mnesia:wait_for_tables([emedia_item, emedia_item_item, emedia_media], 20000) of
    {timeout, RemainingTabs} ->
      throw(RemainingTabs);
    ok ->
      synced
  end,
  case Create of
    created ->
      lager:info("insert initial items"),
      Item_0 = add_item("0", "eMedia Server", "object.container"),
      Item_V = add_item("V", "Videos", "object.container"),
      add_items_link(Item_0, Item_V),
      Item_A = add_item("A", "Music", "object.container"),
      add_items_link(Item_0, Item_A),
      Item_P = add_item("P", "Photos", "object.container"),
      add_items_link(Item_0, Item_P),
      ok;
    _ -> ok
  end.

add_item(Title, Class) ->
  add_item(binary_to_list(uuid:generate()), Title, Class).

add_item(ID, Title, Class) ->
  [_, Type|_] = string:tokens(Class, "."),
  Item = #emedia_item{
    id = ID,
    title = Title, 
    class = Class,
    type = Type
  },
  insert(Item),
  Item.

search_item_by_id(ID) ->
  uniq(do(qlc:q([X || X <- mnesia:table(emedia_item),
        X#emedia_item.id =:= ID
      ]))).

get_item_childs(#emedia_item{id = ItemID}) ->
  get_item_childs_by_id(ItemID).

get_item_childs_by_id(ItemID) ->
  Childs = do(qlc:q([X || X <- mnesia:table(emedia_item_item),
        X#emedia_item_item.parent_id =:= ItemID
      ])),
  lists:foldl(fun(#emedia_item_item{child_id = ID}, Acc) ->
        Acc ++ [search_item_by_id(ID)]
    end, [], Childs).


count_item_childs(#emedia_item{id = ItemID}) ->
  length(do(qlc:q([X || X <- mnesia:table(emedia_item_item),
        X#emedia_item_item.parent_id =:= ItemID
      ]))).

add_items_link(#emedia_item{id = ParentItemID}, #emedia_item{id = ChildItemID}) ->
  add_items_link_by_ids(ParentItemID, ChildItemID).

add_items_link_by_ids(ParentItemID, ChildItemID) ->
  ItemLink = #emedia_item_item{
    id = binary_to_list(uuid:generate()),
    parent_id = ParentItemID,
    child_id = ChildItemID
  },
  insert(ItemLink).

media_exist(#emedia_media{hash = Hash, fullpath = Fullpath}) ->
  M = #emedia_media{hash = Hash, fullpath = Fullpath, _ = '_'},
  F = fun() -> 
      mnesia:select(emedia_media, [{M, [], ['$_']}])
  end,
  length(mnesia:activity(transaction, F)) > 0.

insert(Data) ->
  F = fun() -> mnesia:write(Data) end,
  mnesia:transaction(F).

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

