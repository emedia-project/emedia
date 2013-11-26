-module(media_db).

-include("../include/database.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([
    start/0,
    start/1
  ]).

create_schema(Nodes) ->
  Schema = mnesia:create_schema(Nodes),
  mnesia:start(),
  case Schema of
    ok ->
      lager:info("create emedia schema."),
      mnesia:create_table(media,
        [{disc_copies, Nodes}, {attributes, record_info(fields, media)}]);
    {error,{_,{already_exists,_}}} ->
      lager:info("emedia schema already exist.");
    {error, Reason} ->
      lager:error("Num... Something was wrong. ~p", [Reason]),
      throw(Reason)
  end.

start() -> start([node()]).
start(Nodes) ->
  create_schema(Nodes),
  case mnesia:wait_for_tables([media], 20000) of
    {timeout, RemainingTabs} ->
      throw(RemainingTabs);
    ok ->
      synced
  end.
