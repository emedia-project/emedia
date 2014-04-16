-module(emedia_explorer_app).

-include("../../eme_db/include/eme_db.hrl").
-include_lib("wx/include/wx.hrl").
-export([
  start/0,
  start/2,
  stop/1,
  tree/0
]).

start() ->
  make_window().

start(_, _) ->
  make_window().

stop(_) ->
  ok.

make_window() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, "eMedia explorer", [{size,{600, 400}}]),

  eme_ex_tree:start(Frame),
  wxFrame:show(Frame),
  ok.


tree() ->
  Root = eme_db:search_item_by_id("0"),
  tree([Root], [""]).
tree([], _) -> ok;
tree([Item = #emedia_item{title = Title}|Rest], Decoration) ->
  Deco = case length(Rest) of
    0 -> pop(Decoration) ++ ["  `"];
    _ -> Decoration
  end,
  io:format("~s~s (~p)~n", [deco(Deco), Title, ucp:detect(Title)]),
  Childs = eme_db:get_item_childs(Item),
  Deco1 = case length(Rest) of
    0 -> pop(Decoration) ++ ["   "];
    _ -> Decoration
  end,
  tree(Childs, Deco1 ++ ["  |"]),
  tree(Rest, Deco1).

pop(List) ->
  [_|Rest] = lists:reverse(List),
  lists:reverse(Rest).

deco([_|List]) ->
  lists:flatten(List) ++ "- ".
