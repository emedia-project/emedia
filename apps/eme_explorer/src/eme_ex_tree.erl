-module(eme_ex_tree).

-behaviour(wx_object).

-export([start/1]).
-export([init/1, terminate/2,  code_change/3,
         handle_info/2, handle_call/3, handle_cast/2, handle_event/2]).

-include("../../eme_db/include/eme_db.hrl").
-include_lib("wx/include/wx.hrl").

-record(state, {
    parent,
    panel
  }).

start(Parent) ->
  wx_object:start_link(?MODULE, Parent, []).

init(Parent) ->
  wx:batch(fun() -> do_init(Parent) end).

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _State) ->
  ok.

handle_info(_Msg, State) ->
  {noreply, State}.

handle_call(shutdown, _From, State=#state{panel=Panel}) ->
  wxPanel:destroy(Panel),
  {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
  {reply,{error, nyi}, State}.

handle_cast(_Msg, State) ->
  {noreply,State}.

handle_event(#wx{event = #wxTree{type = command_tree_sel_changed,
                                 item = Item},
                 obj = TreeCtrl},
             State) ->
  ItemID = wxTreeCtrl:getItemData(TreeCtrl, Item),
  case ItemID of
    undefined -> ok;
    ID -> 
      wx_misc:launchDefaultBrowser("http://" ++ 
                                   eme_config:get(ip) ++
                                   ":" ++ integer_to_list(eme_config:get(port)) ++
                                   "/media/" ++ ID)
  end,
  {noreply, State}.

% Private

do_init(Parent) ->
  Panel  = wxPanel:new(Parent, []),

  %% Setup sizers
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "eMedia"}]),

  TreeCtrl = wxTreeCtrl:new(Panel, []),
  RootId = wxTreeCtrl:addRoot(TreeCtrl, "eMedia"),
  Root = eme_db:search_item_by_id("0"),
  add_items(eme_db:get_item_childs(Root), RootId, TreeCtrl),

  wxTreeCtrl:expand(TreeCtrl, RootId),

  Options = [{flag, ?wxEXPAND}, {proportion, 1}],
  wxSizer:add(Sizer, TreeCtrl, Options),
  wxSizer:add(MainSizer, Sizer, Options),

  wxTreeCtrl:connect(TreeCtrl, command_tree_sel_changed),

  wxPanel:setSizer(Panel, MainSizer),

  {Panel, #state{parent = Parent, panel = Panel}}.

add_items([], _, _) -> ok;
add_items([Item = #emedia_item{title = Title}|Rest], ID, TreeCtrl) ->
  MediaID = case eme_db:item_has_media(Item) of
    true -> 
      #emedia_media{id = MediaID1} = eme_db:get_media_for_item(Item),
      MediaID1;
    false ->
      undefined
  end,
  ItemID = wxTreeCtrl:appendItem(TreeCtrl, ID, Title, [{data, MediaID}]),
  Childs = eme_db:get_item_childs(Item),
  add_items(Childs, ItemID, TreeCtrl),
  add_items(Rest, ID, TreeCtrl).
