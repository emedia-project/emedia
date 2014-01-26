-module(emediaserver_content_directory_control_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("../include/database.hrl").
-include("../include/soap_messages.hrl").

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  RequestRecord = case cowboy_req:body(Req) of
    {ok, Data, _Req2} -> 
      eme_soap:parse_request(Data);
    {error, _Reason} -> 
      {error, "Invalid request: missing data"}
  end,
  lager:info("RequestRecord = ~p", [RequestRecord]),
  Body1 = case RequestRecord of 
    #browse{ 
      object_id = ObjectID,
      flag = "BrowseDirectChildren",
      filter = _Filter,
      starting_index = _StartIndex,
      requested_count = _Request_Count,
      sort_criteria = _SortCriteria
    } ->
      {Sec, MSec, _} = now(),
      Item = media_db:search_item_by_id(ObjectID),
      TplData = case media_db:count_item_childs(Item) of
        0 -> 
          [{entries, []}, {number_returned, 0}, {total_matches, 0}, {update_id, Sec*MSec}];
        N -> 
          [{entries, 
              [?record_to_tuplelist(emedia_item, X) ++ [
                  {child_count, media_db:count_item_childs(X)}, 
                  {parent_id, ObjectID}] 
                || X <- media_db:get_item_childs(Item)]
            }, 
            {number_returned, N}, 
            {total_matches, N}, 
            {update_id, Sec*MSec}]
        end,
      lager:info("Browse result = ~p", [TplData]),
      {ok, Body} = browse_direct_children_dtl:render(TplData),
      Body;
    {error, Message} -> 
      {ok, ErrBody} = soap_error_dtl:render([
          {upnp_error_code, 402},
          {upnp_error_message, Message}
        ]),
      ErrBody
  end,
  Headers = [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}],
  {ok, Req2} = cowboy_req:reply(200, Headers, Body1, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
