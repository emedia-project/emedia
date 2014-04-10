-module(service_content_directory_control).

-export([all/1]).

-include("../../../eme_db/include/eme_db.hrl").
-include("../include/soap_messages.hrl").

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).

all(Request) ->
  RequestRecord = case paris_request:body(Request) of
    {ok, Body} -> 
      eme_soap:parse_request(Body);
    {error, _} -> {error, "Invalid request: missing data"}
  end,
  case RequestRecord of 
    #browse{ 
      object_id = ObjectID,
      flag = "BrowseDirectChildren",
      filter = _Filter,
      starting_index = _StartIndex,
      requested_count = _Request_Count,
      sort_criteria = _SortCriteria
    } = B ->
      lager:info("---> B = ~p", [B]),
      {Sec, MSec, _} = now(),
      Item = eme_db:search_item_by_id(ObjectID),
      lager:info("---> Item = ~p", [Item]),
      TplData = case eme_db:count_item_childs(Item) of
        0 -> 
          % TODO : use a correct UpdateID
          [{entries, []}, {number_returned, 0}, {total_matches, 0}, {update_id, Sec*MSec}];
        N -> 
          [{entries, 
              [?record_to_tuplelist(emedia_item, X) ++ [
                  {child_count, eme_db:count_item_childs(X)}, 
                  {parent_id, ObjectID}
                  ] ++ get_media_item(X)
                || X <- eme_db:get_item_childs(Item)]
            }, 
            {number_returned, N}, 
            {total_matches, N}, 
            {update_id, Sec*MSec},
            {server_ip, eme_config:get(tcp_ip)},
            {server_port, eme_config:get(tcp_port)}
          ]
      end,
      paris_response:render_view(
        service_content_directory_control,
        TplData,
        [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}]
      );
    {error, Message} -> 
      paris_response:render_view(
        soap_error,
        [
          {upnp_error_code, 402},
          {upnp_error_message, Message}
        ],
        [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}]
      )
  end.

get_media_item(Item) ->
  case eme_db:get_media_for_item(Item) of
    X when X =:= undefined; X =:= error -> [];
    M -> [{media, ?record_to_tuplelist(emedia_media, M)}]
  end.
