-module(emediaserver_content_directory_control_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(browse, {
    object_id = "0",
    flag = "BrowseDirectChildren",
    filter = "*",
    starting_index = 0,
    requested_count = 10000,
    sort_criteria = undefined
  }).

-record(sax_data, {
    envelope = false,
    path = [],
    request = undefined,
    record = undefined
  }).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  SaxData = case cowboy_req:body(Req) of
    {ok, Data, _Req2} -> 
      lager:error("~p", [Data]), Data,
      {ok, Result, _} = erlsom:parse_sax(Data, #sax_data{}, fun(Event, #sax_data{envelope = Envelope, path = Path, request = Request, record = Record} = Acc) -> 
          case Event of 
            {startElement, _Uri, LocalName, _Prefix, _Attributes} -> 
              Path1 = Path ++ [LocalName],
              {Request1, Record1} = case eme_utils:join(Path1, "/") of
                "Envelope/Body/Browse" -> {browse, #browse{}};
                _ -> {Request, Record}
              end,
              Envelope1 = case Envelope of
                false -> case eme_utils:join(Path1, "/") of
                    "Envelope/Body" -> true;
                    _ -> false
                  end;
                Other -> Other
              end,
              Acc#sax_data{envelope = Envelope1, path = Path1, request = Request1, record = Record1};
            {endElement, _Uri, LocalName, _Prefix} ->
              [LastLocal| Rest] = lists:reverse(Path),
              case LocalName of
                LastLocal -> 
                  Path1 = lists:reverse(Rest),
                  Acc#sax_data{path = Path1};
                _ -> throw(xml_error) % TODO
              end;
            {characters, Data} ->
              Record1 = case Request of
                browse ->
                  case eme_utils:join(Path, "/") of
                    "Envelope/Body/Browse/ObjectID" -> Record#browse{object_id = Data};
                    "Envelope/Body/Browse/BrowseFlag" -> Record#browse{flag = Data};
                    "Envelope/Body/Browse/Filter" -> Record#browse{filter = Data};
                    "Envelope/Body/Browse/StartingIndex" -> Record#browse{starting_index = list_to_integer(Data)};
                    "Envelope/Body/Browse/RequestedCount" -> Record#browse{requested_count = list_to_integer(Data)};
                    "Envelope/Body/Browse/SortCriteria" -> Record#browse{sort_criteria = Data};
                    _ -> Record
                  end;
                _ -> Record
              end,
              Acc#sax_data{record = Record1};
            _ -> Acc
          end
        end),
      Result;
    {error, _Reason} -> 
      lager:error("No body!"), 
      throw(request_error) % TODO
  end,
  #sax_data{request = RequestType, record = RequestRecord} = SaxData,
  lager:error("RequestType = ~p | RequestRecord = ~p", [RequestType, RequestRecord]),
  TplData = [{entries, [
        [{id, <<"V">>}, {parent_id, <<"0">>}, {child_count, <<"0">>}, {title, <<"Video">>}, {class, <<"object.container">>}],
        [{id, <<"A">>}, {parent_id, <<"0">>}, {child_count, <<"0">>}, {title, <<"Audio">>}, {class, <<"object.container">>}],
        [{id, <<"I">>}, {parent_id, <<"0">>}, {child_count, <<"0">>}, {title, <<"Images">>}, {class, <<"object.container">>}]
      ]}, {number_returned, 3}, {total_matches, 3}, {update_id, 1940}],
  lager:error("TplData = ~p", [TplData]),
  {ok, Body} = browse_direct_children_dtl:render(TplData),
  Headers = [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}],
  lager:error("Body = ~s", [Body]),
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
