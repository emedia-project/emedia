-module(eme_soap).

-export([parse_request/1]).

-include("../include/soap_messages.hrl").

-record(sax_data, {
    envelope = false,
    path = [],
    request = undefined,
    record = undefined
  }).

parse_request(Data) ->
  {ok, #sax_data{record = Result}, _} = erlsom:parse_sax(Data, #sax_data{}, fun(Event, #sax_data{envelope = Envelope, path = Path, request = Request, record = Record} = Acc) -> 
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
          {characters, Characteres} ->
            Characteres1 = string:strip(Characteres),
            Record1 = case Request of
              browse ->
                case eme_utils:join(Path, "/") of
                  "Envelope/Body/Browse/ObjectID" -> Record#browse{object_id = Characteres1};
                  "Envelope/Body/Browse/BrowseFlag" -> Record#browse{flag = Characteres1};
                  "Envelope/Body/Browse/Filter" -> Record#browse{filter = Characteres1};
                  "Envelope/Body/Browse/StartingIndex" -> Record#browse{starting_index = list_to_integer(Characteres1)};
                  "Envelope/Body/Browse/RequestedCount" -> Record#browse{requested_count = list_to_integer(Characteres1)};
                  "Envelope/Body/Browse/SortCriteria" -> Record#browse{sort_criteria = Characteres1};
                  _ -> Record
                end;
              _ -> Record
            end,
            Acc#sax_data{record = Record1};
          _ -> 
            Acc
        end
    end),
  Result.
