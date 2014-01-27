-module(ssdp_message).

-export([build_msearch_response/3]).

-define(CRLF,[13, 10]). %% "\r\n"

build_msearch_response(ST, URI, Service_Type) ->
  List = [
    "HTTP/1.1 200 OK", ?CRLF,
    "CACHE-CONTROL: max-age = 1200", ?CRLF,
    "DATE: ", eme_utils:get_date(), ?CRLF,
    "EXT:", ?CRLF,
    "LOCATION: http://", rootdevice:get_ip_port(), URI,?CRLF,
    "SERVER: ", rootdevice:get_os(), ?CRLF,
    "ST: " , ST, ?CRLF,
    "USN:uuid:" , binary_to_list(rootdevice:get_uuid()) ++ "::" ++ Service_Type, ?CRLF,
    "Content-Length: 0" ,?CRLF, ?CRLF
  ],
  lists:append(List).

