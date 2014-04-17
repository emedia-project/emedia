-module(ssdp).

-export([start/0, receiver/0]).

-define(M_SEARCH, "M-SEARCH").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  lager:info("ssdp started"),
  udp_mcast:start({239,255,255,250},1900,?MODULE).

receiver() ->
  receive
    {udp, Socket, IP, InPortNo, Packet} ->
      lager:debug("[SSDP] From: ~p - Port: ~p - Data: ~p",[IP,InPortNo,Packet]),
      handle(udp, Socket, IP, InPortNo, Packet),
      receiver();
    stop -> true;
    AnythingElse -> 
      lager:debug("[SSDP] RECEIVED: ~p~n",[AnythingElse]),
      receiver()
  end.

%% ===================================================================
%% Internal functions
%% ===================================================================
handle(udp, Socket, IP, InPortNo, Packet) ->
  case is_msearch(Packet) of
    false -> 
      lager:debug("[SSDP] Don't understand message ~p", [Packet]);
    true ->
      handle_msearch(Socket, IP, InPortNo, Packet)
  end.

is_msearch(Packet) ->
  estring:start_with(Packet, ?M_SEARCH).

handle_msearch(Socket, IP, InPortNo, Packet) ->
  case get_st(Packet) of
    {service, ST} ->
      case rootdevice:get_service(ST) of
        {ok, Service} -> send_msearch_response(Socket, IP, InPortNo, Packet, ST, Service);
        {error, _Reason} -> lager:error("[SSDP] Unknow service type ~p", [ST])
      end;
    noservice ->
      lager:error("[SSDP] No service found")
  end,
  true.

get_st(Message) ->
  lager:debug("[SSDP] get_st : Message = ~p", [Message]),
  LST = [string:strip(string:sub_string(X, 4)) || X <- string:tokens(Message, "\r\n"), estring:start_with(X, "ST")],
  case length(LST) of
    1 -> [ST] = LST, {service, ST};
    _ -> noservice
  end.

send_msearch_response(Socket, IP, InPortNo, _Packet, ST, Service) ->
  Message = ssdp_message:build_msearch_response(ST, Service:get_uri(), Service:get_service_type()),
  lager:debug("[SSDP] Send response for ST : ~p ~p", [ST, Message]),
  gen_udp:send(Socket, IP, InPortNo, erlang:list_to_binary(Message)),
  ok.
