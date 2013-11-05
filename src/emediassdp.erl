-module(emediassdp).

-export([start/0, receiver/0]).

-define(M_SEARCH, "M-SEARCH").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  lager:info("emediassdp started"),
  {_S,_Pid}=udp_mcast:start({239,255,255,250},1900,?MODULE).

receiver() ->
  receive
    {udp, Socket, IP, InPortNo, Packet} ->
      lager:info("From: ~p - Port: ~p - Data: ~p",[IP,InPortNo,Packet]),
      handle(udp, Socket, IP, InPortNo, Packet),
      receiver();
    stop -> true;
    AnythingElse -> 
      lager:info("RECEIVED: ~p~n",[AnythingElse]),
      receiver()
  end.

%% ===================================================================
%% Internal functions
%% ===================================================================
handle(udp, Socket, IP, InPortNo, Packet) ->
  case is_msearch(Packet) of
    false -> 
      lager:error("Don't understand message ~p", [Packet]);
    true ->
      handle_msearch(Socket, IP, InPortNo, Packet)
  end.

is_msearch(Packet) ->
  eme_utils:starts_with(Packet, ?M_SEARCH).

handle_msearch(Socket, IP, InPortNo, Packet) ->
  ST = get_st(Packet),
  case rootdevice:get_service(ST) of
    {ok, Service} -> send_msearch_response(Socket, IP, InPortNo, Packet, ST, Service);
    {error, _Reason} -> lager:error("Unknow service type ~p", [ST])
  end,
  true.

get_st(Message) ->
  [ST] = [string:strip(string:sub_string(X, 4)) || X <- string:tokens(Message, "\r\n"), eme_utils:starts_with(X, "ST")],
  ST.

send_msearch_response(Socket, IP, InPortNo, _Packet, ST, Service) ->
  Message = emediassdp_message:build_msearch_response(ST, Service:get_uri(), Service:get_service_type()),
  lager:info("Send response for ST : ~p ~p", [ST, Message]),
  gen_udp:send(Socket, IP, InPortNo, erlang:list_to_binary(Message)),
  ok.
