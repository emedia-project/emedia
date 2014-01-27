%%% CODE-START
%%% Usage 1> {S,Pid}=udp_mcast:start({239,255,255,250},1900).
%%% Stolen from http://www.nabble.com/Beginner:-Windows-UDP-multicast-receive-td18096904.html
-module(udp_mcast).
-export([open/2,start/3]).
-export([stop/1]).

open(Ip,Port) ->
    Opts1=[{reuseaddr,true},
    {multicast_loop,false},
    {multicast_if,{0,0,0,0}},
    {multicast_ttl,4}],
    {ok,S}=gen_udp:open(Port,Opts1),
    inet:setopts(S,[{add_membership,{Ip,{0,0,0,0}}}]),
    S.

close(S) -> gen_udp:close(S).

start(Ip,Port,Module)    ->
    S=open(Ip,Port),
    Pid=spawn(Module,receiver,[]),
    gen_udp:controlling_process(S,Pid),
    {S,Pid}.

stop({S,Pid}) ->
    close(S),
    Pid ! stop.

%% Example receiver() implemented in module
%% receiver() ->
%%     receive
%%         {udp, Socket, IP, InPortNo, Packet} ->
%%             io:format("~n~nFrom: ~p~nIP: ~p~nData:
%% ~p~n",[IP,InPortNo,Packet]),
%%             receiver();
%%         stop -> true;
%%         AnythingElse -> io:format("RECEIVED: ~p~n",[AnythingElse]),
%%             receiver()
%%     end.
%%% CODE-END
