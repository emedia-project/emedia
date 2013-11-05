-module(mediaserver).

-behaviour(gen_server).

-include("../include/upnp.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).
-export([get_st/0, get_uri/0, get_service_type/0]).

-define(SERVICE_TYPE, "urn:schemas-upnp-org:device:MediaServer:1").
-define(SERVICE_URI, "/description").

% wrappers
start() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% public
get_st() ->
  ?SERVICE_TYPE.

get_uri() ->
  ?SERVICE_URI.

get_service_type() ->
  get_st().

% Server
init([]) ->
  {ok, #mediaserver{}}.

terminate(_Reason, _MediaServer) -> 
  ok.

handle_cast(_Message, MediaServer) -> 
  {noreply, MediaServer}.

handle_info(_Message, MediaServer) -> 
  {noreply, MediaServer}.

code_change(_OldVersion, MediaServer, _Extra) -> 
  {ok, MediaServer}.

handle_call(_Message, _From, MediaServer) ->
  {reply, error, MediaServer}.

