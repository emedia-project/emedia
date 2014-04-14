-module(rootdevice).

-behaviour(gen_server).

-include("../../include/rootdevice.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, get_service/1, get_uuid/0, get_ip_port/0, get_os/0, get_hostname/0]).
-export([get_st/0, get_uri/0, get_service_type/0]).

-define(SERVICE_TYPE, "upnp:rootdevice").
-define(SERVICE_URI, "/description").

% wrappers
start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_service(ST) ->
  gen_server:call(?MODULE, {get_service, ST}).

get_uuid() ->
  gen_server:call(?MODULE, {get_uuid}).

get_ip_port() ->
  gen_server:call(?MODULE, {get_ip_port}).

get_os() ->
  gen_server:call(?MODULE, {get_os}).

get_hostname() ->
  gen_server:call(?MODULE, {get_hostname}).

% public
get_st() ->
  ?SERVICE_TYPE.

get_uri() ->
  ?SERVICE_URI.

get_service_type() ->
  get_st().

% Server
init([]) ->
  {ok, root_device()}.

terminate(_Reason, _RootDevice) -> 
  ok.

handle_cast(_Message, RootDevice) -> 
  {noreply, RootDevice}.

handle_info(_Message, RootDevice) -> 
  {noreply, RootDevice}.

code_change(_OldVersion, RootDevice, _Extra) -> 
  {ok, RootDevice}.

handle_call({get_service, ST}, _From, RootDevice) ->
  #rootdevice{services = Services} = RootDevice,
  {reply, get_service(Services, ST), RootDevice};
handle_call({get_uuid}, _From, RootDevice) ->
  #rootdevice{uuid = Uuid} = RootDevice,
  {reply, Uuid, RootDevice};
handle_call({get_ip_port}, _From, RootDevice) ->
  #rootdevice{ip = Ip, port = Port} = RootDevice,
  IpPort = Ip ++ ":" ++ integer_to_list(Port),
  {reply, IpPort, RootDevice};
handle_call({get_os}, _From, RootDevice) ->
  #rootdevice{os = Os} = RootDevice,
  {reply, Os, RootDevice};
handle_call({get_hostname}, _From, RootDevice) ->
  #rootdevice{hostname = Hostname} = RootDevice,
  {reply, Hostname, RootDevice};
handle_call(_Message, _From, RootDevice) ->
  {reply, error, RootDevice}.

% private

get_service(Services, ST) ->
  case [X || X <- Services, string:equal(ST, X:get_st())] of
    [] -> {error, "no service found"};
    [Service] -> {ok, Service}
  end.

root_device() ->
  #rootdevice{
    uuid = eme_config:get(uuid),
    os = ?UPNP,
    ip = eme_config:get(ip),
    hostname = eme_config:get(hostname),
    port = eme_config:get(port),
    services = eme_config:get(services)
  }.

