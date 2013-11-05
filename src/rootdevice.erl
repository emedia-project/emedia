-module(rootdevice).

-behaviour(gen_server).

-include("../include/upnp.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, get_service/1, get_uuid/0, get_ip_port/0, get_os/0, get_hostname/0]).
-export([get_st/0, get_uri/0, get_service_type/0]).

-define(SERVICE_TYPE, "upnp:rootdevice").
-define(SERVICE_URI, "/description").

% wrappers
start() -> 
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
  IpPort = inet_parse:ntoa(Ip) ++ ":" ++ integer_to_list(Port),
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
  {ok, Hostname} = inet:gethostname(),
  #rootdevice{
    uuid = uuid:generate(),
    os = ?UPNP,
    ip = get_active_ip(), 
    hostname = Hostname,
    port = eme_config:get(tcp_port),
    services = eme_config:get(services)
  }.

get_ip([]) ->
  get_loopback();

get_ip([If]) ->
  case inet:ifget(If, [addr]) of
    {ok, []} -> get_loopback();
    {_, [{_, Ip}]} -> Ip
  end.

get_loopback() ->
  get_loopback(get_iflist()).

get_loopback(If_list) ->
  get_ip([A || A <- If_list, inet:ifget(A,[addr]) == {ok,[{addr,{127,0,0,1}}]}]).

get_active_ip() ->
  get_active_ip(get_iflist()).

get_active_ip(If_list) ->
  get_ip([A || A <- If_list, inet:ifget(A,[addr]) /= {ok,[{addr,{127,0,0,1}}]}, filter_networkcard(list_to_binary(A))]).

get_iflist() ->
  {ok, IfList} = inet:getiflist(),
  IfList.

filter_networkcard(<<"vnic", _R/binary>>) ->
  false;
filter_networkcard(<<"vmnet", _R/binary>>) ->
  false;
filter_networkcard(_) ->
  true.
