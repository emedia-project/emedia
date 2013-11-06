-module(media_scanner).

-behaviour(gen_server).

-include("../include/upnp.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0]).
-export([terminate_scan/0]).

-define(FFPROB_OPTIONS, "-v quiet -print_format json -show_format -show_streams").

% wrappers
start() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

terminate_scan() ->
  gen_server:call(?MODULE, {terminate_scan}).

% Server
init([]) ->
  erlang:send_after(1000, self(), trigger),
  {ok, #media_scanner{}}.

terminate(_Reason, _MediaScanner) -> 
  ok.

handle_cast(_Message, MediaScanner) -> 
  {noreply, MediaScanner}.

handle_info(_Message, MediaScanner) -> 
  #media_scanner{scanning = Scanning} = MediaScanner,
  MediaScanner1 = case Scanning of
    true -> MediaScanner;
    false -> p_start_scan(MediaScanner)
  end,
  {noreply, MediaScanner1}.

code_change(_OldVersion, MediaScanner, _Extra) -> 
  {ok, MediaScanner}.

handle_call({terminate_scan}, _From, MediaScanner) ->
  {reply, ok, p_terminate_scan(MediaScanner)};
handle_call(_Message, _From, MediaScanner) ->
  {reply, error, MediaScanner}.

% Private

p_start_scan(MediaScanner) ->
  lager:info("Start scanning..."),
  MediaScanner1 = MediaScanner#media_scanner{scanning = true},
  spawn_link(fun run_scan/0),
  MediaScanner1.

p_terminate_scan(MediaScanner) ->
  lager:info("Scan complete."),
  MediaScanner1 = MediaScanner#media_scanner{scanning = false},
  Interval = eme_config:get(scan_interval),
  erlang:send_after(Interval, self(), trigger),
  MediaScanner1.

run_scan() ->
  lager:info("*********************** Scanning...1"),
  timer:sleep(5000),
  lager:info("*********************** Scanning...2"),
  timer:sleep(5000),
  lager:info("*********************** Scanning...3"),
  timer:sleep(5000),
  lager:info("*********************** Scanning...done"),
  media_scanner:terminate_scan().
