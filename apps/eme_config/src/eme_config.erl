%% @author Gregoire Lejeune <gregoire.lejeune@free.fr>
%% @copyright 2013 Gregoire Lejeune
%% @doc This module allow you to acces the emedia configuration
-module(eme_config).

-behaviour(gen_server).

-include("../include/eme_config.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-export([get/1, get/2]).

% wrappers

%% @doc Start the configuration server
start_link() -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Access a configuration information
-spec(get/1 :: (tcp_port | max_conn | services | ffprobe_path | scan_interval) -> any()).
get(tcp_port) ->
  gen_server:call(?MODULE, {get_tcp_port});
get(max_conn) ->
  gen_server:call(?MODULE, {get_max_conn});
get(services) ->
  gen_server:call(?MODULE, {get_services});
get(ffprobe_path) ->
  gen_server:call(?MODULE, {get_ffprobe_path});
get(ffmpeg_path) ->
  gen_server:call(?MODULE, {get_ffmpeg_path});
get(scan_interval) ->
  gen_server:call(?MODULE, {get_scan_interval});
get(db_path) ->
  gen_server:call(?MODULE, {get_db_path}).
-spec(get/2 :: (medias, string()) -> [string()]).
%% @doc Access a configuration information
get(medias, Type) ->
  gen_server:call(?MODULE, {get_medias, Type}).

% Server

%% @hidden
init([]) ->
  {ok, read_config()}.

%% @hidden
terminate(_Reason, _Config) -> 
  ok.

%% @hidden
handle_cast(_Message, Config) -> 
  {noreply, Config}.

%% @hidden
handle_info(_Message, Config) -> 
  {noreply, Config}.

%% @hidden
code_change(_OldVersion, Config, _Extra) -> 
  {ok, Config}.

%% @hidden
handle_call({get_tcp_port}, _From, Config) ->
  #emeconfig{tcp_port = TcpPort} = Config,
  {reply, TcpPort, Config};
handle_call({get_max_conn}, _From, Config) ->
  #emeconfig{max_conn = MaxConn} = Config,
  {reply, MaxConn, Config};
handle_call({get_services}, _From, Config) ->
  #emeconfig{services = Services} = Config,
  {reply, Services, Config};
handle_call({get_ffprobe_path}, _From, Config) ->
  #emeconfig{ffprobe_path = FFProbePath} = Config,
  {reply, FFProbePath, Config};
handle_call({get_ffmpeg_path}, _From, Config) ->
  #emeconfig{ffmpeg_path = FFMpegPath} = Config,
  {reply, FFMpegPath, Config};
handle_call({get_scan_interval}, _From, Config) ->
  #emeconfig{scan_interval = ScanInterval} = Config,
  {reply, ScanInterval, Config};
handle_call({get_db_path}, _From, Config) ->
  #emeconfig{db_path = DBPath} = Config,
  {reply, DBPath, Config};
handle_call({get_medias, Type}, _From, Config) ->
  #emeconfig{medias = Medias} = Config,
  {reply, get_medias_type(Medias, Type), Config};
handle_call(_Message, _From, Config) ->
  {reply, error, Config}.

% private

get_medias_type(Medias, Type) ->
  {_, MediaList} = lists:foldl(fun(Media, {Type1, Acc}) ->
      case re:split(Media, "\s*,\s*", [{return, list}]) of
        [Type1|Path] ->
          Path1 = string:join(Path, ","),
          Path2 = eme_utils:expand_path(Path1),
          {Type1, Acc ++ [Path2]};
        _ ->
          {Type1, Acc}
      end
    end, {Type, []}, Medias),
  MediaList.

read_config() ->
  lager:info("Check configuration"),
  find_config(?EMEDIACONFIG_PATH, #emeconfig{}).

find_config([], Config) ->
  Config;
find_config([F|H], Config) ->
  ConfFile = eme_utils:expand_path(F),
  case filelib:is_file(ConfFile) of
    true -> 
      find_config(H, read_config_file(ConfFile, Config));
    false -> 
      find_config(H, Config)
  end.

read_config_file(ConfFile, Config) ->
  lager:info("Update configuration with ~p", [ConfFile]),
  IniBin = case file:read_file(ConfFile) of
    {ok, IniBin0} ->
      IniBin0;
    {error, eacces} ->
      throw({file_permission_error, ConfFile});
    {error, enoent} ->
      Fmt = "Couldn't find server configuration file ~s.",
      Msg = list_to_binary(io_lib:format(Fmt, [ConfFile])),
      throw({startup_error, Msg})
  end,
  Lines = re:split(IniBin, "\r\n|\n|\r|\032", [{return, list}]),
  Config1 = lists:foldl(fun(Line, Acc) ->
        case string:strip(Line) of
          "" -> 
            Acc;
          "#" ++ _Rest ->
            Acc;
          Rest ->
            case re:split(Rest, "\s*=\s*", [{return, list}]) of
              [Key|Value] ->
                Value1 = string:join(Value, "="),
                Value2 = case re:split(Value1, "\s*#", [{return, list}]) of
                  [Value3] -> Value3;
                  [Value4|_] -> Value4
                end,
                case list_to_atom(Key) of
                  tcp_port ->
                    Acc#emeconfig{tcp_port = list_to_integer(Value2)};
                  max_conn ->
                    Acc#emeconfig{max_conn = list_to_integer(Value2)};
                  medias ->
                    #emeconfig{medias = Medias} = Acc,
                    Acc#emeconfig{medias = Medias ++ [Value2]};
                  scan_interval ->
                    Time = list_to_integer(Value2) * 60000,
                    if 
                      (Value2 > 0) and (Time < 4294967295) -> 
                        Acc#emeconfig{scan_interval = Time};
                      true -> 
                        Acc
                    end;
                  tmdb_api_key ->
                    Acc#emeconfig{tmdb_api_key = Value2};
                  ffprobe_path ->
                    Acc#emeconfig{ffprobe_path = eme_utils:expand_path(Value2)};
                  ffmpeg_path ->
                    Acc#emeconfig{ffmpeg_path = eme_utils:expand_path(Value2)};
                  db_path ->
                    Acc#emeconfig{db_path = eme_utils:expand_path(Value2)};
                  Other ->
                    lager:info("Unknow option `~p' : ignored", [Other]),
                    Acc
                end;
              _ ->
                Acc
            end
        end
    end, Config, Lines),
  Config1.

