%% Record for configuration

-define(EMEDIASERVER_TCP_PORT, 8080).
-define(EMEDIASERVER_IP, "0.0.0.0").
-define(EMEDIASERVER_MAX_CONN, 100).
-define(EMEDIASERVER_SCAN_INTERVAL, 300).
-define(EMEDIASERVER_SERVICES, [rootdevice, mediaserver]).

-record(emeconfig, {
    hostname,
    uuid,
    tcp_ip,
    tcp_port,
    max_conn,
    services,
    medias,
    scan_interval,
    tmdb_api_key,
    ffprobe_path,
    ffmpeg_path,
    db_path
  }).
