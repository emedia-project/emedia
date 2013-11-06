-define(UPNP, "eMedia/1.0 UPnP/1.0").

%% records for services
-record(rootdevice, {
    uuid, 
    wirelessmode, 
    port, 
    descriptionuri="/description/fetch", 
    services=[], 
    os,
    ip, 
    hostname,
    rootdevice="upnp:rootdevice", 
    elementname="device"
  }).

-record(mediaserver, {}).

%% Record for configuration

-define(EMEDIASERVER_TCP_PORT, 8080).
-define(EMEDIASERVER_MAX_CONN, 100).
-define(EMEDIASERVER_SERVICES, [rootdevice, mediaserver]).
-define(EMEDIACONFIG_PATH, [
    "/etc/emedia.conf", 
    "/Library/Application Support/eMedia/emedia.conf", 
    "~/Library/Application Support/eMedia/emedia.conf", 
    "~/.emedia/emedia.conf", 
    "emedia.conf"
  ]).

-record(emeconfig, {
    tcp_port = ?EMEDIASERVER_TCP_PORT,
    max_conn = ?EMEDIASERVER_MAX_CONN,
    services = ?EMEDIASERVER_SERVICES,
    medias = [],
    scan_interval = 300000,
    tmdb_api_key = false,
    ffprobe_path = "ffprobe"
  }).

%% Record for the media scanner

-record(media_scanner, {
    scanning = false,
    last_scan = false,
    last_addition,
    last_update
  }).
