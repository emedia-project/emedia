-define(UPNP, "eMedia/1.0 UPnP/1.0").

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
