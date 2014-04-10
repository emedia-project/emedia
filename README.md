# eMedia

A simple UPNP mediaserver

> **This is a alpha version**. I plan evolutions that will involved major modifications in the database schema. So please **use this software for tests only**. The more you test, the more you report issues, the more we have chance to have something that's fit to your needs.
>
> Thanks in advance for your help.

## Install

> eMedia depend on ffmpeg and ffprobe (which is part of ffmpeg). For more informations about `ffmpeg` and how to install it, see [http://www.ffmpeg.org](http://www.ffmpeg.org).

    git clone https://github.com/glejeune/emedia.git
    cd emedia
    ./start.sh --compile

See `./start.sh --help` for more options.

## Configuration

The configuration is store in the `config/emedia.config` file.

You can add comments in a configuration file. A comment start with a `%`.

A configuration accept the following parameters :

* `tcp_ip` : IP binding
* `tcp_port` : port used by the server
* `max_conn` : maximum connections accepted by the server
* `medias` : path to the differents media directories. This option must be repeated for every directory. The value is a path preceded by the type of media found in the directory (`A` for Audio, `V` for Video and `P` for Photo). Example :

        A,/my/audio/directory
        V,/my/video/directory
        P,/my/photos
        V,/to/classify
        A,/to/classify

    In this example, the `/to/classify` directory is given two times, once to specify that it contains videos and a second time for audio.
    
* `scan_interval` : time interval (in minutes) between every scan of the medias directories by the server
* `tmdb_api_key` : API key used to access [The Movie Database](https://www.themoviedb.org/)
* `ffprobe_path` : Path to `ffprobe`
* `ffmpeg_path` : Path to `ffmpeg`
* `db_path` : Path to the eMedia database

Example :

```erlang
{emedia, [
  {port, 9090}, 
  {ip, "0.0.0.0"},
  {max_conn, 100},
  {routes, [
    {"/description", description},
    {"/service/content_directory", service_content_directory},
    {"/service/content_directory/control", service_content_directory_control},
    {"/service/connection_manager", service_connection_manager},
    {"/media", media}
  ]},
  {medias, [
    "V,/home/user/videos",
    "V,/usr/share/videos",
    "P,/home/user/Photos",
    "A,/home/user/music"
  ]},
  {scan_interval, 60},
  {tmdb_api_key, "mY4p1k3y"},
  {ffprobe_path, "/usr/local/bin/ffprobe"},
  {ffmpeg_path, "/usr/local/bin/ffmpeg"},
  {db_path, "~/.emedia"}
]},
{mimetypes, [
  {load_async, true},
  {load_timeout, 10000},
  {load, [
    {default, [
      {<<"mkv">>, <<"video/x-matroska">>}
    ]}
  ]}
]}
```

## Architecture

![](emedia.png)

## Plan

* Rewrite `eme_db`
* Add "update" to the media scanner (allowing to detect changes in the media database)
* Support additional ressources
* Add a web interface
* Better media infos (using erlFFMpeg + emdbd) -> new media's classifications
* Use erlFFMpeg for media conversion
* Refactor SOAP support
* Rewrite `eme_ssdp`
* Rewrite `start.sh`
* Add DAAP support

## Licence

eMedia is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2013, 2014 Grégoire Lejeune <<gregoire.lejeune@free.fr>>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

