-module(description).

-export([get/1]).

get(_Request) ->
  Version = case application:get_key(emedia, vsn) of
    {ok, Vsn} -> list_to_binary(Vsn);
    _ -> <<"unknow">>
  end,
  paris_response:render_view(
    description,
    [
      {url_base, "http://" ++ 
       eme_config:get(tcp_ip) ++ 
       ":" ++ integer_to_list(eme_config:get(tcp_port))},
      {hostname, eme_config:get(hostname)},
      {udn, binary_to_list(eme_config:get(uuid))},
      {version, Version}
      ],
    [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}]
  ).

