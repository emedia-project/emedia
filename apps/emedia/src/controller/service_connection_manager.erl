-module(service_connection_manager).

-export([get/1]).

get(_Request) ->
  paris_response:render_view(
    service_connection_manager,
    [],
    [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}]
  ).
