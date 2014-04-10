-module(service_content_directory).

-export([get/1]).

get(_Request) ->
  paris_response:render_view(
    service_content_directory,
    [],
    [{<<"Content-Type">>, <<"text/xml; charset=utf-8">>}]
  ).
