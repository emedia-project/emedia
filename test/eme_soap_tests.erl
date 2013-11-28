-module(eme_soap_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/soap_messages.hrl").

eme_soap_test_() ->
  {setup,
    fun setup/0, fun teardown/1,
    [
      ?_test(parse_browse())
    ]}.

% Helpers

setup() ->
  ?debugMsg("setup..."),
  ok = application:start(lager),
  ok.

teardown(_) ->
  ?debugMsg("teardown..."),
  ok.

% Tests

parse_browse() ->
  ?debugMsg("Test parse_browse..."),
  Data = " 
  <?xml version=\"1.0\" encoding=\"UTF-8\"?>
  <s:Envelope s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">
    <s:Body>
      <u:Browse xmlns:u=\"urn:schemas-upnp-org:service:ContentDirectory:1\">
        <ObjectID>A</ObjectID>
        <BrowseFlag>BrowseDirectChildren</BrowseFlag>
        <Filter>*</Filter>
        <StartingIndex>1</StartingIndex>
        <RequestedCount>10</RequestedCount>
        <SortCriteria></SortCriteria>
      </u:Browse>
    </s:Body>
  </s:Envelope>
  ",
  Result = eme_soap:parse_request(Data),
  ?assertEqual(Result, #browse{
      object_id = "A", 
      flag = "BrowseDirectChildren",
      filter = "*",
      starting_index = 1,
      requested_count = 10,
      sort_criteria = ""
    }).
