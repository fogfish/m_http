%% @doc
%%   example of declarative testing of RESTfull API
-module(m_http_mock_SUITE).
-compile({parse_transform, category}).

-export([all/0]).
-compile(export_all).

%%
all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
%%
httpbin_status_200(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/status/200",

         _ < 200
      ]
   ),
   m_http_mock:free().

%%
%%
httpbin_headers(_) ->
   m_http_mock:init(200, 
      [
         {<<"Host">>, <<"httpbin.org">>},
         {<<"Content-Type">>, <<"application/json">>}
      ],
      [<<"{}">>]
   ),
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/headers",

         _ < 200,
         _ < "Host: _",
         _ < "Content-Type: application/json",
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
%%
httpbin_json(_) ->
   m_http_mock:init(200, 
      [
         {<<"Host">>, <<"httpbin.org">>},
         {<<"Content-Type">>, <<"application/json">>}
      ],
      [<<"{\"slideshow\":{\"title\":\"mock title\", \"slides\":[]}}">>]
   ),
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/json",
         _ > "Accept: application/json",

         _ < 200,
         _ < "Content-Type: application/json",
         _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"title">>), lens:defined()),
         _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"slides">>))
      ]
   ),
   m_http_mock:free().
