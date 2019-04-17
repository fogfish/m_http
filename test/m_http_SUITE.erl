%% @doc
%%   common test suite for m_http public api
-module(m_http_SUITE).
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
http_pass_method(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < "X-Mock-Mthd: GET"
      ]
   ),
   m_http_mock:free().

%%
http_pass_url(_) -> 
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < "X-Mock-Url: http://example.com/"
      ]
   ),
   m_http_mock:free().

%%
http_pass_method_and_url(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://example.com/",

         _ < 200,
         _ < "X-Mock-Mthd: GET",
         _ < "X-Mock-Url: http://example.com/"
      ]
   ),
   m_http_mock:free().

%%
http_pass_head(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},
         _ > "Accept: */*",

         _ < 200,
         _ < "X-Mock-Accept: */*"
      ]
   ),
   m_http_mock:free().

%%
http_pass_body(_) -> 
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},
         _ > <<"abcdef">>,

         _ < 200,
         _ < "X-Mock-Body: abcdef"
      ]
   ),
   m_http_mock:free().

%%
http_code_200(_) ->
   m_http_mock:init(200, [], []),
   {ok, 200} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200
      ]
   ),
   m_http_mock:free().

%%
http_code_404(_) ->
   m_http_mock:init(404, [], []),
   {error, {require, 200, 404}} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200
      ]
   ),
   m_http_mock:free().

%%
http_head_match_any(_) ->
   m_http_mock:init(200, [{<<"Content-Type">>, <<"text/plain">>}], []),
   {ok, <<"text/plain">>} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < "Content-Type: _"
      ]
   ),
   m_http_mock:free().

%%
http_head_match_value(_) ->
   m_http_mock:init(200, [{<<"Content-Type">>, <<"text/plain">>}], []),
   {ok, <<"text/plain">>} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < "Content-Type: text/plain"
      ]
   ),
   m_http_mock:free().

%%
http_head_match_error(_) ->
   m_http_mock:init(200, [{<<"Content-Type">>, <<"text/plain">>}], []),
   {error, {require,<<"application/json">>,<<"text/plain">>}} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < "Content-Type: application/json"
      ]
   ),
   m_http_mock:free().

%%
http_body_match_any(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"text/plain">>}],
      [<<"abcde">>]
   ),
   {ok, <<"abcde">>} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < '*'
      ]
   ),
   m_http_mock:free().
   
%%
http_body_decode_and_match(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/json">>}], 
      [<<"{\"a\":\"abcde\"}">>]
   ),
   {ok, <<"abcde">>} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < lens:at(<<"a">>)
      ]
   ),
   m_http_mock:free().

