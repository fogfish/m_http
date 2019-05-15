%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
%%   Copyright (c) 2016, Mario Cardona
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   common test suite for m_http public api
-module(m_http_implicit_SUITE).
-compile({parse_transform, category}).
-compile({parse_transform, generic}).

-export([all/0, init_per_suite/1, end_per_suite/1]).
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
init_per_suite(Config) ->
   {ok, _} = application:ensure_all_started(m_http),
   Config.

end_per_suite(_) ->
   application:stop(m_http).

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

%%
http_body_decode_and_match_with_accept(_) ->
   m_http_mock:init(200, 
      [], 
      [<<"{\"a\":\"abcde\"}">>]
   ),
   {ok, <<"abcde">>} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},
         _ > "Accept: application/json",

         _ < 200,
         _ < lens:at(<<"a">>)
      ]
   ),
   m_http_mock:free().

%%
%%
-record(adt, {a, b}).

http_body_decode_adt(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/json">>}], 
      [<<"{\"a\":\"abcde\",\"b\":1}">>]
   ),
   {ok, #adt{a = <<"abcde">>, b = 1}} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < #adt{
            a = lens:at(<<"a">>),
            b = lens:at(<<"b">>)
         }
      ]
   ),
   m_http_mock:free().

%%
http_body_encode_adt(_) -> 
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},
         _ > "Content-Type: application/json",
         _ > generic:from(#adt{a = <<"hello">>, b = <<"world">>}),

         _ < 200,
         _ < "X-Mock-Body: {\"a\":\"hello\",\"b\":\"world\"}"
      ]
   ),
   m_http_mock:free().
