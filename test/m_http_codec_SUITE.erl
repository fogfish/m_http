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
%%   common test suite for m_http codec(s)
-module(m_http_codec_SUITE).
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
http_header_encode(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Binary: text/plain",
         _ > "Int: 10",
         _ > "Float: 1.0",
         _ > "E: 1.0e2",
         _ > "True: true",
         _ > "False: false",

         _ < 200,
         _ < "X-Mock-Binary: text/plain",
         _ < "X-Mock-Int: 10",
         _ < "X-Mock-Float: 1.0",
         _ < "X-Mock-E: 1.0e2",
         _ < "X-Mock-True: true",
         _ < "X-Mock-False: false"
      ]
   ),
   m_http_mock:free().

%%
%%
http_text_encode_binary(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: text/plain",
         _ > <<"abcdef">>,

         _ < 200,
         _ < "X-Mock-Body: abcdef"
      ]
   ),
   m_http_mock:free().

%%
%%
http_text_encode_iolist(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: text/plain",
         _ > [<<"abcdef">>],

         _ < 200,
         _ < "X-Mock-Body: abcdef"
      ]
   ),
   m_http_mock:free().

%%
%%
http_text_encode_invalid(_) ->
   m_http_mock:init(200, [], []),
   {error, badarg} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: text/plain",
         _ > {1, 2, 3, 4, 5},

         _ < 200
      ]
   ),
   m_http_mock:free().

%%
%%
http_text_decode(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"text/plain">>}],
      [<<"abcdef">>]
   ),
   {ok, <<"abcdef">>} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
%%
http_json_encode(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: application/json",
         _ > #{<<"a">> => <<"abcdef">>},

         _ < 200,
         _ < "X-Mock-Body: {\"a\":\"abcdef\"}"
      ]
   ),
   m_http_mock:free().

%%
%%
http_json_encode_invalid(_) ->
   m_http_mock:init(200, [], []),
   {error, badarg} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: application/json",
         _ > {1, 2, 3, 4, 5},

         _ < 200
      ]
   ),
   m_http_mock:free().

%%
%%
http_json_decode(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/json">>}],
      [<<"{\"a\":\"abcdef\"}">>]
   ),
   {ok, #{<<"a">> := <<"abcdef">>}} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
%%
http_json_decode_invalid(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/json">>}],
      [<<"{1, 2, 3, 4, 5}">>]
   ),
   {error, badarg} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200
      ]
   ),
   m_http_mock:free().

%%
%%
http_www_form_encode(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: application/x-www-form-urlencoded",
         _ > #{<<"a">> => <<" ">>, <<"b">> => <<"abcdef">>},

         _ < 200,
         _ < "X-Mock-Body: a=%20&b=abcdef"
      ]
   ),
   m_http_mock:free().

%%
%%
http_www_form_decode(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
      [<<"a=%20&b=abcdef">>]
   ),
   {ok, #{<<"a">> := <<" ">>, <<"b">> := <<"abcdef">>}} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
%%
http_ndjson_encode(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: application/x-ndjson",
         _ > [#{<<"a">> => <<"abcdef">>}, #{<<"b">> => <<"abcdef">>}],

         _ < 200,
         _ < "X-Mock-Body: {\"a\":\"abcdef\"}\n{\"b\":\"abcdef\"}\n"
      ]
   ),
   m_http_mock:free().

http_ndjson_encode_binary(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'PUT', "http://example.com/"},
         _ > "Content-Type: application/x-ndjson",
         _ > <<"{\"a\":\"abcdef\"}\n{\"b\":\"abcdef\"}\n">>,

         _ < 200,
         _ < "X-Mock-Body: {\"a\":\"abcdef\"}\n{\"b\":\"abcdef\"}\n"
      ]
   ),
   m_http_mock:free().
%%
%%
http_ndjson_decode(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/x-ndjson">>}],
      [<<"{\"a\":\"abcdef\"}\n{\"b\":\"abcdef\"}\n">>]
   ),
   {ok, [
      #{<<"a">> := <<"abcdef">>}
   ,  #{<<"b">> := <<"abcdef">>}
   ]} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
%%
http_ndjson_decode_invalid(_) ->
   m_http_mock:init(200, 
      [{<<"Content-Type">>, <<"application/x-ndjson">>}],
      [<<"{1, 2, 3, 4, 5}">>]
   ),
   {error, badarg} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200
      ]
   ),
   m_http_mock:free().
