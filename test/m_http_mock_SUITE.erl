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
-module(m_http_mock_SUITE).
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
m_http_mock(_) ->
   m_http_mock:init(200, [{<<"X-Mock-It">>, <<"mock">>}], [<<"mock">>]),
   {ok, [<<"mock">>]} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com"},

         _ < 200,
         _ < "X-Mock-It: mock",
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
m_http_mock_status_code(_) ->
   m_http_mock:init(404),
   {ok, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com"},

         _ < 404
      ]
   ),
   m_http_mock:free().

%%
m_http_mock_body(_) ->
   m_http_mock:init([<<"mock">>]),
   {ok, [<<"mock">>]} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com"},

         _ < 200,
         _ < '*'
      ]
   ),
   m_http_mock:free().

%%
m_http_mock_with_router(_) ->
   m_http_mock:init(
      fun
         (#{url := "http://example.com/a"}) -> {200, [], [<<"a">>]};
         (#{url := "http://example.com/b"}) -> {200, [], [<<"b">>]};
         (_) -> {500, [], []}
      end
   ),

   {ok, [<<"a">>]} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/a"},

         _ < 200,
         _ < '*'
      ]
   ),
   {ok, [<<"b">>]} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/b"},

         _ < 200,
         _ < '*'
      ]
   ),
   {error, _} = m_http:once(
      [m_http ||
         _ > {'GET', "http://example.com/"},

         _ < 200,
         _ < '*'
      ]
   ),

   m_http_mock:free().


