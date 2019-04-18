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
%%   Example of m_http compose feature
-module(m_http_compose).
-compile({parse_transform, category}).

-export([hof/0]).

hof() ->
   [m_state ||
       _ <- httpbin_status_code(),
      UA <- httpbin_get(),
    Text <- httpbin_json(),
      cats:unit(#{ua => UA, text => Text})
   ].


httpbin_status_code() ->
   [m_http ||
      _ > "GET http://httpbin.org/get",

      _ < 200
   ].

httpbin_get() ->
   [m_http ||
      _ > "GET http://httpbin.org/get",
      _ > "Accept: application/json",

      _ < 200,
      _ < "Content-Type: application/json",
      _ < lens:c(lens:at(<<"headers">>), lens:at(<<"User-Agent">>))
   ].

httpbin_json() ->
   [m_http ||
      _ > "GET http://httpbin.org/json",
      _ > "Accept: application/json",

      _ < 200,
      _ < "Content-Type: application/json",
      _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"title">>))
   ].
