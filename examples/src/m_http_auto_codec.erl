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
-module(m_http_auto_codec).
-compile({parse_transform, category}).
-compile({parse_transform, generic}).

-export([gen/0, adt/0]).

-record(adt, {hello}).


gen() ->
   [m_http ||
      _ > "POST http://httpbin.org/post",
      _ > "Accept: application/json",
      _ > "Content-Type: application/json",
      _ > #{<<"hello">> => <<"world">>},

      _ < 200,
      _ < lens:at(<<"json">>)
   ].

adt() ->
   [m_http ||
      _ > "POST http://httpbin.org/post",
      _ > "Accept: application/json",
      _ > "Content-Type: application/json",
      _ > generic:from(#adt{hello = <<"world">>}),

      _ < 200,
      _ < #adt{
         hello = lens:c(lens:at(<<"json">>), lens:at(<<"hello">>))
      }
   ].

