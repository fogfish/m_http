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
%%   Example of m_http syntax, See doc/interface.md for detailed specification.
-module(m_http_syntax).
-compile({parse_transform, category}).

-export([example/0]).

example() ->
   [m_http ||
      _ > "GET http://httpbin.org/json",
      _ > "Accept: application/json",

      _ < 200,
      _ < "Content-Type: application/json",
      _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"title">>), lens:defined()),
      _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"slides">>))
   ].
