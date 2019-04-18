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
%%   application/x-www-form-urlencoded codec
-module(m_http_codec_www_form).
-compile({parse_transform, category}).

-export([encode/1, decode/1]).

%%
%%
-spec encode(_) -> datum:either(_).

encode(Form) ->
   try
      {ok, [identity ||
         maps:to_list(Form),
         lists:map(fun to_pair/1, _),
         typecast:s(lists:join(<<$&>>, _))
      ]}
   catch _:_ ->
      {error, badarg}
   end.

to_pair(Pair) ->
   typecast:s(
      lists:join(<<$=>>, 
         [m_http_codec_url:escape(X) || X <- erlang:tuple_to_list(Pair)]
      )
   ).

-spec decode(_) -> datum:either(_).

decode(Form) ->
   try
      {ok, [identity ||
         binary:split(typecast:s(Form), <<$&>>, [trim, global]),
         lists:map(fun as_pair/1, _),
         maps:from_list(_)
      ]}
   catch _:_ ->
      {error, badarg}
   end.

as_pair(Pair) ->
   erlang:list_to_tuple(
      [m_http_codec_url:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).
