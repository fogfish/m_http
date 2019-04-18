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
-module(m_http_codec_text).

-export([encode/1, decode/1]).


%%
%%
-spec encode(_) -> datum:either(_).

encode(Text) ->
   try
      {ok, typecast:s(Text)}
   catch _:_ ->
      {error, badarg}
   end.


-spec decode(_) -> datum:either(_).

decode(Text) ->
   {ok, typecast:s(Text)}.
