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
-module(m_http_codec).

-export([
   typecast/1
,  encode/2
,  decode/1
,  decode/2
]).

%%
%% decode scalar type
-spec typecast(binary()) -> _.

typecast(<<"true">>)  ->
   true;
typecast(<<"false">>) ->
   false;
typecast(X)
 when is_binary(X) ->
   case re:run(X, "^(-?[0-9]+)(\\.[0-9]+)?([eE][+-]?[0-9])?$") of
      {match, [_, _]}       -> typecast:i(X);
      {match, [_, _, _]}    -> typecast:f(X); 
      {match, [_, _, _, _]} -> typecast:f(X);
      nomatch               -> deref(X)
   end.

deref(X) ->
   case binary:referenced_byte_size(X) of
      Size when Size > 2 * byte_size(X) -> 
         binary:copy(X);
      _ -> 
         X
   end.

%%
%%
-spec encode(_, _) -> datum:either(_).

encode(<<"application/json", _/binary>>, Json) ->
   m_http_codec_json:encode(Json);
encode(<<"application/x-ndjson", _/binary>>, Json) ->
   m_http_codec_ndjson:encode(Json);
encode(<<"application/x-www-form-urlencoded", _/binary>>, Form) ->
   m_http_codec_www_form:encode(Form);
encode(<<"text/plain", _/binary>>, Text) ->
   m_http_codec_text:encode(Text);
encode(_, Binary) ->
   {ok, Binary}.

%%
%%
-spec decode(_, _) -> datum:either(_).

decode([{_Code, _Text, Head} = Http | Data]) ->
   case decode(lens:get(lens:pair(<<"Content-Type">>), Head), Data) of
      {ok, Content} ->
         {ok, [Http, Content]};
      {error, _} = Error ->
         Error
   end.

decode(<<"application/json", _/binary>>, Data) ->
   m_http_codec_json:decode(Data);
decode(<<"application/x-ndjson">>, Data) ->
   m_http_codec_ndjson:decode(Data);
decode(<<"application/x-www-form-urlencoded", _/binary>>, Data) -> 
   m_http_codec_www_form:decode(Data);
decode(<<"text/plain", _/binary>>, Data) ->
   m_http_codec_text:decode(Data);
decode(_, Data) ->
   {ok, Data}.
