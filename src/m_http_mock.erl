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
%%   mock networking stack
-module(m_http_mock).

-export([
   init/1
,  init/3
,  free/0
]).

%%
%%
init(Router)
 when is_function(Router) ->
   init(),
   response(Router);

init(Body)
 when is_list(Body) ->
   init(fun(_) -> {200, [], Body} end);

init(Code)
 when is_integer(Code) ->
   init(fun(_) -> {Code, [], []} end).

%%
%%
init(Code, Head, Body) ->
   init(fun(_) -> {Code, Head, Body} end).

%%
%%
init() ->
   meck:new(hackney, [unstick]).

%%
%%
free() ->
   meck:unload(hackney).

%%
%%
response(Router) ->
   meck:expect(hackney, request,
      fun(Mthd, Url, ReqHead, ReqBody, _Opts) ->
         MockHead = [
            {<<"X-Mock-Mthd">>, typecast:s(Mthd)}
         ,  {<<"X-Mock-Url">>,  typecast:s(Url)}
         ,  {<<"X-Mock-Body">>, typecast:s(ReqBody)}
         |  [{<<"X-Mock-", H/binary>>, V}  || {H, V} <- ReqHead]
         ],
         {Code, Head, Body} = Router(#{method => Mthd, url => Url, headers => ReqHead, body => ReqBody}),
         stream_http(Body),
         {ok, Code, Head ++ MockHead, undefined}
      end
   ).

stream_http([]) ->
   meck:expect(hackney, stream_body, fun(_) -> done end);

stream_http([Head | Tail]) ->
   meck:expect(hackney, stream_body, fun(_) -> stream_http(Tail), {ok, Head} end).
