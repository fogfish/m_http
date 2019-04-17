%% @doc
%%   mock networking stack
-module(m_http_mock).

-export([
   init/0
,  init/3
,  free/0
,  response/3
]).

%%
%%
init() ->
   meck:new(hackney, [unstick]).

init(Code, Head, Body) ->
   init(),
   response(Code, Head, Body).

%%
%%
free() ->
   meck:unload(hackney).

%%
%%
response(Code, Head, Body) ->
   meck:expect(hackney, request,
      fun(Mthd, Url, ReqHead, ReqBody, _Opts) ->
         MockHead = [
            {<<"X-Mock-Mthd">>, typecast:s(Mthd)}
         ,  {<<"X-Mock-Url">>,  typecast:s(Url)}
         ,  {<<"X-Mock-Body">>, typecast:s(ReqBody)}
         |  [{<<"X-Mock-", H/binary>>, V}  || {H, V} <- ReqHead]
         ], 
         {ok, Code, Head ++ MockHead, undefined}
      end
   ),
   stream_http(Body).

stream_http([]) ->
   meck:expect(hackney, stream_body, fun(_) -> done end);

stream_http([Head | Tail]) ->
   meck:expect(hackney, stream_body, fun(_) -> stream_http(Tail), {ok, Head} end).
