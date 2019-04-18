%% @doc
%%   example of declarative testing of RESTfull API
-module(httpbin_SUITE).
-compile({parse_transform, category}).

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

init_per_suite(Config) ->
   application:ensure_all_started(m_http),
   Config.

end_per_suite(_Config) ->
   ok.

%%
%%
httpbin_status_200(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/status/200",

         _ < 200
      ]
   ).

%%
%%
httpbin_status_404(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/status/404",

         _ < 404
      ]
   ).

%%
%%
httpbin_headers(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/headers",

         _ < 200,
         _ < "Host: _",
         _ < "Content-Type: application/json",
         _ < '*'
      ]
   ).

%%
%%
httpbin_json(_) ->
   {ok, _} = m_http:once(
      [m_http ||
         _ > "GET http://httpbin.org/json",
         _ > "Accept: application/json",

         _ < 200,
         _ < "Content-Type: application/json",
         _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"title">>), lens:defined()),
         _ < lens:c(lens:at(<<"slideshow">>), lens:at(<<"slides">>))
      ]
   ).
