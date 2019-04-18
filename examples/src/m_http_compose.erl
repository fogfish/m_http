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
