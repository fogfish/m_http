-module(m_http_app).
-behaviour(application).

-export([start/2, stop/1]).

%%
%%
start(_Type, _Args) ->
   ok = hackney_pool:start_pool(m_http_pool, []),
   {ok, self()}.

%%
%%
stop(_State) ->
   hackney_pool:stop_pool(m_http_pool),
   ok.
