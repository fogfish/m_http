-module(m_http_explicit_SUITE).
-compile({parse_transform, category}).

-export([all/0]).
-compile(export_all).

%%
all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
http_pass_method(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         cats:new("http://example.com/"),
         cats:method('GET'),

         [{200, _, Head} | _] <- cats:request(60000),
         #{
            <<"X-Mock-Mthd">> := <<"GET">>
         } =< maps:from_list(Head),
         cats:unit(pass)
      ]
   ),
   m_http_mock:free().

%%
http_pass_url(_) -> 
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         cats:new("http://example.com/"),
         cats:method('GET'),

         [{200, _, Head} | _] <- cats:request(60000),
         #{
            <<"X-Mock-Url">> := <<"http://example.com/">>
         } =< maps:from_list(Head),
         cats:unit(pass)
      ]
   ),
   m_http_mock:free().

%%
http_pass_method_and_url(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         cats:new("http://example.com/"),
         cats:method('GET'),

         [{200, _, Head} | _] <- cats:request(60000),
         #{
            <<"X-Mock-Mthd">> := <<"GET">>,
            <<"X-Mock-Url">>  := <<"http://example.com/">>
         } =< maps:from_list(Head),
         cats:unit(pass)
      ]
   ),
   m_http_mock:free().

%%
http_pass_socket_options(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         cats:new("http://example.com/"),
         cats:so([]),
         cats:method('GET'),

         [{200, _, Head} | _] <- cats:request(60000),
         #{
            <<"X-Mock-Mthd">> := <<"GET">>,
            <<"X-Mock-Url">>  := <<"http://example.com/">>
         } =< maps:from_list(Head),
         cats:unit(pass)
      ]
   ),
   m_http_mock:free().

%%
http_pass_head(_) ->
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         cats:new("http://example.com/"),
         cats:method('GET'),
         cats:header('Accept', "*/*"),

         [{200, _, Head} | _] <- cats:request(60000),
         #{
            <<"X-Mock-Accept">> := <<"*/*">>
         } =< maps:from_list(Head),
         cats:unit(pass)
      ]
   ),
   m_http_mock:free().

%%
http_pass_body(_) -> 
   m_http_mock:init(200, [], []),
   {ok, _} = m_http:once(
      [m_http ||
         cats:new("http://example.com/"),
         cats:method('GET'),
         cats:payload(<<"abcdef">>),

         [{200, _, Head} | _] <- cats:request(60000),
         #{
            <<"X-Mock-Body">> := <<"abcdef">>
         } =< maps:from_list(Head),
         cats:unit(pass)
      ]
   ),
   m_http_mock:free().

