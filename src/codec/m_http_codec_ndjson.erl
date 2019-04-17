-module(m_http_codec_ndjson).
-compile({parse_transform, category}).

-export([encode/1, decode/1]).

-spec encode(_) -> datum:either(_).

encode(Json) ->
   [either ||
      cats:unit([to_json(X) || X <- Json]),
      cats:sequence(_),
      cats:unit(_)
   ].

to_json(Data) ->
   try
      {ok, 
         [jsx:encode(Data), <<$\n>>]
      }
   catch _:_ ->
      {error, badarg}
   end.   

-spec decode(_) -> datum:either(_).

decode(Json) ->
   [either ||
      cats:unit(binary:split(erlang:iolist_to_binary(Json), [<<$\n>>, <<$\r,$\n>>], [trim, global])),
      cats:unit([m_http_codec_json:decode(X) || X <- _]),
      cats:sequence(_)
   ].
