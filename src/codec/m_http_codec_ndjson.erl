-module(m_http_codec_ndjson).
-compile({parse_transform, category}).

-export([encode/1, decode/1]).

-spec encode(_) -> datum:either(_).

encode(Json) ->
   [either ||
      cats:unit([m_http_codec_json:encode(X) || X <- Json]),
      cats:seqeunce(_)
   ].

-spec decode(_) -> datum:either(_).

decode(Json) ->
   [either ||
      cats:unit(binary:split(Json, [<<$\n>>, <<$\r,$\n>>], [trim, global])),
      cats:unit([m_http_codec_json:decode(X) || X <- _]),
      cats:seqeunce(_)
   ].
