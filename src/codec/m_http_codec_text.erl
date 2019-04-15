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
