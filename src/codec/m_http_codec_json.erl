-module(m_http_codec_json).

-export([encode/1, decode/1]).

%%
%%
-spec encode(_) -> datum:either(_).

encode(Json) ->
   try
      {ok, 
         jsx:encode(Json)
      }
   catch _:_ ->
      {error, badarg}
   end.

-spec decode(_) -> datum:either(_).

decode(Json) ->
   try
      {ok, 
         jsx:decode(erlang:iolist_to_binary(Json), [return_maps])
      }
   catch _:_ ->
      {error, badarg}
   end.
