-module(m_http_codec).

-export([
   typecast/1
,  encode/2
,  decode/1
,  decode/2
]).

%%
%% decode scalar type
-spec typecast(binary()) -> _.

typecast(<<"true">>)  ->
   true;
typecast(<<"false">>) ->
   false;
typecast(X)
 when is_binary(X) ->
   case re:run(X, "^(-?[0-9]+)(\\.[0-9]+)?([eE][+-]?[0-9])?$") of
      {match, [_, _]}       -> typecast:i(X);
      {match, [_, _, _]}    -> typecast:f(X); 
      {match, [_, _, _, _]} -> typecast:f(X);
      nomatch               -> deref(X)
   end.

deref(X) ->
   case binary:referenced_byte_size(X) of
      Size when Size > 2 * byte_size(X) -> 
         binary:copy(X);
      _ -> 
         X
   end.

%%
%%
-spec encode(_, _) -> datum:either(_).

encode(<<"application/json">>, Json) ->
   m_http_codec_json:encode(Json);
encode(<<"application/x-www-form-urlencoded">>, Form) ->
   m_http_codec_www_form:encode(Form);
encode(<<"text/plain">>, Text) ->
   m_http_codec_text:encode(Text);
encode(_, Binary) ->
   {ok, Binary}.

%%
%%
-spec decode(_, _) -> datum:either(_).

decode([{_Code, _Text, Head} = Http | Data]) ->
   case decode(lens:get(lens:pair(<<"Content-Type">>), Head), Data) of
      {ok, Content} when is_list(Content) ->
         {ok, [Http | Content]};
      {ok, Content} ->
         {ok, [Http, Content]};
      {error, _} = Error ->
         Error
   end.

decode(<<"application/json", _/binary>>, Data) ->
   m_http_codec_json:decode(Data);
decode(<<"application/x-www-form-urlencoded", _/binary>>, Data) -> 
   m_http_codec_www_form:decode(Data);
decode(<<"text/plain", _/binary>>, Data) ->
   m_http_codec_text:decode(Data);
decode(_, Data) ->
   {ok, Data}.
