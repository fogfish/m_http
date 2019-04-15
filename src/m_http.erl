-module(m_http).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").

%%
%%
-export([start/0]).
-export([unit/1, fail/1, '>>='/2, putT/1, getT/1]).
-export([once/1, once/2]).

%%
%%
-type m(A)    :: fun((_) -> [A|_]).
-type f(A, B) :: fun((A) -> m(B)).

-record('GET',    {uri, headers = []}).

%%
%% @doc starts application and its deps, use command in shell for RnD purpose 
-spec start() -> datum:either(_).

start() ->
   application:ensure_all_started(?MODULE).


%%%----------------------------------------------------------------------------   
%%%
%%% http monad
%%%
%%%----------------------------------------------------------------------------

%%
%%
-spec unit(A) -> m(A).

unit(X) ->
   m_state:unit(X).

%%
%%
-spec fail(_) -> _.

fail(X) ->
   m_state:fail(X).

%%
%%
-spec '>>='(m(A), f(A, B)) -> m(B).

'>>='(X, Fun) ->
   m_state:'>>='(X, Fun).

%%
%% @doc 
-spec putT(_) -> m(_).

putT({Mthd, Url}) when is_atom(Mthd) ->
   [m_state || new(Url), method(Mthd)];

putT(Expr) when is_list(Expr) ->
   case parse_either_request_or_header(Expr) of
      {request, Mthd, Url} ->
         putT({typecast:atom(Mthd), Url});
      {header, Head, Value} ->
         header(typecast:s(Head), hv(typecast:s(Value)));
      {payload, Value} ->
         payload(Value)
   end;

putT(X) ->
   payload(X).

%%
%% @doc
-spec getT(_) -> m(_).

getT(Code) when is_integer(Code) ->
   [m_state ||
      request(),
      require(code, Code)
   ];

getT(Head)
 when is_list(Head) ->
   [H, V] = binary:split(typecast:s(Head), <<$:>>),
   case hv(V) of
      <<$_>> ->
         require(header, lens:pair(H));
      Value  ->
         require(header, lens:c(lens:pair(H), lens:require(Value)))
   end;

getT('*') ->
   require(content, lens:id());

getT(Lens) ->
   require(content, Lens).


%%
%% @doc evaluate monadic expression
-spec once(m(_)) -> _.

once(Expr) ->
   once(Expr, #{so => []}).

once(Expr, SOpt) ->
   try
      [Result | _] = Expr(SOpt),
      {ok, Result}
   catch throw:Reason ->
      {error, Reason}
   end.

%%%----------------------------------------------------------------------------   
%%%
%%% helper methods
%%%
%%%----------------------------------------------------------------------------

%%
%%
-spec parse_either_request_or_header(string()) -> 
   {request, string(), string()} | {header, string(), string()} | {payload, string()}.

parse_either_request_or_header([H | _] = Expr)
 when is_integer(H) ->
   parse_either_request_or_header(Expr, []);
parse_either_request_or_header(Expr) ->
   {payload, Expr}.

parse_either_request_or_header([$  | Tail], Acc) ->
   {request, lists:reverse(Acc), Tail};
parse_either_request_or_header([$: | Tail], Acc) ->
   {header, lists:reverse(Acc), Tail};
parse_either_request_or_header([Head | Tail], Acc) ->
   parse_either_request_or_header(Tail, [Head | Acc]).


%%
%% lenses to http context
l_req_url() ->
   lens:c(lens:at(req), lens:hd(), lens:ti(#'GET'.uri)).

l_req_method() ->
   lens:c(lens:at(req), lens:hd(), lens:t1()).

l_req_headers() ->
   lens:c(lens:at(req), lens:hd(), lens:ti(#'GET'.headers)).

l_req_header(Head) ->
   lens:c(lens:at(req), lens:hd(), lens:ti(#'GET'.headers), lens:pair(Head, ?None)).

l_req_payload() ->
   lens:c(lens:at(req), lens:tl()).



%%
%% create a context for http request
-spec new(_) -> m(_).

new(Url) ->
   fun(State) ->
      Request = #'GET'{
         uri     = Url,
         headers = [
            {<<"Connection">>, <<"keep-alive">>},
            {<<"Accept">>,     <<"*/*">>}
         ]
      },
      [Url | State#{req => [Request]}]
   end.


%%
%% set method of http request
-spec method(_) -> m(_).

method(Mthd) ->
   m_state:put(l_req_method(), Mthd).


%%
%% add header to http request
-spec header(_, _) -> m(_).

header(Head, Value) ->
   m_state:put(l_req_header(Head), Value).

hv(<<$\s, X/binary>>) -> hv(X);
hv(<<$\t, X/binary>>) -> hv(X);
hv(<<$\n, X/binary>>) -> hv(X);
hv(<<$\r, X/binary>>) -> hv(X);
hv(X) -> m_http_codec:typecast(X).


%%
%% encode and add payload to http request
-spec payload(_) -> m(_).

payload(Value) ->
   fun(State0) ->
      case
         m_http_codec:encode(
            lens:get(l_req_header(<<"Content-Type">>)),
            Value
         )
      of
         {ok, Payload} ->
            Length = erlang:byte_size(Payload),
            State1 = lens:put(l_req_payload(), Payload, State0),
            State2 = lens:put(l_req_header(<<"Content-Length">>), Length, State1),
            [Payload | State2];
         {error, Reason} ->
            fail(Reason)
      end
   end.

%%
%% evaluate http request
-spec request() -> m(_).

request() ->
   fun(#{so := SOpts} = State) ->
      case 
         hackney:request(
            lens:get(l_req_method(), State),
            lens:get(l_req_url(), State),
            lens:get(l_req_headers(), State),
            lens:get(l_req_payload(), State),
            SOpts
         )
      of
         {ok, Code, Head, Stream} ->
            stream(Stream, [{Code, typecast:s(Code), Head}], State);
         {error, Reason} ->
            fail(Reason)
      end
   end.

stream(Stream, Acc, State) ->
   case hackney:stream_body(Stream) of
      {ok, Data} ->
         stream(Stream, [Data | Acc], State);
      done ->
         case m_http_codec:decode(lists:reverse(Acc)) of
            {ok, Http} ->
               [Http | State#{ret => Http}];
            {error, Reason} ->
               fail(Reason)
         end;
      {error, Reason} ->
         fail(Reason)
   end.

%%
%%
-spec require(lens:lens()) -> m(_).
-spec require(atom(), lens:lens()) -> m(_).

require(Lens) ->
   fun(State) ->
      case lens:get(lens:c(lens:at(ret, #{}), Lens), State) of
         {ok, Expect} ->
            [Expect | State];
         {error, Reason} ->
            throw(Reason);
         LensFocusedAt ->
            [LensFocusedAt | State]
      end
   end.

require(code, Code) ->
   require( lens:c(lens:hd(), lens:t1(), lens:require(Code)) );

require(header, Lens) ->
   require( lens:c(lens:hd(), lens:t3(), Lens) );

require(content, Lens) ->
   require( lens:c(lens:tl(), lens:hd(), Lens) ).

%%
%%
-spec defined(lens:lens()) -> m(_).

defined(Lens) ->
   fun(State) ->
      case lens:get(lens:c(lens:at(ret, #{}), Lens), State) of
         undefined ->
            throw(undefined);
         LensFocusedAt ->
            [LensFocusedAt | State]
      end
   end.
