%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
%%   Copyright (c) 2016, Mario Cardona
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
-module(m_http_codec_url).

-export([escape/1, unescape/1]).

%%
%% escape
escape(X) when is_binary(X) ->
   escape(X, <<>>);
escape(X) ->
   escape(typecast:s(X)).


escape(<<H:8, Bin/binary>>, Acc) when H >= $a, H =< $z ->
   escape(Bin, <<Acc/binary, H>>);
escape(<<H:8, Bin/binary>>, Acc) when H >= $A, H =< $Z ->
   escape(Bin, <<Acc/binary, H>>);
escape(<<H:8, Bin/binary>>, Acc) when H >= $0, H =< $9 ->
   escape(Bin, <<Acc/binary, H>>);

escape(<<$ , Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $%, $2, $0>>);

%% unreserved "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
escape(<<$-, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $->>);
escape(<<$_, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $_>>);
escape(<<$., Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $.>>);
escape(<<$!, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $!>>);
escape(<<$~, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $~>>);
escape(<<$*, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $*>>);
escape(<<$', Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $'>>);
escape(<<$(, Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $(>>);
escape(<<$), Bin/binary>>, Acc) ->
   escape(Bin, <<Acc/binary, $)>>);

escape(<<H:8, Bin/binary>>, Acc) when H =< 16#7f ->
   escape(Bin, <<Acc/binary, $%, (escape_byte(H))/binary>>);

escape(<<H:8, Bin/binary>>, Acc) when H  > 16#7f ->
   escape(Bin, 
      <<Acc/binary,
      $%, (escape_byte((H bsr      6) + 16#c0))/binary, 
      $%, (escape_byte((H band 16#3f) + 16#80))/binary>>
   );

escape(<<>>, Acc) ->
   Acc.

escape_byte(H) ->
   <<(hex(H div 16)), (hex(H rem 16))>>.

hex(H) when H <  10 ->
   $0 + H;
hex(H) when H >= 10 ->
   $A + (H - 10).

%%
%% unescape
unescape(Bin) ->
   decode(unescape(Bin, <<>>), <<>>).

unescape(<<$%, H:8, L:8, Bin/binary>>, Acc) ->
   unescape(Bin, <<Acc/binary, (unescape_byte(H, L))>>);
unescape(<<H:8, Bin/binary>>, Acc) ->
   unescape(Bin, <<Acc/binary, H>>);
unescape(<<>>, Acc) ->
   Acc.

unescape_byte(H, L) ->
   int(H) * 16 + int(L).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

%%
%% decode utf8 (TODO: utf8 for binaries)
decode(<<H:8, L:8, Rest/binary>>, Acc) when H >= 16#C0 ->
   decode(Rest, <<Acc/binary, ((H bsl 6) + (L band 16#3f))>>);
decode(<<H:8, Rest/binary>>, Acc) ->
   decode(Rest, <<Acc/binary, H>>);
decode(<<>>, Acc) ->
   Acc.
