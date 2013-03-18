%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Base 16 ASCII armor.
%%%
%%% Copyright 2011-2013 Klarna AB
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_hex).

%%%_* Exports ==========================================================
-export([ encode/1
        , decode/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
-spec encode(_)                    -> string().
%% @doc Encode an arbitrary Erlang term as a lowercase ASCII string.
encode(X)                          -> do_encode(term_to_binary(X)).

-spec decode(string())             -> _.
%% @doc Decode the output of `encode/1'.
decode(Y)                          -> binary_to_term(do_decode(Y)).


-spec do_encode(binary())          -> string().
do_encode(<<N:4, Rest/bitstring>>) -> [enc(N)|do_encode(Rest)];
do_encode(<<>>)                    -> "". %it's actually a binary

-spec do_decode(string())          -> binary().
do_decode(Str)                     -> lists:foldl(
                                        fun(C, Acc) ->
                                          N = dec(C),
                                          <<Acc/bitstring, N:4>>
                                        end, <<>>, Str).


enc(N)                             -> char(tab(N)).
dec(C)                             -> tab(str(C)).

char([C])                          -> C.
str(C)                             -> [C].

tab(0)                             -> "0";
tab(1)                             -> "1";
tab(2)                             -> "2";
tab(3)                             -> "3";
tab(4)                             -> "4";
tab(5)                             -> "5";
tab(6)                             -> "6";
tab(7)                             -> "7";
tab(8)                             -> "8";
tab(9)                             -> "9";
tab(10)                            -> "a";
tab(11)                            -> "b";
tab(12)                            -> "c";
tab(13)                            -> "d";
tab(14)                            -> "e";
tab(15)                            -> "f";
tab("0")                           -> 0;
tab("1")                           -> 1;
tab("2")                           -> 2;
tab("3")                           -> 3;
tab("4")                           -> 4;
tab("5")                           -> 5;
tab("6")                           -> 6;
tab("7")                           -> 7;
tab("8")                           -> 8;
tab("9")                           -> 9;
tab("a")                           -> 10;
tab("b")                           -> 11;
tab("c")                           -> 12;
tab("d")                           -> 13;
tab("e")                           -> 14;
tab("f")                           -> 15.

%%%_* Tests ============================================================
-ifdef(TEST).

encode_decode_test() ->
  X = { 'foo bar'
      , "baz"
      , <<"quux">>
      , 42
      , [6.66]
      , make_ref()
      , self()
      , fun(X) -> X end
      },
  X = decode(encode(X)),
  0 = (element(8, X))(0),
  ok.

do_encode_decode_test() ->
  X          = <<13:4,14:4,10:4,13:4,11:4,14:4,14:4,15:4>>,
  Y          = do_encode(X),
  "deadbeef" = Y,
  X          = do_decode(Y),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
