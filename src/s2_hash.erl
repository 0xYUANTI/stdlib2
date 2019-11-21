%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Hashing functions
%%%
%%% Copyright 2014-2018 Kivra AB
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
-module(s2_hash).

%%%_* Exports ==========================================================
-export([ luhn/1
        , luhn_validate/1
        , mod97_validate/1
        , md5/1
        , sha256/1
        ]).

%%%_* Includes =========================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Code =============================================================
-spec luhn(string()) -> integer().
luhn(String) when is_binary(String) ->
  luhn(binary_to_list(String));
luhn(String) when is_list(String) ->
  IntList  = lists:map(fun(ASCII) -> ASCII - 48 end, String),
  {_, Sum} = lists:foldr(fun luhn_fold/2, {odd, 0}, IntList),
  (Sum*9) rem 10.

-spec luhn_validate(string()) -> boolean().
luhn_validate(String) when is_binary(String) ->
  luhn_validate(binary_to_list(String));
luhn_validate(String) when is_list(String) ->
  IntList  = lists:map(fun(ASCII) -> ASCII - 48 end, String),
  {_, Sum} = lists:foldr(fun luhn_fold/2, {even, 0}, IntList),
  Sum rem 10 =:= 0.

luhn_fold(X, {odd, Sum}) ->
  Y = X * 2,
  N = case Y > 9 of
        true  -> Y - 9;
        false -> Y
      end,
  {even, Sum + N};
luhn_fold(X, {even, Sum}) ->
  {odd, Sum + X}.

%% @doc 1. Move the four initial characters to the end of the string
%%      2. Replace each letter in the string with two digits, thereby
%%         expanding the string, where A = 10, B = 11, ..., Z = 35
%%      3. Interpret the string as a decimal integer and compute the remainder
%%         of that number on division by 97
mod97_validate(<<Four:4/binary, Rest/binary>>) ->
  Int = list_to_integer(
          lists:concat(
            lists:reverse(
              lists:foldl(fun(I, Acc) when I >= 48, I =< 57  -> [I-48 | Acc];
                             (C, Acc) when C >= 97, C =< 122 -> [C-87 | Acc]
                          end,
                          [],
                          binary_to_list(
                            string:lowercase(<<Rest/binary, Four/binary>>))))) ),
  1 =:= (Int rem 97).

-spec md5(iodata()) -> binary().
md5(Data) ->
  hash_to_bin(crypto:hash(md5, Data)).

-spec sha256(iodata()) -> binary().
sha256(Data) ->
  hash_to_bin(crypto:hash(sha256, Data)).

%%%_* Private functions ================================================
hash_to_bin(Hash) ->
  list_to_binary(
    [ element(C+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f})
      || <<C:4>> <= Hash ] ).

%%%_* Tests ============================================================
-ifdef(TEST).

luhn_test_() ->
  [ ?_assertEqual(5, luhn("7992739870"))
  , ?_assertEqual(3, luhn("7992739871"))
  ].

luhn_validate_test_() ->
  [ ?_assertEqual(false, luhn_validate("79927398711"))
  , ?_assertEqual(false, luhn_validate("79927398712"))
  , ?_assertEqual(true,  luhn_validate("79927398713"))
  , ?_assertEqual(false, luhn_validate("79927398714"))
  , ?_assertEqual(false, luhn_validate("79927398715"))
  , ?_assertEqual(false, luhn_validate("79927398716"))
  , ?_assertEqual(false, luhn_validate("79927398717"))
  , ?_assertEqual(false, luhn_validate("79927398718"))
  , ?_assertEqual(false, luhn_validate("79927398719"))
  , ?_assertEqual(false, luhn_validate("79927398710"))
  ].

mod97_validate_test_() ->
  [ ?_assertEqual(false, mod97_validate(<<"123456">>))
  , ?_assertEqual(true, mod97_validate(<<"RF485000056789012345">>))
  , ?_assertEqual(true, mod97_validate(<<"FI2112345600000785">>))
  ].

dogfood_test() ->
    Str = "1234567890",
    Lund = Str ++ integer_to_list(luhn(Str)),
    ?assertEqual(true, luhn_validate(Lund)).

md5_test_() ->
  [ ?_assertEqual(
       <<"d41d8cd98f00b204e9800998ecf8427e">>,
       md5(""))
  , ?_assertEqual(
       <<"900150983cd24fb0d6963f7d28e17f72">>,
       md5("abc"))
  , ?_assertEqual(
       <<"900150983cd24fb0d6963f7d28e17f72">>,
       md5(<<"abc">>))
  ].

sha256_test_() ->
  [ ?_assertEqual(
       <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>,
       sha256(""))
  , ?_assertEqual(
       <<"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad">>,
       sha256("abc"))
  , ?_assertEqual(
       <<"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad">>,
       sha256(<<"abc">>))
  ].

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
