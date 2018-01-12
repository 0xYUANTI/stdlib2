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
        , sha256/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

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

-spec sha256(iodata()) -> binary().
sha256(Data) ->
  list_to_binary(
    [ element(C+1, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f})
      || <<C:4>> <= crypto:hash(sha256, Data) ] ).

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

dogfood_test() ->
    Str = "1234567890",
    Lund = Str ++ integer_to_list(luhn(Str)),
    ?assertEqual(true, luhn_validate(Lund)).

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
