%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Randomness.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_rand).

%%%_* Exports ==========================================================
-export([ int/0
        , numbers/2
        , pick/1
        , shuffle/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec int() -> non_neg_integer().
%% int() is a random 128-bit integer.
int() -> crypto:rand_uniform(0, 1 bsl 127).


-spec numbers(pos_integer(), pos_integer()) -> [pos_integer()].
%% @doc numbers(N, Max) is a list of N random numbers between 0 and
%% Max-1.
numbers(N, Max) -> [crypto:rand_uniform(0, Max) || _ <- lists:seq(1, N)].

numbers_test() ->
  Max       = 1 bsl 127,
  []        = numbers(0, Max),
  [_, _, _] = numbers(3, Max),
  [X]       = numbers(1, 1),
  true      = X =:= 0 orelse X =:= 1,
  Xs        = numbers(42, 41),
  true      = length(Xs) > length(lists:usort(Xs)).


-spec pick([A]) -> A.
%% @doc pick(Xs) is a random element of Xs.
pick(Xs) -> lists:nth(crypto:rand_uniform(1, length(Xs) + 1), Xs).

pick_test() ->
  foo  = pick([foo]),
  X    = pick([foo, bar]),
  true = X =:= foo orelse X =:= bar.


-spec shuffle([_]) -> [_].
%% @doc shuffle(Xs) is a random permutation of Xs.
shuffle(Xs) ->
  [X || {_N, X} <- lists:keysort(1, [{crypto:rand_uniform(0, length(Xs)), X} ||
                                      X <- Xs])].

shuffle_test() ->
  Xs = lists:seq(0, 42),
  ?assert(shuffle(Xs) =/= shuffle(Xs)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
