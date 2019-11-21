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
-include("prelude.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Code =============================================================
-spec int() -> non_neg_integer().
%% int() is a random 128-bit integer.
int() -> rand:uniform(1 bsl 127) - 1.


-spec numbers(pos_integer(), pos_integer()) -> [pos_integer()].
%% @doc numbers(N, Max) is a list of N random numbers between 0 and
%% Max-1.
numbers(N, Max) -> [rand:uniform(Max) - 1 || _ <- lists:seq(1, N)].

-ifdef(TEST).
numbers_test() ->
  Max       = 1 bsl 127,
  []        = numbers(0, Max),
  [_, _, _] = numbers(3, Max),
  [X]       = numbers(1, 1),
  true      = X =:= 0 orelse X =:= 1,
  Xs        = numbers(42, 41),
  true      = length(Xs) > length(lists:usort(Xs)).
-endif.


-spec pick([A]) -> A.
%% @doc pick(Xs) is a random element of Xs.
pick(Xs) -> lists:nth(rand:uniform(length(Xs)), Xs).

-ifdef(TEST).
pick_test() ->
  foo  = pick([foo]),
  X    = pick([foo, bar]),
  true = X =:= foo orelse X =:= bar.
-endif.


-spec shuffle([_]) -> [_].
%% @doc shuffle(Xs) is a random permutation of Xs.
shuffle(Xs) ->
  [X || {_N, X} <- lists:keysort(1, [{rand:uniform(length(Xs)) - 1, X} ||
                                      X <- Xs])].

-ifdef(TEST).
shuffle_test() ->
  Xs = lists:seq(0, 42),
  ?assert(shuffle(Xs) =/= shuffle(Xs)).
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
