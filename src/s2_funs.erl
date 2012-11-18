%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Functions.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_funs).

%%%_* Exports ==========================================================
-export([ fix/2
        , fix/3
        , flip/1
        , o/1
        , o/2
        , o/3
        , reduce/1
        , unwind_with/3
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec fix(fun(), _) -> _ | no_return().
%% @doc fix(F, X) is the fixpoint of F.
fix(F, X) ->
  fix(F, X, fun(X1, X2) -> X1 =:= X2 end).
fix(F, X, Eq) ->
  fix(X, F(X), F, Eq).
fix(X1, X2, F, Eq) ->
  case Eq(X1, X2) of
    true  -> X2;
    false -> fix(X2, F(X2), F, Eq)
  end.

fix_test() ->
  2 = fix(fun(X) when X rem 2 =:= 0 -> X;
             (X) when X rem 2 =:= 1 -> X+1
          end, 1).


-spec flip(fun((A, B) -> C)) -> fun((B, A) -> C).
%% @doc flip(F) is F with reversed argument order.
flip(F) -> fun(X, Y) -> F(Y, X) end.

flip_test() ->
  [0|1] = s2_lists:cons(0, 1),
  [1|0] = (flip(fun s2_lists:cons/2))(0, 1).


-spec o([fun()]) -> fun().
%% @doc (o([F, G]))(X) is F(G(X)).
o(Fs)            -> fun(X) -> do_o(lists:reverse(Fs), X) end.
o(F, G)          -> o([F, G]).
o(F, G, H)       -> o([F, G, H]).

do_o([],     X)  -> X;
do_o([F|Fs], X)  -> do_o(Fs, F(X)).

o_test() ->
  foo = (o([]))(foo),
  F   = fun(X) -> X+1 end,
  G   = fun(X) -> X*2 end,
  H   = fun(X) -> X-1 end,
  83  = (o(F, G, H))(42),
  83  = (o(F, o(G, H)))(42).


-spec reduce(fun(() -> A) | A)   -> A.
%% @doc reduce(F) is the value of F.
reduce(X) when is_function(X, 0) -> reduce(X());
reduce(X)                        -> X.

reduce_test() -> val = reduce(fun() -> fun() -> val end end).


-spec unwind_with(fun((A, fun((B) -> C)) -> C),
                  [A],
                  fun(([B]) -> C)) -> C.
%% @doc See s2_fs.erl for use cases.
unwind_with(F, Xs, G) ->
  unwind_with(Xs, F, G, []).
unwind_with([X|Xs], F, G, Ys) ->
  F(X, fun(Y) -> unwind_with(Xs, F, G, [Y|Ys]) end);
unwind_with([], _F, G, Ys) ->
  G(lists:reverse(Ys)).

unwind_with_test() ->
  F         = fun(X, G) ->
                Y = X + 1,
                try G(Y)
                after io:format(user, "Y = ~p~n", [Y])
                end
              end,
  G         = fun(Ys) -> [Y rem 2 || Y <- Ys] end,
  [0, 1, 0] = unwind_with(F, [1, 2, 3], G).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
