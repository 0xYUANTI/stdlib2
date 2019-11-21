%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Lists.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_lists).

%%%_* Exports ==========================================================
-export([ assoc/2
        , assoc/3
        , butlast/1
        , cons/1
        , cons/2
        , dissoc/2
        , drop/2
        , dsort/1
        , intersperse/2
        , is_permutation/2
        , partition/2
        , position/2
        , repeatedly/2
        , take/2
        , to_list/1
        ]).

%%%_* Includes =========================================================
-include("prelude.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Code =============================================================
-spec assoc(alist(A, B), A) -> maybe(B, notfound).
%% @doc assoc(KVs, K) is the value associated with K in KVs.
assoc(L, K) ->
  case lists:keyfind(K, 1, L) of
    {K, V} -> {ok, V};
    false  -> {error, notfound}
  end.

-ifdef(TEST).
assoc2_test() ->
  {ok, bar}         = assoc([{baz, quux}, {foo, bar}], foo),
  {error, notfound} = assoc([],                        foo).
-endif.


-spec assoc(alist(A, B), A, B) -> B.
%% @doc assoc(KVs, K, Def) is the value associated with K in KVs or Def.
assoc(KVs, K, Def) ->
  case assoc(KVs, K) of
    {ok, V}           -> V;
    {error, notfound} -> Def
  end.

-ifdef(TEST).
assoc3_test() ->
  bar = assoc([{foo, bar}], foo, bar),
  bar = assoc([],           foo, bar).
-endif.


-spec butlast([A,...]) -> [A].
%% @doc butlast(Xs) is Xs with its last element removed.
butlast([_])           -> [];
butlast([X|Xs])        -> [X|butlast(Xs)].

-ifdef(TEST).
butlast_test()         -> [foo] = butlast([foo, bar]).
-endif.


-spec cons(_, _) -> maybe_improper_list().
%% @doc cons(Car, Cdr) is Car consed unto Cdr.
cons(Car, Cdr)   -> [Car|Cdr].
cons(Car)        -> fun(Cdr) -> cons(Car, Cdr) end.

-ifdef(TEST).
cons_test()      -> [foo, bar|baz] = (cons(foo))(cons(bar, baz)).
-endif.


-spec dissoc(alist(A, _), A) -> alist(A, _).
%% @doc dissoc(KVs, K) is KVs with all K-entries removed.
dissoc(KVs, K) -> lists:filter(fun({Key, _Val}) -> Key =/= K end, KVs).

-ifdef(TEST).
dissoc_test() ->
  [{bar, 1}, {baz, 3}] = dissoc([{foo, 0}, {bar, 1}, {foo, 2}, {baz, 3}], foo).
-endif.


-spec drop(integer(), [_]) -> [_].
%% @doc drop(N, Xs) is the Nth tail of Xs (empty if Xs has fewer than N
%% elements).
drop(N, Xs) when N =< 0    -> Xs;
drop(_, [])                -> [];
drop(N, [_|Xs])            -> drop(N - 1, Xs).

-ifdef(TEST).
drop_test() ->
  [baz] = drop(2, [foo, bar, baz]),
  []    = drop(2, []).
-endif.


-spec intersperse(_, [_]) -> [_].
%% @doc intersperse(X, Ys) is Ys with X interspersed.
intersperse(X, Ys) ->
  lists:reverse(tl(lists:foldl(fun(Y, Acc) -> [X, Y|Acc] end, [], Ys))).

-ifdef(TEST).
intersperse_test() -> "f o o" = intersperse($ , "foo").
-endif.

-spec dsort(list()) -> list().
%% @doc Sort a list and its list and proplist value elements.
dsort(L)                              -> lists:sort(dsort_i(L)).
dsort_i([])                           -> [];
dsort_i([{K, V} | T]) when is_list(V) -> [{K, lists:sort(dsort_i(V))} | dsort_i(T)];
dsort_i([H | T]) when is_list(H)      -> [lists:sort(dsort_i(H)) | dsort_i(T)];
dsort_i([H | T])                      -> [H | dsort_i(T)].

-ifdef(TEST).
dsort_test() ->
  [] = dsort([]),
  [a, b] = dsort([b, a]),
  [c, [a, b]] = dsort([[b, a], c]),
  [{a, x}, {b, y}] = dsort([{b, y}, {a, x}]),
  [d, {c, [{a, x}, {b, y}]}] = dsort([{c, [{b, y}, {a, x}]}, d]).
-endif.

-spec is_permutation([A], [A]) -> boolean().
%% @doc is_permutation(Xs, Ys) is true iff Xs is a permutation of Ys.
is_permutation(Xs, Ys) -> lists:sort(Xs) =:= lists:sort(Ys).

-ifdef(TEST).
is_permutation_test() ->
  true  = is_permutation([foo, bar, baz], [baz, bar, foo]),
  false = is_permutation([foo, bar, baz], [baz, bar, quux]).
-endif.


-spec partition(pos_integer(), [_]) -> [[_]] | [].
%% @doc partition(N, Xs) is a list of N-partitions of Xs.
partition(N, Xs)
  when is_integer(N)
     , N > 0
     , is_list(Xs) ->
  Len = length(Xs),
  case {Len > 0, Len > N} of
    {true,  true } -> [take(N, Xs)|partition(N, drop(N, Xs))];
    {true,  false} -> [Xs];
    {false, false} -> []
  end.

-ifdef(TEST).
partition_test() ->
  []               = partition(2, []),
  [[1]]            = partition(2, [1]),
  [[1, 2]]         = partition(2, [1, 2]),
  [[1, 2], [3]]    = partition(2, [1, 2, 3]),
  [[1, 2], [3, 4]] = partition(2, [1, 2, 3, 4]).
-endif.

-spec position(_, [_]) -> pos_integer() | notfound.
%% @doc posistion(Elem, Xs) is the position of Elem in Xs or notfound
position(Elem, Xs) when is_list(Xs) -> position(Elem, Xs, 1).

position(_, [], _)          -> notfound;
position(Elem, [Elem|_], N) -> N;
position(Elem, [_|Xs], N)   -> position(Elem, Xs, N+1).

-ifdef(TEST).
position_test() ->
  notfound         = position(x, []),
  1                = position(a, [a,b,c]),
  2                = position(b, [a,b,c]),
  3                = position(c, [a,b,c]),
  notfound         = position(d, [a,b,c]).
-endif.

-spec repeatedly(non_neg_integer(), fun(() -> A)) -> [A].
%% @doc repeatedly(N, F) is a list of the results of N calls to F.
repeatedly(N, F) when is_integer(N) , N > 0 -> [F()|repeatedly(N - 1, F)];
repeatedly(0, F) when is_function(F, 0)     -> [].

-ifdef(TEST).
repeatedly_test() -> [foo, foo] = repeatedly(2, fun() -> foo end).
-endif.


-spec take(integer(), [_]) -> [_].
%% @doc take(N, Xs) is a list containing the first N elements of Xs
%% (Xs if Xs has fewer than N elements).
take(N, _) when N =< 0     -> [];
take(_, [])                -> [];
take(N, [X|Xs])            -> [X|take(N - 1, Xs)].

-ifdef(TEST).
take_test() ->
  [1, 2] = take(2, [1, 2, 3]),
  [1]    = take(2, [1]).
-endif.


-spec to_list(_)              -> [_].
%% @doc to_list(X) is the list-representation of X.
to_list(X) when is_atom(X)    -> ?a2l(X);
to_list(X) when is_binary(X)  -> ?b2l(X);
to_list(X) when is_integer(X) -> ?i2l(X);
to_list(X) when is_float(X)   -> ?f2l(X);
to_list(X) when is_list(X)    -> X;
to_list(X) when is_pid(X)     -> pid_to_list(X);
to_list(X) when is_tuple(X)   -> ?t2l(X).

-ifdef(TEST).
to_list_test() ->
  "atom"     = to_list(atom),
  "bin"      = to_list(<<"bin">>),
  []         = to_list(<<>>),
  "42"       = to_list(42),
  []         = to_list([]),
  "<" ++ _   = to_list(self()),
  [foo, bar] = to_list({foo, bar}).
-endif.

%%%_* Emacs =============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
