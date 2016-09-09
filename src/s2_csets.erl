%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Counting sets.
%%% @reference
%%% http://research.microsoft.com/en-us/people/aguilera/walter-sosp2011.pdf
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_csets).
-behaviour(s2_gen_lattice).

%%%_* Exports ==========================================================
%% gen_lattice callbacks
-export([ new/0
        , compare/2
        , merge/2
        , update/2
        , value/1
        ]).

%% API
-export([ add_element/2
        , cnt_element/2
        , del_element/2
        , is_cset/1
        , is_element/2
        , is_mode/1
        , is_subset/2
        , mode/1
        , toggle/1
        , union/2
        ]).

%% Types
-export_type([ cset/0
             , mode/0
             ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-ifdef(no_namespaced_types).
-opaque cset()                  :: {mode(), dict()}.
-else.
-opaque cset()                  :: {mode(), dict:dict()}.
-endif.
-type mode()                    :: cset | set.

%%%_ * gen_lattice callbacks -------------------------------------------
-spec new()                     -> cset().
new()                           -> {cset, dict:new()}.

-spec compare(cset(), cset())   -> s2_gen_lattice:relation().
compare(S1, S2)                 -> case {is_subset(S1,S2), is_subset(S2,S1)} of
                                     {true, false}  -> less_than;
                                     {true, true}   -> equal;
                                     {false, true}  -> greater_than;
                                     {false, false} -> concurrent
                                   end.

-spec merge(cset(), cset())     -> cset().
merge(S1, S2)                   -> union(S1, S2).

-spec update(cset(), _)         -> cset().
update(S, {add, X})             -> add_element(S, X);
update(S, {del, X})             -> del_element(S, X).

-spec value(cset())             -> [_].
value({_, S})                   -> [Elt || {Elt, Cnt} <- dict:to_list(S),
                                           Cnt > 0].

%%%_ * API -------------------------------------------------------------
-spec add_element(cset(), _)    -> cset().
add_element({M, Xs}, X)         -> {M, add_element(M, X, Xs)}.
add_element(cset, X, Xs)        -> dict:update_counter(X, 1, Xs);
add_element(set,  X, Xs)        -> dict:store(X, 1, Xs).

-spec del_element(cset(), _)    -> cset().
del_element({M, Xs}, X)         -> {M, del_element(M, X, Xs)}.
del_element(cset, X, Xs)        -> dict:update_counter(X, -1, Xs);
del_element(set,  X, Xs)        -> dict:erase(X, Xs).

-spec cnt_element(cset(), _)    -> integer().
cnt_element({cset, Xs}, X)      -> try dict:fetch(X, Xs)
                                   catch _:_ -> 0
                                   end;
cnt_element({set, Xs}, X)       -> int_to_bool(cnt_element({cset, Xs}, X)).

-spec is_cset(_)                -> boolean().
is_cset({M, _})                 -> is_mode(M);
is_cset(_)                      -> false.

-spec is_element(cset(), _)     -> boolean().
is_element(S, X)                -> cnt_element(S, X) > 0.

-spec is_mode(_)                -> boolean().
is_mode(cset)                   -> true;
is_mode(set)                    -> true;
is_mode(_)                      -> false.

-spec is_subset(cset(), cset()) -> boolean().
is_subset({M, Xs1}, {M, Xs2})   -> catch dict:fold(
                                           fun(Elt, Cnt, true) ->
                                             (cnt_element({M, Xs2}, Elt)
                                              >=
                                              if M =:= cset -> Cnt;
                                                 M =:= set  -> int_to_bool(Cnt)
                                              end)
                                                 orelse throw(false)
                                           end, true, Xs1);
is_subset(_, _)                 -> erlang:error(mode).

-spec mode(cset())              -> mode().
mode({M, _Xs})                  -> M.

-spec toggle(cset())            -> cset().
toggle({cset, Xs})              -> {set, Xs};
toggle({set, Xs})               -> {cset, Xs}.

-spec union(cset(), cset())     -> cset().
union({M, Xs1}, {M, Xs2})       -> {M, union(M, Xs1, Xs2)};
union(_, _)                     -> erlang:error(mode).
union(cset, Xs1, Xs2)           -> dict:merge(fun sum/3,  Xs1, Xs2);
union(set,  Xs1, Xs2)           -> dict:merge(fun bsum/3, Xs1, Xs2).

%%%_ * Helpers ---------------------------------------------------------
sum(_, N1, N2)                  -> N1 + N2.
bsum(_, N1, N2)                 -> int_to_bool(N1 + N2).
int_to_bool(N) when N =< 0      -> 0;
int_to_bool(N) when N  > 0      -> 1.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lattice_test() ->
  S1              = new(),
  S2              = new(),
  S11             = update(S1, {add, foo}),
  S21             = update(S2, {add, foo}),
  equal           = compare(S11, S21),
  equal           = compare(S21, S11),
  [foo]           = value(S11),

  S12             = update(S11, {del, foo}),
  S22             = update(S21, {add, foo}),
  equal           = compare(S1,  S12), %!
  equal           = compare(S2,  S12), %!
  greater_than    = compare(S11, S12),
  greater_than    = compare(S21, S12),
  greater_than    = compare(S22, S12),
  less_than       = compare(S1,  S22),
  less_than       = compare(S2,  S22),
  less_than       = compare(S11, S22),
  less_than       = compare(S21, S22),
  less_than       = compare(S12, S22),

  S13             = update(S12, {add, bar}),
  S23             = update(S22, {add, baz}),
  less_than       = compare(S12, S13),
  concurrent      = compare(S22, S13),
  concurrent      = compare(S23, S13),
  less_than       = compare(S12, S23),
  less_than       = compare(S22, S23),
  concurrent      = compare(S13, S23),

  S14             = update(S13, {del, foo}),
  S24             = update(S23, {add, baz}),
  greater_than    = compare(S13, S14),
  concurrent      = compare(S23, S14),
  concurrent      = compare(S24, S14),
  concurrent      = compare(S13, S24),
  less_than       = compare(S23, S24),
  concurrent      = compare(S14, S24),

  S               = merge(S14, S24),
  S_              = merge(S24, S14),

 -1               = cnt_element(S14, foo),
  1               = cnt_element(S14, bar),
  0               = cnt_element(S14, baz),

  2               = cnt_element(S24, foo),
  0               = cnt_element(S24, bar),
  2               = cnt_element(S24, baz),

  1               = cnt_element(S, foo),
  1               = cnt_element(S, bar),
  2               = cnt_element(S, baz),

  equal           = compare(S, S_),
  less_than       = compare(S14, S),
  concurrent      = compare(S24, S), %!

  [bar, baz, foo] = lists:sort(value(S)),
  [baz, foo]      = lists:sort(value(S24)),

  S14_set         = toggle(S14),
  S24_set         = toggle(S24),
  S_set           = merge(S14_set, S24_set),

  0               = cnt_element(S14_set, foo),
  1               = cnt_element(S14_set, bar),
  0               = cnt_element(S14_set, baz),

  1               = cnt_element(S24_set, foo),
  0               = cnt_element(S24_set, bar),
  1               = cnt_element(S24_set, baz),

  1               = cnt_element(S_set, foo),
  1               = cnt_element(S_set, bar),
  1               = cnt_element(S_set, baz),

  less_than       = compare(S14_set, S_set),
  less_than       = compare(S24_set, S_set).

api_test() ->
  S     = new(),
  true  = mode(S) =:= mode(toggle(toggle(S))),
  true  = is_cset(S),
  true  = is_mode(mode(S)),
  false = is_cset([]),
  false = is_element(S, foo),
  S_    = toggle(S),
  true  = is_mode(mode(S_)),
  false = is_mode(foo),
  1     = cnt_element(add_element(add_element(S_, foo), foo), foo),
  0     = cnt_element(del_element(del_element(S_, foo), foo), foo),
  catch is_subset(S, S_),
  catch union(S, S_).

-endif.

%%%_* Emacs =============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
