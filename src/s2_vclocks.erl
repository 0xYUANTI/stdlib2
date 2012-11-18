%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Vector clocks.
%%% @reference citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.142.3682
%%% @reference ieeexplore.ieee.org/xpl/freeabs_all.jsp?arnumber=1703051
%%% @reference citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.47.7435
%%% @reference github.com/basho/riak_core/blob/master/src/vclock.erl
%%% @reference github.com/cliffmoon/dynomite/blob/master/elibs/vector_clock.erl
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_vclocks).
-behaviour(s2_gen_lattice).
-compile({no_auto_import, [size/1]}).

%%%_* Exports ==========================================================
%% gen_lattice callbacks
-export([ new/0
        , compare/2
        , merge/2
        , update/2
        , value/1
        ]).

%% API
-export([ increment/2
        , is_vclock/1
        , pp/1
        , prune/2
        , size/1
        ]).

%% Types
-export_type([ vclock/0
             , index/0
             ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-opaque vclock() :: alist(index(), non_neg_integer()).
-type   index()  :: _.

%%%_ * gen_lattice callbacks -------------------------------------------
-spec new() -> vclock().
%% @doc new() is a fresh vclock.
new() -> [].


-spec compare(vclock(), vclock()) -> s2_gen_lattice:relation().
%% @doc compare(C1, C2) is C1's relation to C2.
compare(C1, C2) -> compare(C1, C2, equal).

compare([_|_], [],    R)  -> rel(R, greater_than);
compare([],    [_|_], R)  -> rel(R, less_than);
compare([],    [],    R)  -> R;
compare([H|T], C0,    R0) -> {C, R} = do_compare(H, C0, R0),
                             compare(T, C, R).

do_compare({Idx, N1}, Clock, Rel) ->
  case lists:keytake(Idx, 1, Clock) of
    {value, {Idx, N2}, Rest} -> {Rest,  rel(Rel, cmp(N1, N2))};
    false                    -> {Clock, rel(Rel, greater_than)}
  end.

cmp(N1, N2) when N1  <  N2 -> less_than;
cmp(N1, N2) when N1 =:= N2 -> equal;
cmp(N1, N2) when N1  >  N2 -> greater_than.

rel(equal, Rel)   -> Rel;
rel(Rel,   equal) -> Rel;
rel(Rel,   Rel)   -> Rel;
rel(_,     _)     -> concurrent.


-spec merge(vclock(), vclock()) -> vclock().
%% @doc merge(C1, C2) is the LUB of C1 and C2.
merge(C1, []) ->
  C1;
merge([], C2) ->
  C2;
merge([{Idx, N1}|C1], C2) ->
  case lists:keytake(Idx, 1, C2) of
    {value, {Idx, N2}, Rest} -> [{Idx, erlang:max(N1, N2)}|merge(C1, Rest)];
    false                    -> [{Idx, N1}|merge(C1, C2)]
  end.


-spec update(vclock(), index()) -> vclock().
%% @doc update(C, Idx) is C with Idx's entry incremented by
%% one.
update([{Idx, N}|T], Idx) -> [{Idx, N + 1}|T];
update([H|T],        Idx) -> [H|update(T, Idx)];
update([],           Idx) -> [{Idx, 1}].


-spec value(vclock()) -> no_return().
%% @doc value(C) is the sum of the entries in C.
value(C) -> lists:sum([N || {_Idx, N} <- C]).

%%%_ * API -------------------------------------------------------------
-spec increment(vclock(), index()) -> vclock().
%% @equiv update/2
increment(C, Idx) -> update(C, Idx).

-spec is_vclock(_) -> boolean().
%% @doc is_vclock(X) is true iff X is a vclock.
is_vclock(X) -> is_list(X).

-spec pp(vclock()) -> string().
%% @doc pp(C) is the string-representation of C.
pp(C)         -> "[" ++ pp1(C) ++ "]".
pp1({Idx, N}) -> s2_lists:to_list(Idx) ++ ":" ++ s2_lists:to_list(N);
pp1([])       -> "";
pp1([X])      -> pp1(X);
pp1([X|Xs])   -> pp1(X) ++ "," ++ pp1(Xs).

-spec prune(vclock(), fun((index()) -> boolean())) -> vclock().
%% @doc prune(C, Pred) is C with all entries for which Pred returns true
%% removed.
prune(C, Pred) -> [X || {Idx, _} = X <- C, not Pred(Idx)].

-spec size(vclock()) -> non_neg_integer().
%% @doc size(C) is the number of entries in C.
size(C) -> length(C).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%_ * Reference -------------------------------------------------------
mattern_test() ->
  P11          = incr(new(4), [0, 1]),
  P12          = incr(new(4), [0, 0, 1]),
  "[1,1,0,0]"  = print(P11),
  "[2,1,0,0]"  = print(P12),

  P21          = incr(new(4), [1]),
  P22          = incr(new(4), [1, 1]),
  P23          = incr(new(4), [0, 0, 1, 1, 1, 2, 2, 2, 3]),
  "[0,1,0,0]"  = print(P21),
  "[0,2,0,0]"  = print(P22),
  "[2,3,3,1]"  = print(P23),

  P31          = incr(new(4), [2, 3]),
  P32          = incr(new(4), [0, 0, 1, 2, 2, 3]),
  P33          = incr(new(4), [0, 0, 1, 2, 2, 2, 3]),
  "[0,0,1,1]"  = print(P31),
  "[2,1,2,1]"  = print(P32),
  "[2,1,3,1]"  = print(P33),

  P41          = incr(new(4), [3]),
  P42          = incr(new(4), [3, 3]),
  "[0,0,0,1]"  = print(P41),
  "[0,0,0,2]"  = print(P42),

  equal        = compare(P11, P11),
  less_than    = compare(P11, P12),
  greater_than = compare(P11, P21),
  concurrent   = compare(P11, P22),
  less_than    = compare(P11, P23),
  concurrent   = compare(P11, P31),
  less_than    = compare(P11, P32),
  less_than    = compare(P11, P33),
  concurrent   = compare(P11, P41),
  concurrent   = compare(P11, P42),

  greater_than = compare(P12, P11),
  equal        = compare(P12, P12),
  greater_than = compare(P12, P21),
  concurrent   = compare(P12, P22),
  less_than    = compare(P12, P23),
  concurrent   = compare(P12, P31),
  less_than    = compare(P12, P32),
  less_than    = compare(P12, P33),
  concurrent   = compare(P12, P41),
  concurrent   = compare(P12, P42),

  less_than    = compare(P21, P11),
  less_than    = compare(P21, P12),
  equal        = compare(P21, P21),
  less_than    = compare(P21, P22),
  less_than    = compare(P21, P23),
  concurrent   = compare(P21, P31),
  less_than    = compare(P21, P32),
  less_than    = compare(P21, P33),
  concurrent   = compare(P21, P41),
  concurrent   = compare(P21, P42),

  concurrent   = compare(P22, P11),
  concurrent   = compare(P22, P12),
  greater_than = compare(P22, P21),
  equal        = compare(P22, P22),
  less_than    = compare(P22, P23),
  concurrent   = compare(P22, P31),
  concurrent   = compare(P22, P32),
  concurrent   = compare(P22, P33),
  concurrent   = compare(P22, P41),
  concurrent   = compare(P22, P42),

  greater_than = compare(P23, P11),
  greater_than = compare(P23, P12),
  greater_than = compare(P23, P21),
  greater_than = compare(P23, P22),
  equal        = compare(P23, P23),
  greater_than = compare(P23, P31),
  greater_than = compare(P23, P32),
  greater_than = compare(P23, P33),
  greater_than = compare(P23, P41),
  concurrent   = compare(P23, P42),

  concurrent   = compare(P31, P11),
  concurrent   = compare(P31, P12),
  concurrent   = compare(P31, P21),
  concurrent   = compare(P31, P22),
  less_than    = compare(P31, P23),
  equal        = compare(P31, P31),
  less_than    = compare(P31, P32),
  less_than    = compare(P31, P33),
  greater_than = compare(P31, P41),
  concurrent   = compare(P31, P42),

  greater_than = compare(P32, P11),
  greater_than = compare(P32, P12),
  greater_than = compare(P32, P21),
  concurrent   = compare(P32, P22),
  less_than    = compare(P32, P23),
  greater_than = compare(P32, P31),
  equal        = compare(P32, P32),
  less_than    = compare(P32, P33),
  greater_than = compare(P32, P41),
  concurrent   = compare(P32, P42),

  greater_than = compare(P33, P11),
  greater_than = compare(P33, P12),
  greater_than = compare(P33, P21),
  concurrent   = compare(P33, P22),
  less_than    = compare(P33, P23),
  greater_than = compare(P33, P31),
  greater_than = compare(P33, P32),
  equal        = compare(P33, P33),
  greater_than = compare(P33, P41),
  concurrent   = compare(P33, P42),

  concurrent   = compare(P41, P11),
  concurrent   = compare(P41, P12),
  concurrent   = compare(P41, P21),
  concurrent   = compare(P41, P22),
  less_than    = compare(P41, P23),
  less_than    = compare(P41, P31),
  less_than    = compare(P41, P32),
  less_than    = compare(P41, P33),
  equal        = compare(P41, P41),
  less_than    = compare(P41, P42),

  concurrent   = compare(P42, P11),
  concurrent   = compare(P42, P12),
  concurrent   = compare(P42, P21),
  concurrent   = compare(P42, P22),
  concurrent   = compare(P42, P23),
  concurrent   = compare(P42, P31),
  concurrent   = compare(P42, P32),
  concurrent   = compare(P42, P33),
  greater_than = compare(P42, P41),
  equal        = compare(P42, P42).

new(N)        -> [{Idx, 0} || Idx <- lists:seq(0, N - 1)].
incr(C, Idxs) -> lists:foldl(fun(Idx, C) -> increment(C, Idx) end, C, Idxs).
print(C)      -> lists:flatten(io_lib:format("~p", [[N || {_, N} <- C]])).


parker_test() ->
  %% p. 243
  C1                  = incr(new(), [a,b,b,c,c,c,c,d,d,d]),
  C2                  = incr(new(), [b,b,c,c,d,d,d]),
  C3                  = incr(new(), [a,b,b,c,c,c,d,d,d,d]),
  C4                  = incr(new(), [a,b,b,c,c,c,c,d,d,d,d]),
  "[a:1,b:2,c:4,d:3]" = pp(C1),
  "[b:2,c:2,d:3]"     = pp(C2),
  "[a:1,b:2,c:3,d:4]" = pp(C3),
  "[a:1,b:2,c:4,d:4]" = pp(C4),
  greater_than        = compare(C1, C2),
  concurrent          = compare(C1, C3),
  greater_than        = compare(C4, C1),
  greater_than        = compare(C4, C3),
  %% p. 244
  C5                  = new(),
  C6                  = incr(new(), [a,a]),
  C7                  = incr(new(), [a,a,c]),
  C8                  = incr(new(), [a,a,a]),
  "[]"                = pp(C5),
  "[a:2]"             = pp(C6),
  "[a:2,c:1]"         = pp(C7),
  "[a:3]"             = pp(C8),
  less_than           = compare(C5, C6),
  less_than           = compare(C6, C7),
  less_than           = compare(C5, C7),
  less_than           = compare(C6, C8),
  concurrent          = compare(C7, C8).

%%%_ * Original --------------------------------------------------------
merge_test() ->
  C1           = increment(increment(new(), 0), 1),
  C2           = increment(increment(new(), 0), 1),

  equal        = compare(C1, C2),
  equal        = compare(C2, C1),

  C11          = increment(C1, 0),
  C21          = increment(C2, 1),

  less_than    = compare(C1,  C11),
  less_than    = compare(C2,  C11),
  less_than    = compare(C1,  C21),
  less_than    = compare(C2,  C21),

  concurrent   = compare(C11, C21),
  concurrent   = compare(C21, C11),

  greater_than = compare(C11, C1),
  greater_than = compare(C11, C2),
  greater_than = compare(C21, C1),
  greater_than = compare(C21, C2),

  CM1          = merge(C11, C21),
  CM1          = merge(C21, C11),

  equal        = compare(CM1, CM1),

  less_than    = compare(C1,  CM1),
  less_than    = compare(C2,  CM1),
  less_than    = compare(C11, CM1),
  less_than    = compare(C21, CM1),

  greater_than = compare(CM1, C1),
  greater_than = compare(CM1, C2),
  greater_than = compare(CM1, C11),
  greater_than = compare(CM1, C21),

  CM2          = increment(CM1, 2),

  equal        = compare(CM2, CM2),

  less_than    = compare(C1,  CM2),
  less_than    = compare(C2,  CM2),
  less_than    = compare(C11, CM2),
  less_than    = compare(C21, CM2),
  less_than    = compare(CM1, CM2),

  greater_than = compare(CM2, C1),
  greater_than = compare(CM2, C2),
  greater_than = compare(CM2, C11),
  greater_than = compare(CM2, C21),
  greater_than = compare(CM2, CM1),

  CM3          = increment(CM1, 3),

  concurrent   = compare(CM2, CM3),
  concurrent   = compare(CM3, CM2),

  C            = merge(CM2, CM3),
  C_           = merge(CM3, CM2),
  equal        = compare(C, C_).

prune_test() ->
  C1  = new(42),
  42  = ?MODULE:size(C1),
  C11 = prune(C1, fun(Idx) -> Idx rem 2 =:= 0 end),
  21  = ?MODULE:size(C11).

value_test() ->
  3 = value(increment(increment(increment(new(), foo), bar), baz)).

cover_test() ->
  false = is_vclock(foo).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
