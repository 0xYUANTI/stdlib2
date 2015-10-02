%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Math.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_math).

%%%_* Exports ==========================================================
-export([ceiling/1]).
-export([floor/1]).

%%%_* API ==============================================================
-spec ceiling(number()) -> integer().
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true  -> T;
        false -> T + 1
    end.

-spec floor(number()) -> integer().
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true  -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ceil_test_() ->
    [ ?_assertEqual(0,             ceiling(0))
    , ?_assertEqual(0,             ceiling(0.0))
    , ?_assertEqual(-1,            ceiling(-1))
    , ?_assertEqual(-1,            ceiling(-1.1))
    , ?_assertEqual(1,             ceiling(0.1))
    , ?_assertEqual(42,            ceiling(42))
    , ?_assertEqual(43,            ceiling(42.1))
    , ?_assertEqual(1000000000000, ceiling(999999999999.0001))
    , ?_assertEqual(-999999999999, ceiling(-999999999999.0001))
    ].

floor_test_() ->
    [ ?_assertEqual(0,              floor(0))
    , ?_assertEqual(0,              floor(0.0))
    , ?_assertEqual(-1,             floor(-1))
    , ?_assertEqual(-2,             floor(-1.1))
    , ?_assertEqual(0,              floor(0.1))
    , ?_assertEqual(42,             floor(42))
    , ?_assertEqual(42,             floor(42.1))
    , ?_assertEqual(999999999999,   floor(999999999999.0001))
    , ?_assertEqual(-1000000000000, floor(-999999999999.0001))
    ].

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
