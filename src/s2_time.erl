%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Time.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_time).

%%%_* Exports ==========================================================
-export([ datetime/0
        , stamp/0
        , stamp/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec datetime() -> tuple().
%% @doc datetime() is the current date and time..
datetime()       -> calendar:now_to_datetime(os:timestamp()).

datetime_test()  -> {{_Ye, _Mo, _Da}, {_Ho, _Mi, _Se}} = datetime().


-spec stamp() -> non_neg_integer().
%% @doc stamp() is the number of microseconds since the Unix epoch.
stamp()       -> stamp(now).

-ifdef(has_erlang_now).
stamp(now)    -> now_to_microsecs(now());
stamp(os)     -> now_to_microsecs(os:timestamp()).
-else.
stamp(now)    -> monotonic_us();
stamp(os)     -> now_to_microsecs(os:timestamp()).

monotonic_us() ->
    erlang:monotonic_time(micro_seconds) + erlang:time_offset(micro_seconds).
-endif.

stamp_test() -> ?assert(stamp()   < stamp()),
                ?assert(stamp(os) < stamp(os)).

now_to_microsecs({MegaSecs, Secs, MicroSecs}) ->
  (1000000 * 1000000 * MegaSecs) + (1000000 * Secs) + MicroSecs.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
