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
-include("prelude.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
-spec datetime() -> tuple().
%% @doc datetime() is the current date and time..
datetime()       -> calendar:now_to_datetime(os:timestamp()).

datetime_test()  -> {{_Ye, _Mo, _Da}, {_Ho, _Mi, _Se}} = datetime().


-spec stamp() -> non_neg_integer().
%% @doc stamp() is the number of microseconds since the Unix epoch.
stamp()       -> stamp(os).
stamp(now)    -> stamp(os);
stamp(os)     -> now_to_microsecs(os:timestamp()).

stamp_test()  -> ?assert(stamp()   < stamp()),
                 ?assert(stamp(os) < stamp(os)).

now_to_microsecs({MegaSecs, Secs, MicroSecs}) ->
  (1000000 * 1000000 * MegaSecs) + (1000000 * Secs) + MicroSecs.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
