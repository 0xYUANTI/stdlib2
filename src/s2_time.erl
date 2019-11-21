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
        , unix_epoch/0
        ]).

%%%_* Includes =========================================================
-include("prelude.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%_* Code =============================================================
-spec datetime() -> tuple().
%% @doc datetime() is the current date and time..
datetime()       -> calendar:now_to_datetime(os:timestamp()).

-ifdef(TEST).
datetime_test()  -> {{_Ye, _Mo, _Da}, {_Ho, _Mi, _Se}} = datetime().
-endif.


-spec stamp() -> non_neg_integer().
%% @doc stamp() is the number of microseconds since the Unix epoch.
stamp()       -> stamp(os).
stamp(now)    -> stamp(os);
stamp(os)     -> now_to_microsecs(os:timestamp()).

-ifdef(TEST).
stamp_test()  -> ?assert(stamp()   < stamp()),
                 ?assert(stamp(os) < stamp(os)).
-endif.

unix_epoch() ->
  {MegaSecs, Secs, _} = os:timestamp(),
  MegaSecs * 1000000 + Secs.

now_to_microsecs({MegaSecs, Secs, MicroSecs}) ->
  (1000000 * 1000000 * MegaSecs) + (1000000 * Secs) + MicroSecs.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
