%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Standard Erlang Prelude.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Header ===========================================================
-ifndef(__PRELUDE_HRL).
-define(__PRELUDE_HRL, true).

%%%_* Assertions =======================================================
-define(hence(A),
        (case A of
           true -> ok;
           _    -> throw({error, {assert, {??A, '=', true}, ?FILE, ?LINE}})
         end)).

-define(given(A, B),
        (case ((not (A)) orelse (B)) of
           true -> ok;
           _    -> throw({error, {assert, {??A, '->', ??B}, ?FILE, ?LINE}})
         end)).

%%%_* Casts ============================================================
-define(a2l(X), erlang:atom_to_list(X)).
-define(l2a(X), erlang:list_to_atom(X)).
-define(b2l(X), erlang:binary_to_list(X)).
-define(l2b(X), erlang:list_to_binary(X)).
-define(b2t(X), erlang:binary_to_term(X)).
-define(t2b(X), erlang:term_to_binary(X)).
-define(i2l(X), erlang:integer_to_list(X)).
-define(l2i(X), erlang:list_to_integer(X)).
-define(l2t(X), erlang:list_to_tuple(X)).
-define(t2l(X), erlang:tuple_to_list(X)).

%%%_* Eccentric ========================================================
-define(lift(E),   s2_maybe:lift(fun() -> E end)).
-define(unlift(E), s2_maybe:unlift(fun() -> E end)).

-define(do(F0, F1),
        s2_maybe:do([F0, F1])).
-define(do(F0, F1, F2),
        s2_maybe:do([F0, F1, F2])).
-define(do(F0, F1, F2, F3),
        s2_maybe:do([F0, F1, F2, F3])).
-define(do(F0, F1, F2, F3, F4),
        s2_maybe:do([F0, F1, F2, F3, F4])).
-define(do(F0, F1, F2, F3, F4, F5),
        s2_maybe:do([F0, F1, F2, F3, F4, F5])).
-define(do(F0, F1, F2, F3, F4, F5, F6),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7, F8),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7, F8])).
-define(do(F0, F1, F2, F3, F4, F5, F6, F7, F8, F9),
        s2_maybe:do([F0, F1, F2, F3, F4, F5, F6, F7, F8, F9])).

-define(ido(F0, F1),
        ?ido([F0, F1])).
-define(ido(F0, F1, F2),
        ?ido([F0, F1, F2])).
-define(ido(F0, F1, F2, F3),
        ?ido([F0, F1, F2, F3])).
-define(ido(F0, F1, F2, F3, F4),
        ?ido([F0, F1, F2, F3, F4])).
-define(ido(F0, F1, F2, F3, F4, F5),
        ?ido([F0, F1, F2, F3, F4, F5])).
-define(ido(F0, F1, F2, F3, F4, F5, F6),
        ?ido([F0, F1, F2, F3, F4, F5, F6])).
-define(ido(F0, F1, F2, F3, F4, F5, F6, F7),
        ?ido([F0, F1, F2, F3, F4, F5, F6, F7])).
-define(ido(F0, F1, F2, F3, F4, F5, F6, F7, F8),
        ?ido([F0, F1, F2, F3, F4, F5, F6, F7, F8])).
-define(ido(F0, F1, F2, F3, F4, F5, F6, F7, F8, F9),
        ?ido([F0, F1, F2, F3, F4, F5, F6, F7, F8, F9])).

%% Instrumented do.
-define(ido(Fs),
        (case ?time(?FUNCTION, s2_maybe:do(Fs)) of
           {ok, ___Res} = ___Ok ->
             ?debug("~p: ok: ~p", [?FUNCTION, ___Res]),
             ?increment(?FUNCTION, ok),
             ___Ok;
           {error, ___Rsn} = ___Err ->
             ?error("~p: error: ~p", [?FUNCTION, ___Rsn]),
             ?increment(?FUNCTION, error),
             ___Err
         end)).

-define(thunk(E0),
        fun() -> E0 end).
-define(thunk(E0, E1),
        fun() -> E0, E1 end).
-define(thunk(E0, E1, E2),
        fun() -> E0, E1, E2 end).
-define(thunk(E0, E1, E2, E3),
        fun() -> E0, E1, E2, E3 end).
-define(thunk(E0, E1, E2, E3, E4),
        fun() -> E0, E1, E2, E3, E4 end).
-define(thunk(E0, E1, E2, E3, E4, E5),
        fun() -> E0, E1, E2, E3, E4, E5 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6),
        fun() -> E0, E1, E2, E3, E4, E5, E6 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7, E8),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7, E8 end).
-define(thunk(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9),
        fun() -> E0, E1, E2, E3, E4, E5, E6, E7, E8, E9 end).

%%%_* Emacs ============================================================
-define(Q,  $\'). %'
-define(QQ, $\"). %"

%%%_* Guards ===========================================================
-define(is_string(X),
        (((X) =:= "") orelse (is_list(X) andalso is_integer(hd(X))))).

-define(is_thunk(X), is_function(X, 0)).

%%%_* Logging ==========================================================
-ifdef(S2_USE_LAGER).

-compile([{parse_transform, lager_transform}]).

-define(debug(Format),           lager:debug(Format, [])).
-define(debug(Format, Args),     lager:debug(Format, Args)).
-define(info(Format),            lager:info(Format, [])).
-define(info(Format, Args),      lager:info(Format, Args)).
-define(notice(Format),          lager:notice(Format, [])).
-define(notice(Format, Args),    lager:notice(Format, Args)).
-define(warning(Format),         lager:warning(Format, [])).
-define(warning(Format, Args),   lager:warning(Format, Args)).
-define(error(Format),           lager:error(Format, [])).
-define(error(Format, Args),     lager:error(Format, Args)).
-define(critical(Format),        lager:critical(Format, [])).
-define(critical(Format, Args),  lager:critical(Format, Args)).
-define(alert(Format),           lager:alert(Format, [])).
-define(alert(Format, Args),     lager:alert(Format, Args)).
-define(emergency(Format),       lager:emergency(Format, [])).
-define(emergency(Format, Args), lager:emergency(Format, Args)).

-else.

-ifdef(S2_DEBUG).
-define(debug(Msg),            ?debug(Msg, [])).
-define(debug(Fmt, As),        ?do_debug("~p:~s:~p: Debug: " Fmt "~n",
                                         [self(), ?FILE, ?LINE|As])).
-else.
-define(debug(Msg),            ok).
-define(debug(Fmt, As),        ok).
-endif. %S2_DEBUG

-define(info(Msg),             ?info(Msg, [])).
-define(info(Fmt, As),         ?do_info("~p:~s:~p: Info: " Fmt "~n",
                                        [self(), ?FILE, ?LINE|As])).
-define(notice(Msg),           ?notice(Msg, [])).
-define(notice(Fmt, As),       ?do_notice("~p:~s:~p: Notice: " Fmt "~n",
                                          [self(), ?FILE, ?LINE|As])).
-define(warning(Msg),          ?warning(Msg, [])).
-define(warning(Fmt, As),      ?do_warning("~p:~s:~p: Warning: " Fmt "~n",
                                           [self(), ?FILE, ?LINE|As])).
-define(error(Msg),            ?error(Msg, [])).
-define(error(Fmt, As),        ?do_error("~p:~s:~p: Error: " Fmt "~n",
                                         [self(), ?FILE, ?LINE|As])).
-define(critical(Msg),         ?critical(Msg, [])).
-define(critical(Fmt, As),     ?do_critical("~p:~s:~p: Critical: " Fmt "~n",
                                            [self(), ?FILE, ?LINE|As])).
-define(alert(Msg),            ?alert(Msg, [])).
-define(alert(Fmt, As),        ?do_alert("~p:~s:~p: Alert: " Fmt "~n",
                                         [self(), ?FILE, ?LINE|As])).
-define(emergency(Msg),        ?emergency(Msg, [])).
-define(emergency(Fmt, As),    ?do_emergency("~p:~s:~p: Emergency: " Fmt "~n",
                                             [self(), ?FILE, ?LINE|As])).

-ifdef(S2_NOLOG).

-define(do_debug(Fmt, As),     ok).
-define(do_info(Fmt, As),      ok).
-define(do_notice(Fmt, As),    ok).
-define(do_warning(Fmt, As),   ok).
-define(do_error(Fmt, As),     ok).
-define(do_critical(Fmt, As),  ok).
-define(do_alert(Fmt, As),     ok).
-define(do_emergency(Fmt, As), ok).

-else. %default

-define(do_debug(Fmt, As),     error_logger:info_msg(Fmt, As)).
-define(do_info(Fmt, As),      error_logger:info_msg(Fmt, As)).
-define(do_notice(Fmt, As),    error_logger:info_msg(Fmt, As)).
-define(do_warning(Fmt, As),   error_logger:warning_msg(Fmt, As)).
-define(do_error(Fmt, As),     error_logger:warning_msg(Fmt, As)).
-define(do_critical(Fmt, As),  error_logger:error_msg(Fmt, As)).
-define(do_alert(Fmt, As),     error_logger:error_msg(Fmt, As)).
-define(do_emergency(Fmt, As), error_logger:error_msg(Fmt, As)).

-endif. %S2_NOLOG

-endif. %S2_USE_LAGER

%%%_* Metrics ==========================================================
%% Luke Gorrie's favourite profiling macro.
-define(TIME(Tag, Expr),
        (fun() ->
           %% NOTE: timer:tc/4 does an annoying 'catch' so we
           %% need to wrap the result in 'ok' to be able to
           %% detect an unhandled exception.
           {__TIME, __RESULT} =
             timer:tc(erlang, apply, [fun() -> {ok, Expr} end, []]),
           io:format("time(~s): ~18.3fms ~999p~n",
                     [?MODULE, __TIME/1000, Tag]),
           case __RESULT of
             {ok, _}         -> element(2, __RESULT);
             {'EXIT', Error} -> exit(Error)
           end
         end)()).

-ifdef(S2_USE_FOLSOM).

-define(name(Xs), (s2_atoms:catenate(s2_lists:intersperse('_', Xs)))).

-define(do_increment(Name),
        (catch folsom_metrics:notify({?name(Name), 1}))).
-define(do_increment(Fun, Ret),
        ?do_increment([?APP, ?MODULE, Fun, Ret])).
-define(do_time(Name, Expr),
        (try case is_list(Name) of
           true ->
             folsom_metrics:histogram_timed_update(?name(Name),
                                                   ?thunk(Expr));
           false ->
             folsom_metrics:histogram_timed_update(?name([?MODULE, ?APP, Name]),
                                                   ?thunk(Expr))
         end catch _:_ -> Expr
         end)).

-else.

-define(name(X), X).

-define(do_increment(Name),     ok).
-define(do_increment(Fun, Ret), ok).
-define(do_time(Name, Expr),    Expr).

-endif.

-define(increment(Name),     ?do_increment(Name)).
-define(increment(Fun, Ret), ?do_increment(Fun, Ret)).
-define(time(Name, Expr),    ?do_time(Name, Expr)).

%%%_* Misc =============================================================
-define(FUNCTION,
        (element(2, element(2, process_info(self(), current_function))))).

-define(UUID(), (s2_rand:int())).

%%%_* Types ============================================================
-type alist(A, B) :: [{A, B}].
-type fd()        :: file:io_device().
-type file()      :: string().
-type thunk(A)    :: fun(() -> A).

-type ok(A)       :: {ok, A}.
-type error(A)    :: {error, A}.
-type maybe(A, B) :: {ok, A} | {error, B}.
-type whynot(B)   :: ok | {error, B}.

%%%_* Footer ===========================================================
-endif. %include guard

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
