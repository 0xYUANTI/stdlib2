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
