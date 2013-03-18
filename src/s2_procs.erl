%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Processes.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_procs).

%%%_* Exports ==========================================================
-export([ flush/0
        , flush/1
        , is_up/1
        , kill/1
        , kill/2
        , pid/1
        , recv/1
        , recv/2
        , send/2
        , spinlock/1
        , with_monitor/2
        ]).

-export_type([ proc/0
             ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-type proc()  :: pid() | atom() | {atom(),  node()}.

%%%_ * flush -----------------------------------------------------------
-spec flush() -> ok.
%% @doc flush() causes the calling process's message queue to be
%% emptied.
flush() ->
  receive _ -> flush()
  after   0 -> ok
  end.

-spec flush(_) -> ok.
%% @doc flush() causes the calling process's message queue to be
%% emptied of Msgs.
flush(Msg) ->
  receive Msg -> flush(Msg)
  after   0   -> ok
  end.

flush_test() ->
  self() ! foo,
  self() ! bar,
  flush(),
  baz = receive foo -> foo; bar -> bar after 0 -> baz end,
  self() ! foo,
  self() ! bar,
  flush(foo),
  bar = receive foo -> foo; bar -> bar after 0 -> baz end.

%%%_ * is_up -----------------------------------------------------------
-spec is_up(proc()) -> boolean().
%% @doc is_up(Proc) is true iff Proc is running.
is_up(Proc) ->
  erlang:demonitor(erlang:monitor(process, pid(Proc)), [info, flush]).

is_up_test() ->
  Pid1  = spawn(?thunk(ok)),
  Pid2  = spawn(?thunk(receive after infinity -> ok end)),
  timer:sleep(1),
  false = is_up(Pid1),
  true  = is_up(Pid2).

%%%_ * kill ------------------------------------------------------------
-spec kill([proc()]) -> boolean().
%% @doc kill(Procs) causes Procs to be terminated.
kill(Proc) ->
  kill(Proc, []).
kill(Proc, Opts) when not is_list(Proc) ->
  kill([Proc], Opts);
kill(Procs, Opts)
  when is_list(Procs)
     , is_list(Opts) ->
  [] =:= [Ret || Ret <- [do_kill(pid(Proc), Opts) || Proc <- Procs],
                 Ret =/= true].

do_kill(undefined, []) ->
  false;
do_kill(Pid, [flush]) ->
  {trap_exit, true} = process_info(self(), trap_exit), %assert
  {links, Pids}     = process_info(self(), links),
  ?hence(lists:member(Pid, Pids)),
  do_kill(Pid, []),
  receive {'EXIT', Pid, killed} -> true end;
do_kill(Pid, [unlink]) ->
  unlink(Pid),
  do_kill(Pid, []);
do_kill(Pid, []) when is_pid(Pid) ->
  erlang:exit(Pid, kill).

kill_test() ->
  process_flag(trap_exit, true),
  F     = ?thunk(receive after infinity -> ok end),
  Pid1  = spawn_link(F),
  true  = kill(Pid1, [unlink]),
  timer:sleep(100),
  false = is_up(Pid1),
  Pid2  = spawn_link(F),
  true  = kill(Pid2, [flush]),
  timer:sleep(100),
  false = is_up(Pid1),
  false = kill(nosuchregname).

%%%_ * pid -------------------------------------------------------------
-spec pid(proc())            -> pid() | undefined.
%% @doc pid(Proc) is Proc's pid.
pid(undefined)               -> undefined;
pid(Pid) when is_pid(Pid)    -> Pid;
pid(Name) when is_atom(Name) -> whereis(Name);
pid({Name, Node})            -> case rpc:call(Node, erlang, whereis, [Name]) of
                                  Pid when is_pid(Pid) -> Pid;
                                  _                    -> undefined %badrpc
                                end.

pid_test() ->
  Self      = self(),
  true      = register(pid_test, Self),
  Self      = pid(Self),
  Self      = pid(pid_test),
  undefined = pid(undefined),
  undefined = pid(nosuchregname),
  Self      = pid({pid_test, node()}),
  undefined = pid({pid_test, nosuch@node}).

%%%_ * send/recv -------------------------------------------------------
-spec send(proc(), _) -> whynot(no_such_process).
%% @doc send(Proc, Msg) causes Msg to be sent to Proc.
send(Proc, Msg0) ->
  {Tag, _, _} = tab(Proc),
  Msg         = {Tag, Msg0},
  case catch Proc ! Msg of
    Msg                   -> ok;
    {'EXIT', {badarg, _}} -> {error, no_such_process}
  end.

-spec recv(proc()) -> maybe(_, _).
%% @doc recv(Proc) is the next message received from Proc.
recv(Proc) ->
  recv(Proc, infinity).
recv(Proc, Timeout) ->
  {_, From, _} = tab(Proc),
  receive {From, Msg} -> {ok, Msg}
  after Timeout       -> {error, timeout}
  end.
recv(Proc, Monitor, Timeout) ->
  {_, From, Obj} = tab(Proc),
  receive
    {From, Msg}                          -> {ok, Msg};
    {'DOWN', Monitor, process, Obj, Rsn} -> {error, {down, Rsn}}
  after
    Timeout                              -> {error, timeout}
  end.

%%                               Tag     From           Obj
tab(Pid)  when is_pid(Pid)   -> {self(), Pid,           Pid};
tab(Name) when is_atom(Name) -> {self(), whereis(Name), {Name, node()}};
tab({Name, Node})            -> {node(), Node,          {Name, Node}}.

send_recv_test() ->
  Self                     = self(),
  Pid                      = spawn(?thunk({ok, foo} = recv(Self), ok = send(Self, bar))),
  Pid1                     = spawn(?thunk({ok, foo} = recv(Self), ok = send(Self, bar))),
  Pid2                     = spawn(?thunk(ok)),
  Monitor1                 = erlang:monitor(process, Pid1),
  Monitor2                 = erlang:monitor(process, Pid2),

  ok                       = send(Pid, foo),
  {ok, bar}                = recv(Pid),
  {error, timeout}         = recv(Pid, 1),

  ok                       = send(Pid1, foo),
  {ok, bar}                = recv(Pid1, Monitor1, infinity),

  {error, {down, normal}}  = recv(Pid2, Monitor2, infinity),
  {error, timeout}         = recv(Pid2, Monitor2, 0),

  {error, no_such_process} = send(notregistered, msg).

tab_test() ->
  true = {node(), node(), {name, node()}} =:= tab({name, node()}).

%%%_ * spinlock --------------------------------------------------------
-spec spinlock(fun(() -> boolean())) -> true | no_return().
%% @doc Spin until F returns true.
spinlock(F) -> F() orelse spinlock(F).

spinlock_test() ->
  spawn(?thunk(spinlock(
    fun() ->
      lists:member(spinlock_test, registered())
        orelse begin register(spinlock_test, self()), false end
    end))).

%%%_ * with_monitor ----------------------------------------------------
-spec with_monitor(proc(), fun(({proc(), reference()}) -> A)) -> A.
%% @doc with_monitor(Proc, F) causes F to be called while Proc is being
%% monitored.
with_monitor(Proc, F) ->
  Monitor = erlang:monitor(process, Proc),
  try F({Proc, Monitor})
  after erlang:demonitor(Monitor, [flush])
  end.

with_monitor_test() ->
  {error, {down, _}} =
    with_monitor(spawn(?thunk(ok)),
                 fun({Proc, Monitor}) -> recv(Proc, Monitor, infinity) end).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
