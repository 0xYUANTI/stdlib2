%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Parallel evaluation.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_par).

%%%_* Exports ==========================================================
-export([ map/2
        , map/3
        ]).

-export_type([ opt/0
             ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-type opt() :: {timeout,   non_neg_integer()}
             | {errors,    boolean()}
             | {workers,   non_neg_integer()}
             | {chunksize, non_neg_integer()}.

-spec map(fun((A) -> B), [A]) -> maybe([maybe(B, _) | B], _).
%% @doc map(F, Xs) is the result of mapping F over Xs in parallel.
map(F, Xs) ->
  map(F, Xs, []).
map(_F, [], _Opts) ->
  {ok, []};
map(F, Xs, Opts) ->
  Timeout   = s2_lists:assoc(Opts, timeout,   infinity),
  Errors    = s2_lists:assoc(Opts, errors,    false),
  Workers   = s2_lists:assoc(Opts, workers,   0),
  ChunkSize = s2_lists:assoc(Opts, chunksize, 0),
  Pid       = spawn_middleman(F, Xs, Errors, Workers, ChunkSize, self()),
  Monitor   = erlang:monitor(process, Pid),
  receive
    {Pid, Ret} ->
      erlang:demonitor(Monitor, [flush]),
      Ret;
    {'DOWN', Monitor, process, Pid, Rsn} ->
      ?hence(Rsn =/= normal),
      {error, {middleman, Rsn}}
  after
    Timeout ->
      s2_procs:kill(Pid),
      erlang:demonitor(Monitor, [flush]),
      {error, timeout}
  end.

%%%_ * Supervisor ------------------------------------------------------
%% We want to be able to propagate failure to our workers, so we need to
%% link to them. We can't simply link from the calling process, since it
%% may not be trapping exits. Thusly, the middleman.
spawn_middleman(F, Xs, Errors, Workers, ChunkSize, Parent) ->
  proc_lib:spawn(?thunk(
    process_flag(trap_exit, true),
    Monitor = erlang:monitor(process, Parent),
    Pids    = spawn_workers(F, Xs, Workers, ChunkSize),
    middleman(Errors, Parent, Monitor, Pids, []))).

middleman(_Errors, Parent, _Monitor, [], Acc) ->
  s2_procs:send(Parent, {ok, Acc});
middleman(Errors, Parent, Monitor, Pids, Acc) ->
  receive
    %% Common case
    {Pid, Rets} when Errors ->
      middleman(Errors, Parent, Monitor, Pids -- [Pid], Rets ++ Acc);
    {Pid, Rets} ->
      case lists:partition(fun s2_maybe:to_bool/1, Rets) of
        {Oks, []} ->
          middleman(Errors, Parent, Monitor, Pids -- [Pid], untag(Oks) ++ Acc);
        {_, Errs} ->
          s2_procs:send(Parent, {error, {worker, untag(Errs)}}),
          exit(worker)
      end;
    %% Woker failure
    {'EXIT', Pid, Rsn} ->
      case Rsn of
        normal ->
          middleman(Errors, Parent, Monitor, Pids -- [Pid], Acc);
        Rsn when Errors ->
          middleman(Errors, Parent, Monitor, Pids -- [Pid],
                    [{error, {exit, Rsn}}|Acc]);
        Rsn ->
          s2_procs:send(Parent, {error, {worker, {exit, Rsn}}}),
          exit(worker)
      end;
    %% Parent failure
    {'DOWN', Monitor, process, Parent, Rsn} ->
      exit({parent, Rsn})
    end.

untag(Tagged) -> [Body || {_Tag, Body} <- Tagged].

%%%_ * Workers ---------------------------------------------------------
spawn_workers(F, Xs, Workers, ChunkSize) ->
  Self   = self(),
  Chunks = chunk(Xs, Workers, ChunkSize),
  [proc_lib:spawn_link(?thunk(worker(F, C, Self))) || C <- Chunks].

worker(F, Chunk, Parent) ->
  s2_procs:send(Parent, [?lift(F(X)) || X <- Chunk]).

%%%_ * Chunking --------------------------------------------------------
chunk(Xs, Workers, ChunkSize)
  when is_integer(Workers)
     , is_integer(ChunkSize)
     , Workers >= 0
     , ChunkSize >= 0 ->
  Len    = length(Xs),
  Chunks = chunks(Workers, ChunkSize, Len),
  s2_lists:partition(divide(Len, Chunks), Xs).

%%     Workers ChunkSize Len              Chunks
%% Default
chunks(0,      0,        L)            -> L;
%% ChunkSize
chunks(0,      C,        L) when L > C -> divide(L, C);
chunks(0,      _,        _)            -> 1;
%% Workers
chunks(W,      0,        L) when L > W -> W;
chunks(_,      0,        L)            -> L;
%% ChunkSize & Workers
chunks(W,      C,        L)            -> max(chunks(W,0,L), chunks(0,C,L)).

divide(N, M) when N rem M =:= 0 -> N div M;
divide(N, M) when N rem M =/= 0 -> N div M + 1.

%%%_* Tests ============================================================
-ifdef(TEST).

basic_test() ->
  {ok, []} = map(fun(X) -> X + 1 end, []),
  {ok, Ys} = map(fun(X) -> X + 1 end, [0, 1, 2]),
  true     = length(Ys) =:= 3,
  true     = lists:member(1, Ys),
  true     = lists:member(2, Ys),
  true     = lists:member(3, Ys).

timeout_test() ->
  {ok, []}         = map(fun(X) -> X + 1 end, [],        [{timeout, 0}]),
  {error, timeout} = map(fun(X) -> X + 1 end, [0, 1, 2], [{timeout, 0}]).

errors_test() ->
  {error, {worker, [foo]}} =
    map(fun(F) -> F() end, [?thunk({error, foo})], [{errors, false}]),
  {ok, [{error, foo}]} =
    map(fun(F) -> F() end, [?thunk({error, foo})], [{errors, true}]).

chunk_test() ->
  Xs                        = [1,2,3,4,5],
  [[1], [2], [3], [4], [5]] = chunk(Xs, 0, 0),
  [[1,2,3], [4,5]]          = chunk(Xs, 0, 4),
  [[1, 2, 3, 4, 5]]         = chunk(Xs, 0, 6),
  [[1,2,3], [4,5]]          = chunk(Xs, 2, 0),
  [[1], [2], [3], [4], [5]] = chunk(Xs, 6, 0),
  [[1,2], [3,4], [5]]       = chunk(Xs, 2, 2).

middleman_down_test() ->
  {error, {middleman, _}} = map(fun(X) -> X end, [0, 1, 2], [{workers, foo}]).

worker_exit_test() ->
  F                    = fun(_) ->
                           Self = self(),
                           _Pid = spawn(?thunk(s2_procs:kill(Self))),
                           timer:sleep(100)
                         end,
  {error, {worker, _}} = map(F, [foo]),
  {ok, [{error, _}]}   = map(F, [foo], [{errors, true}]).

parent_down_test() ->
  Worker = fun(_) -> s2_loop:for(infinity, fun() -> timer:sleep(100) end) end,
  Pid    = spawn(?thunk(map(Worker, [0, 1, 2]))),
  timer:sleep(100),
  s2_procs:kill(Pid),
  timer:sleep(100).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
