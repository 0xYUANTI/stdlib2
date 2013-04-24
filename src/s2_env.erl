%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Environment.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_%%%_* Module declaration ===============================================
-module(s2_env).

%%%_* Exports ==========================================================
-export([ ensure_started/1
        , get_arg/3
        , get_arg/4
        , with_sys/2
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * ensure_started --------------------------------------------------
-spec ensure_started(atom() | [atom()]) -> ok | no_return().
%% @doc Ensure that Apps are started.
ensure_started(App) when is_atom(App) ->
  ensure_started([App]);
ensure_started(Apps) when is_list(Apps) ->
  _ = [case application:start(App) of
         ok                              -> ok;
         {error, {already_started, App}} -> ok
       end || App <- Apps],
  ok.

%%%_ * get_arg ---------------------------------------------------------
-spec get_arg(alist(A, B), atom(), A) -> B.
%% @doc get_arg(Args, App, Param, Def) is the value associated with
%% Param in either Args or App's environment.
get_arg(Args, App, Param) ->
  case s2_lists:assoc(Args, Param) of
    {ok, Val}         -> Val;
    {error, notfound} ->
      case application:get_env(App, Param) of
        {ok, Val} -> Val;
        undefined -> throw({missing_param, Param})
      end
  end.


-spec get_arg(alist(A, B), atom(), A, B) -> B.
%% @doc get_arg(Args, App, Param, Def) is the value associated with
%% Param in either Args or App's environment; or Def.
get_arg(Args, App, Param, Def) ->
  case s2_lists:assoc(Args, Param) of
    {ok, Val}         -> Val;
    {error, notfound} ->
      case application:get_env(App, Param) of
        {ok, Val} -> Val;
        undefined -> Def
      end
  end.

get_arg_test() ->
  application:set_env(myapp, myparam, 43),
  42 = get_arg([{myparam, 42}], myapp, myparam, 44),
  43 = get_arg([],              myapp, myparam, 44),
  application:unset_env(myapp, myparam),
  44 = get_arg([],              myapp, myparam, 44).

%%%_ * with_sys --------------------------------------------------------
-type sys() :: [{App::atom(), Param::atom(), Val::_}].

-spec with_sys(sys(), thunk(A)) -> A.
%% @doc with_sys(Sys, Thunk) is the result of calling Thunk in Sys.
with_sys(Sys, Thunk) ->
  Apps  = [App || {App, _, _} <- Sys],
  Envs  = parse(Sys),
  Envs0 = [{App, application:get_all_env(App)} || {App, _Env} <- Envs],
  try
    set(Envs),
    [start(App) || App <- Apps],
    Thunk()
  after
    [stop(App) || App <- Apps],
    set(Envs0)
  end.


parse(Sys) ->
  dict:to_list(
    lists:foldl(
      fun({App, K, V}, Dict) ->
        dict:update(App, fun(KVs) -> [{K, V}|KVs] end, [{K, V}], Dict)
      end,
      dict:new(),
      lists:usort( %earlier entries take priority
        fun({App, K, _}, {App, K, _}) -> true;
           (X,           Y)           -> X < Y
        end, Sys))).

set(Envs) ->
  [ok = set(App, Env) || {App, Env} <- Envs].
set(App, Env) ->
  [ok = application:unset_env(App, K)  || {K, _} <- application:get_all_env(App)],
  [ok = application:set_env(App, K, V) || {K, V} <- Env].

start(App) ->
  ok      = application:start(App),
  {ok, R} = application:get_key(App, registered),
  ok      = sync_registered(R).

stop(App) ->
  case application:get_key(App, registered) of
    {ok, R} ->
      ok = application:stop(App),
      ok = sync_unregistered(R);
    undefined ->
      ok = application:stop(App)
  end.

sync_registered(Regnames) when is_list(Regnames) ->
  [sync_registered(Regname) || Regname <- Regnames];
sync_registered(Regname) ->
  case lists:member(Regname, registered()) of
    true  -> ok;
    false -> sync_registered(Regname)
  end.

sync_unregistered(Regnames) when is_list(Regnames) ->
  [sync_unregistered(Regname) || Regname <- Regnames];
sync_unregistered(Regname) ->
  case lists:member(Regname, registered()) of
    true  -> sync_unregistered(Regname);
    false -> ok
  end.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
