%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Maybe Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_maybe).

%%%_* Exports ==========================================================
-export([ do/1
        , lift/1
        , lift/2
        , map/2
        , reduce/2
        , reduce/3
        , to_bool/1
        , unlift/1
        , unlift/2
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec do([fun()])                            -> maybe(_, _).
%% @doc doc(Fs) is the result of chaining Fs inside the maybe monad.
do([F|Fs])                                   -> do(Fs, lift(F)).
do([],     X)                                -> X;
do(_,      {error, Rsn})                     -> {error, Rsn};
do([F|Fs], {ok, _}) when is_function(F, 0)   -> do(Fs, lift(F));
do([F|Fs], {ok, Res}) when is_function(F, 1) -> do(Fs, lift(F, Res)).

do_test() ->
  {ok, 42} =
    do([ fun()  -> 1        end
       , fun(X) -> X        end
       , fun()  -> 0        end
       , fun(0) -> {ok, 41} end
       , fun(X) -> X + 1    end
       ]),
  Exn = fun() -> throw(exn) end,
  catch Exn(), %cover
  {error, foo} =
    do([ fun()  -> foo        end
       , fun(X) -> {error, X} end
       , Exn
       ]).

-spec lift(fun()) -> maybe(_, _).
%% @doc lift(F) is the value of F() lifted into the maybe monad.
lift(F) ->
  try F() of
    {ok, Res}          -> {ok, Res};
    error              -> {error, error};
    {error, Rsn}       -> {error, Rsn};
    Res                -> {ok, Res}
  catch
    throw:{error, Rsn} -> {error, Rsn};
    _:Exn              -> {error, {lifted_exn, Exn, erlang:get_stacktrace()}}
  end.


lift(F, X) -> lift(?thunk(F(X))).

-spec unlift(fun()) -> _.
%% @doc unlift(F) is the result of F() extracted from the maybe monad.
unlift(F) ->
  case F() of
    {ok, Res}    -> Res;
    error        -> throw({error, error});
    {error, Rsn} -> throw({error, Rsn});
    Res          -> Res
  end.

unlift(F, X) -> unlift(?thunk(F(X))).

lift_unlift_test() ->
  {ok, ok}       = ?lift(?unlift(?lift(ok))),
  {ok, ok}       = ?lift(?unlift(?lift({ok, ok}))),
  ok             = ?unlift(?lift(?unlift(ok))),
  ok             = ?unlift(?lift(?unlift({ok, ok}))),
  {error, error} = ?lift(?unlift(?lift(error))),
  {error, error} = ?lift(?unlift(?lift(throw({error, error})))),
  {error, error} = (catch ?unlift(?lift(?unlift(error)))),
  {ok, ok}       = ?lift(ok),
  {ok, 42}       = lift(fun(X) -> X end, 42),
  42             = unlift(fun(X) -> {ok, X} end, 42).


-spec map(fun(), [_]) -> maybe(_, _).
%%@doc map(F, Xs) is the result of mapping F over Xs inside the maybe
%% monad.
map(F, Xs) -> ?lift([?unlift(F(X)) || X <- Xs]).

map_test() ->
  {ok, [1, 2]} = map(fun(X) -> X + 1       end, [0, 1]),
  {ok, [1, 2]} = map(fun(X) -> {ok, X + 1} end, [0, 1]),
  {error, _}   = map(fun(X) -> X + 1       end, [0, foo]).


-spec reduce(fun(), [_]) -> maybe(_, _).
%% @doc reduce(F, Xs) is the result of reducing Xs to F inside the maybe
%% monad.
reduce(F, [Acc0|Xs]) ->
  reduce(F, Acc0, Xs).
reduce(F, Acc0, Xs) ->
  ?lift(lists:foldl(fun(X, Acc) -> ?unlift(F(X, Acc)) end, Acc0, Xs)).

reduce_test() ->
  {ok, 1}    = reduce(fun(X, Y) -> X + Y       end, [0, 1]),
  {ok, 1}    = reduce(fun(X, Y) -> {ok, X + Y} end, [0, 1]),
  {error, _} = reduce(fun(X, Y) -> X + Y       end, [0, foo]).


-spec to_bool(maybe(_, _)) -> boolean().
%% @doc to_bool(X) is the boolean representation of the maybe-value X.
to_bool({ok, _})           -> true;
to_bool({error, _})        -> false.

to_bool_test() ->
  true  = to_bool({ok, foo}),
  false = to_bool({error, foo}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
