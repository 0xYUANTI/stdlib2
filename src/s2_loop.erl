%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Loops.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_loop).

%%%_* Exports ==========================================================
-export([ for/2
        , for/3
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec for(non_neg_integer(),
          fun(() -> _) | fun((non_neg_integer()) -> _)) -> ok.
%% @doc for(N, F) causes F to be called N times.
for(N, F) ->
  for(0, N, F).
for(N, N, _F) ->
  ok;
for(I, N, F) ->
  if is_function(F, 0) -> _ = F();
     is_function(F, 1) -> _ = F(I)
  end,
  for(I + 1, N, F).

for_test() ->
  for(3, fun()  -> io:format(user, "foo~n",   [])  end),
  for(3, fun(I) -> io:format(user, "foo~p~n", [I]) end).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
