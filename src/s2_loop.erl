%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Loops.
%%%
%%% Copyright 2011-2013 Klarna AB
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_loop).

%%%_* Exports ==========================================================
-export([ for/2
        , for/3
        , retry/1
        , retry/2
        , retry/3
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


-spec retry(fun()) -> maybe(_, _).
%% @doc Call F every T milliseconds until it returns ok.
%% Abort after N retries unless N is infinity.
%% Note that F is called at least once.
retry(F) ->
  retry(F, timer:seconds(1)).
retry(F, T) ->
  retry(F, T, infinity).
retry(F, T, N) ->
  retry(s2_maybe:lift(F), F, T, N).

retry({ok, _} = Ok, _F, _T, _N) ->
  Ok;
retry({error, _} = Err, _F, _T, 0) ->
  Err;
retry({error, _}, F, T, N) when N > 0 ->
  timer:sleep(T),
  retry(s2_maybe:lift(F), F, T, dec(N)).

dec(infinity) -> infinity;
dec(N)        -> N-1.

retry_test() ->
  F = ?thunk(receive foo -> self() ! bar
             after   0   -> self() ! foo, {error, foo}
             end),
  G = ?thunk(receive bar -> ok
             after   0   -> throw(exn)
             end),
  {ok, bar} = retry(F),
  G(),
  {error, foo} = retry(F, 1, 0),
  exn = (catch G()).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
