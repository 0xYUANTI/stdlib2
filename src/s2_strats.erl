%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Simple supervision strategies.
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
-module(s2_strats).

%%%_* Exports ==========================================================
-export([ permanent_worker_spec/1
        , permanent_worker_spec/2
        , supervisor_spec/1
        , supervisor_spec/2
        , supervisor_supervisor_strat/1
        , transient_worker_spec/1
        , transient_worker_spec/2
        , worker_supervisor_strat/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Code =============================================================
%% @doc Return our default spec for supervisors which supervise
%% other supervisors. Propagates crashes upwards in the tree.
supervisor_supervisor_strat(Specs) -> {{one_for_all, 0, 1}, Specs}.

%% @doc Return our default spec for supervisors which supervise
%% workers. Propagates crashes which occur more than four times per
%% hour upwards.
worker_supervisor_strat(Specs) -> {{one_for_one, 4, 3600}, Specs}.


%% Our child specifications are based on the following assumptions:
%%   . Consistent naming
%%       [Id] = Module
%%       {Id, start_link, _} = StartFunc
%%   . `temporary' children are confusing
%%   . Crash-only software
%%       brutal_kill = Shutdown
permanent_worker_spec(M)    -> permanent_worker_spec(M, []).
permanent_worker_spec(M, A) -> worker_spec(M, A, permanent).

transient_worker_spec(M)    -> transient_worker_spec(M, []).
transient_worker_spec(M, A) -> worker_spec(M, A, transient).

worker_spec(M, A, Restart) ->
  {M, {M, start_link, A}, Restart, brutal_kill, worker, [M]}.

supervisor_spec(M) ->
  supervisor_spec(M, []).
supervisor_spec(M, A) ->
  {M, {M, start_link, A}, permanent, infinity, supervisor, [M]}.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
