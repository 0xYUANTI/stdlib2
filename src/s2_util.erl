%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Miscellaneous.
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
-module(s2_util).

%%%_* Exports ==========================================================
-export([ consult_string/1
        , init_folsom/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec consult_string(string()) -> maybe(term(), _).
%% @doc Parse String as an Erlang term.
consult_string(String) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _}    -> erl_parse:parse_term(Tokens);
    {error, Info, Loc} -> {error, {Info, Loc}}
  end.

consult_string_test() ->
  {ok, 42}   = consult_string("42"),
  {error, _} = consult_string("{42"),
  {error, _} = consult_string([12345]).


init_folsom(Metrics) ->
  ?lift([begin
           F = s2_atoms:catenate(['new_', Type]),
           A = ?name(Name),
           folsom_metrics:F(A),
           folsom_metrics:tag_metric(A, {app, App}),
           folsom_metrics:tag_metric(A, {mod, Mod}),
           folsom_metrics:tag_metric(A, {func, Func}),
           [folsom_metrics:tag_metric(A, {ret, Ret}) || Ret <- Rest]
         end || {Type, [App, Mod, Func|Rest] = Name} <- Metrics]).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
