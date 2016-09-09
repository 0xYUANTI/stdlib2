%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Nested dictionaries.
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
-module(s2_maps).

%%%_* Exports ==========================================================
-export([ delete/2
        , get/2
        , get/3
        , new/0
        , set/3
        , to_list/1
        , update/4
        ]).

-export_type([ t/0 ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-ifdef(no_namespaced_types).
-type t() :: dict().
-else.
-type t() :: dict:dict().
-endif.

-spec new() -> t().
%% @doc Return a fresh map.
new() -> dict:new().

-spec set(t(), [_], _) -> t().
%% @doc Set the leaf reachable via Ks in Map to V.
set(Map, Ks, V) -> update(Map, Ks, fun(_) -> V end, V).

-spec update(t(), [_], fun(), _) -> t().
%% @doc Update the leaf reachable via Ks in Map to the result of
%% applying F to its current value or V if it doesn't have one.
update(Map, [K], F, V) ->
  dict:update(K, F, V, Map);
update(Map, [K|Ks], F, V) ->
  Update = fun(Map_) -> update(Map_, Ks, F, V) end,
  dict_update(K, Update, ?thunk(Update(dict:new())), Map).

%% Lazy update
dict_update(K, F, Thunk, Dict) ->
  case dict:find(K, Dict) of
    {ok, V} -> dict:store(K, F(V), Dict);
    error   -> dict:store(K, Thunk(), Dict)
  end.

-spec get(t(), [_]) -> maybe(_, notfound).
%% @doc Look up the value associated with the leaf reachable via Ks in
%% Map.
get(Map, [K]) ->
  case dict:find(K, Map) of
    {ok, _} = Ok -> Ok;
    error        -> {error, notfound}
  end;
get(Map, [K|Ks]) ->
  case dict:find(K, Map) of
    {ok, Map_} -> get(Map_, Ks);
    error      -> {error, notfound}
  end.

get(Map, Ks, Default) ->
  case get(Map, Ks) of
    {ok, X}           -> X;
    {error, notfound} -> Default
  end.

-spec delete(t(), [_]) -> t().
%% @doc Prune everything below Ks from Map.
delete(Map, [K]) ->
  dict:erase(K, Map);
delete(Map, [K|Ks]) ->
  case dict:find(K, Map) of
    {ok, _} -> dict:update(K, fun(Map_) -> delete(Map_, Ks) end, Map);
    error   -> Map
  end.

-spec to_list(t()) -> [_].
%% @doc Return the list-representation of Map.
to_list(Map) ->
  to_list({dict:to_list(Map), []}, [], []).

to_list({[], _}, [], Done) ->
  Done;
to_list({[], _}, Stack, Done) ->
  to_list(hd(Stack), tl(Stack), Done);
to_list({[{K, V}|Rest], Acc}, Stack, Done) ->
  case is_dict(V) of
    true ->
      to_list({dict:to_list(V), [K|Acc]}, [{Rest, Acc}|Stack], Done);
    false ->
      to_list({Rest, Acc}, Stack, [{lists:reverse([K|Acc]), V}|Done])
  end.

is_dict(Dict) ->
  case catch tuple_to_list(Dict) of
    [dict|_] -> true;
    _        -> false
  end.

%%%_* Tests ============================================================
-ifdef(TEST).

flat_get_set_test() ->
  42 = get(set(new(), [foo], 42), [foo], 666),
  42 = get(new(),                 [foo], 42),
  ok.

nested_get_set_test() ->
  Map0     = new(),
  Map1     = set(Map0, [foo, bar], 42),
  {ok, 42} = get(Map1, [foo, bar]),
  Map2     = set(Map1, [foo, bar], 43),
  {ok, 43} = get(Map2, [foo, bar]),
  ok.

delete_test() ->
  Map0              = new(),
  Map1              = set(Map0, [foo, bar, baz],  42),
  Map2              = set(Map1, [foo, bar, quux], 43),
  Map3              = set(Map2, [bar],            44),
  {ok, 42}          = get(Map3, [foo, bar, baz]),
  {ok, 43}          = get(Map3, [foo, bar, quux]),
  {ok, 44}          = get(Map3, [bar]),
  Map4              = delete(Map3, [foo, bar]),
  {error, notfound} = get(Map4, [foo, bar, baz]),
  {error, notfound} = get(Map4, [foo, bar, quux]),
  {ok, 44}          = get(Map4, [bar]),
  Map5              = delete(Map4, [bar]),
  {error, notfound} = get(Map5, [bar]),

  Map2              = delete(Map2, [foo, bar, snarf]),
  Map2              = delete(Map2, [blarg]),

  X                 = new(),
  X                 = delete(X, [foo,bar]),

  ok.

to_list_test() ->
  Map0 = set(new(), [foo, bar,   snarf], 0),
  Map1 = set(Map0,  [foo, bar,   blarg], 1),
  Map2 = set(Map1,  [foo, snarf, baz],   0),
  Map  = set(Map2,  [foo, snarf, quux],  2),
  [ {[foo, snarf, baz],   0}
  , {[foo, snarf, quux],  2}
  , {[foo, bar,   blarg], 1}
  , {[foo, bar,   snarf], 0}
  ] = to_list(Map),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
