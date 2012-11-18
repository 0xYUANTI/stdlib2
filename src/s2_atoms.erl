%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Atoms.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_atoms).

%%%_* Exports ==========================================================
-export([ catenate/1
        , gensym/0
        , gensym/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
-spec catenate([atom() | integer() | float() | string()]) -> atom().
%% @doc catenate(Args) is the concatenation of Args as an atom.
catenate(Args) -> ?l2a(lists:concat(Args)).

catenate_test() ->
  'foo426.66000000000000000000e+02bar' = catenate([foo, 42, 666.0, "bar"]).


-spec gensym() -> atom().
%% @doc gensym() is a fresh atom.
gensym() ->
  gensym('$gensym').
gensym(Prefix0) ->
  Prefix                = s2_lists:to_list(Prefix0),
  Ref                   = erlang:ref_to_list(make_ref()),
  {ok, Suffix0, []}     = io_lib:fread("#Ref<~d.~d.~d.~d>", Ref),
  Suffix                = lists:concat(Suffix0),
  Sym                   = Prefix ++ Suffix,
  {'EXIT', {badarg, _}} = (catch list_to_existing_atom(Sym)), %assert unique
  ?l2a(Sym).

gensym0_test() -> ?assert(gensym()    =/= gensym()).
gensym1_test() -> ?assert(gensym(foo) =/= gensym(foo)).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
