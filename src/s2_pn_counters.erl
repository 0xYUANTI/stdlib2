%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Commutative counters.
%%% @reference
%%% http://pagesperso-systeme.lip6.fr/Marc.Shapiro/papers/
%%%   Comprehensive-CRDTs-RR7506-2011-01.pdf
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_pn_counters).
-behaviour(s2_gen_lattice).

%%%_* Exports ==========================================================
%% gen_lattice callbacks
-export([ new/0
        , compare/2
        , merge/2
        , update/2
        , value/1
        ]).

%% API
-export([ dec/1
        , inc/1
        ]).

%% Types
-export_type([ pn_counter/0
             ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-define(M                 ,  s2_vclocks).
-opaque pn_counter()      :: {?M:new(), ?M:new()}.

%%%_ * gen_lattice callbacks -------------------------------------------
new()                     -> {?M:new(), ?M:new()}.

compare(C1, C2)           -> case
                               {value(C1) < value(C2), value(C1) =:= value(C2)}
                             of
                               {true, false}  -> less_than;
                               {false, true}  -> equal;
                               {false, false} -> greater_than
                             end.

merge({P1, N1}, {P2, N2}) -> {?M:merge(P1, P2), ?M:merge(N1, N2)}.

update({P, N}, inc)       -> {?M:update(P, node()), N};
update({P, N}, dec)       -> {P, ?M:update(N, node())}.

value({P, N})             -> ?M:value(P) - ?M:value(N).

%%%_ * API -------------------------------------------------------------
dec(C)                    -> update(C, dec).
inc(C)                    -> update(C, inc).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lattice_test() ->
  C            = new(),
  C1           = update(C, inc),
  C2           = update(C, dec),
  0            = value(C),
  1            = value(C1),
  -1           = value(C2),
  equal        = compare(C,  C),
  greater_than = compare(C1, C),
  less_than    = compare(C2, C),
  less_than    = compare(C,  C1),
  equal        = compare(C1, C1),
  less_than    = compare(C2, C1),
  greater_than = compare(C,  C2),
  greater_than = compare(C1, C2),
  equal        = compare(C2, C2),
  C_           = merge(C1, C2),
  equal        = compare(C, C_),
  0            = value(C_).

api_test() ->
  1  = value(inc(new())),
  -1 = value(dec(new())).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
