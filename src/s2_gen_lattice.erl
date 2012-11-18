%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc A behaviour for lattice-style data types.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_gen_lattice).

%%%_* Exports ==========================================================
-export([ behaviour_info/1
        ]).

-export([ resolve/3
        ]).

-export_type([ relation/0
             ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
behaviour_info(callbacks) ->
  [ {new,     0}
  , {compare, 2}
  , {merge,   2}
  , {update,  2}
  , {value,   1}
  ];
behaviour_info(_) -> undefined.

-type element()                         :: _.           %element of the lattice
%% -type value()                        :: _.           %user-visible value
-type relation()                        :: less_than    %\  Position
                                         | equal        % } in
                                         | greater_than %/  lattice
                                         | concurrent.  %incomparable

%% Uncomment for R15+...
%% -callback new()                         -> element().
%% -callback compare(element(), element()) -> relation().
%% -callback merge(element(), element())   -> element().
%% -callback update(element(), value())    -> element().
%% -callback value(element())              -> value().


-spec resolve(atom(), element(), element()) -> element().
%% @doc resolve(Mod, Elt1, Elt2) is the least upper bound of Elt1 and
%% Elt2 in Mod.
resolve(Mod, Elt1, Elt2) ->
  case Mod:compare(Elt1, Elt2) of
    less_than    -> Elt2;
    equal        -> Elt1;
    greater_than -> Elt1;
    concurrent   -> Mod:merge(Elt1, Elt2)
  end.

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cover_test() ->
  _ = behaviour_info(callbacks),
  _ = behaviour_info(foo).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
