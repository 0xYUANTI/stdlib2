%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Export functions from the REPL.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_export).

%%%_* Exports ==========================================================
-export([ all/1
        , call/3
        ]).

-export([ parse_transform/2
        ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
-spec all(atom()) -> ok.
%% @doc all(Mod) causes Mod to be recompiled with all its functions
%% exported.
all(Mod) ->
  File                        = code:which(Mod),
  {ok, {Mod, [AbstractCode]}} = beam_lib:chunks(File, [abstract_code]),
  {abstract_code, {_, Forms}} = AbstractCode,
  {ok, Mod, Bin}              = compile:forms(Forms, [export_all]),
  true                        = code:soft_purge(Mod),
  {module, Mod}               = code:load_binary(Mod, "", Bin),
  ok.


-spec call(atom(), atom(), [_]) -> _.
%% @equiv apply(M, F, A).
call(M, F, A) when not is_list(A) ->
  call(M, F, [A]);
call(M, F, A) ->
  {ok, File}   = s2_lists:assoc(M:module_info(compile), source),
  {ok, M, Bin} = compile:file(File, [ {parse_transform, ?MODULE}
                                    , {export_fun, {F, length(A)}}
                                    , binary
                                    ]),
  true         = code:soft_purge(M),
  {module, M}  = code:load_binary(M, "", Bin),
  Ret          = apply(M, 'APPLY', A),
  true         = code:soft_purge(M),
  {module, M}  = code:load_file(M),
  Ret.

%%%_ * Parse transform -------------------------------------------------
parse_transform(AbstractCode0, Opts) ->
  case s2_lists:assoc(Opts, export_fun) of
    {ok, {F, Arity}} ->
      N             = last_line(AbstractCode0),
      AbstractCode1 = add_form(AbstractCode0, mk_apply(F, Arity, N), N + 4),
      add_export(AbstractCode1, {'APPLY', Arity});
    {error, notfound} ->
      AbstractCode0
  end.

last_line(AbstractCode) ->
  {eof, Line} = lists:last(AbstractCode),
  Line.

mk_apply(F, Arity, N) ->
  {function, N + 1, 'APPLY', Arity,
   [{clause, N + 2, mk_vars(Arity, N + 2), [],
     [{call, N + 3, { 'fun'
                    , N + 3
                    , {function, F, Arity}}
                    , mk_vars(Arity, N + 3)
                    }]}]}.

mk_vars(0,   _)    -> [];
mk_vars(Idx, Line) -> [{var, Line, mk_var(Idx)}|mk_vars(Idx - 1, Line)].
mk_var(Idx)        -> s2_atoms:catenate(['V', Idx]).

add_form([{eof, _Line}], NewForm, Line) ->
  [NewForm, {eof, Line}];
add_form([Form|Forms],  NewForm, Line) ->
  [Form|add_form(Forms, NewForm, Line)].

add_export([{attribute, Line, export, Exports}|Forms], Export) ->
  [{attribute, Line, export, [Export|Exports]}|Forms];
add_export([Form|Forms], Export) ->
  [Form|add_export(Forms, Export)];
add_export([], _Export) ->
  [].

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Disable cover to test...

%% all_test() ->
%%   {'EXIT', {undef, _}} = (catch test_mod:unexported()),
%%   ok                   = all(test_mod),
%%   ok                   = test_mod:unexported(),
%%   true                 = code:soft_purge(test_mod),
%%   {module, test_mod}   = code:load_file(test_mod),
%%   {'EXIT', {undef, _}} = (catch test_mod:unexported()).

%% call_test() ->
%%   {'EXIT', {undef, _}} = (catch test_mod:unexported()),
%%   ok                   = call(test_mod, unexported, []),
%%   {'EXIT', {undef, _}} = (catch test_mod:unexported()).

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
