%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Extended IO-lists.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_iolists).

%%%_* Exports ==========================================================
-export([ cat/2
        , eq/2
        , new/1
        , safe_cat/2
        , safe_eq/2
        , safe_new/1
        , to_binary/1
        , to_string/1
        ]).

-export_type([ strble/0
             ]).

%%%_* Includes =========================================================
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%% ``stringable''
-type strble()                         :: atom()
                                        | binary()
                                        | string()
                                        | iolist().

%%-type iolist() :: maybe_improper_list(byte() | binary() | iolist(),
%%                                      binary() | []).

%% Unsafe API
-spec cat(strble(), strble())          -> iolist().
cat(X, Y) when is_atom(X)              -> cat(?a2l(X), Y);
cat(X, Y) when is_atom(Y)              -> cat(X, ?a2l(Y));
cat(X, Y)                              -> [X|Y]. %binary()|string()|iolist()

-spec new(strble())                    -> iolist().
new(X) when is_atom(X)                 -> ?a2l(X);
new(X) when is_binary(X)               -> [X];
new(X) when is_list(X)                 -> X. %string()|iolist()

-spec eq(strble(), strble())           -> boolean().
eq(X, Y)                               -> to_binary(new(X)) =:=
                                          to_binary(new(Y)).

-spec to_binary(iolist())              -> binary().
to_binary(Str)                         -> erlang:iolist_to_binary(Str).

-spec to_string(iolist())              -> string().
to_string(Str)                         -> ?b2l(to_binary(Str)).

%% Safe API
-spec safe_cat(strble(), strble())     -> iolist().
safe_cat(X, Y)                         -> [safe_new(X)|safe_new(Y)].

-spec safe_eq(strble(), strble())      -> boolean().
safe_eq(X, Y)                          -> to_binary(safe_new(X)) =:=
                                          to_binary(safe_new(Y)).

-spec safe_new(strble())               -> iolist().
safe_new(X) when is_atom(X)            -> ?a2l(X);
safe_new(X) when is_binary(X)          -> [X];
safe_new(X) when is_list(X)            -> do_safe_new(X).

do_safe_new([X|Xs]) when is_integer(X) -> [X|do_safe_new(Xs)];
do_safe_new([X|Xs]) when is_binary(X)  -> [X|do_safe_new(Xs)];
do_safe_new([X|Xs]) when is_list(X)    -> [do_safe_new(X)|do_safe_new(Xs)];
do_safe_new(X)      when is_binary(X)  -> X;
do_safe_new([])                        -> [].

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  Str = lists:foldl(fun cat/2,
                    "",
                    [ foo
                    , <<"bar">>
                    , "baz"
                    , [["quux", <<"blarg">>]|<<"snarf">>]
                    ]),
  "quuxblargsnarfbazbarfoo" = to_string(Str),
  ["foo"|"bar"] = cat(foo, bar),
  true = eq(foo, <<"foo">>),
  ok.

safe_test() ->
  _ = (catch safe_new([foo])),
  [] = new([]),
  Str = lists:foldl(fun safe_cat/2,
                    "",
                    [ foo
                    , <<"bar">>
                    , "baz"
                    , [["quux", <<"blarg">>]|<<"snarf">>]
                    ]),
  "quuxblargsnarfbazbarfoo" = to_string(Str),
  true = safe_eq("foo", "foo"),
  ok.

-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
