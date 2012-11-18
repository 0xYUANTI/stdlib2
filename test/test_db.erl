-module(test_db).
-behaviour(s2_gen_db).

-export([ do_init/1
        , do_insert/2
        , do_lookup/2
        , do_delete/2
        ]).

do_init(DB)                 -> {ok, DB}.
do_insert(DB, {Proc, up})   -> {ok, [{Proc, up}|DB]};
do_insert(DB, {Proc, down}) -> {ok, [{Proc, down}|DB]}.
do_lookup(DB, Proc)         -> s2_lists:assoc(DB, Proc).
do_delete(DB, Proc)         -> {ok, s2_lists:dissoc(DB, Proc)}.

%%% eof
