%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Filesystem.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_fs).

%%%_* Exports ==========================================================
-export([ read/1
        , write/2
        ]).

-export([ with_fd/2
        , with_fds/2
        ]).

-export([ with_temp_fd/1
        , with_temp_fd/2
        , with_temp_fds/2
        ]).

-export([ with_temp_file/1
        , with_temp_file/2
        , with_temp_files/2
        ]).

-export([ with_temp_dir/1
        , with_temp_dir/2
        , with_temp_dirs/2
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * read/write ------------------------------------------------------
-spec read(file())     -> _ | undefined.
read(File)             -> case file:read_file(File) of
                            {ok, Bin}       -> ?b2t(Bin);
                            {error, enoent} -> undefined
                          end.

-spec write(file(), _) -> _.
write(File, Term)      -> ok = file:write_file(File, ?t2b(Term)),
                          Term.

read_write_test() ->
  with_temp_file(fun(F) ->
    foo = write(F, foo),
    foo = read(F)
  end),
  undefined = read("nosuchfile").

%%%_ * with_fd ---------------------------------------------------------
-spec with_fd(file(), fun((fd()) -> A)) -> A.
%% @doc
with_fd(File, F) ->
  {ok, FD} = file:open(File, [read, write]),
  try F(FD)
  after file:close(FD)
  end.


-spec with_fds([file()], fun(([fd()]) -> A)) -> A.
%% @doc
with_fds(Files, F) -> s2_funs:unwind_with(fun with_fd/2, Files, F).


with_fds_ok_test() ->
  with_temp_files(2, fun([F1, F2]) ->
    ok =
      with_fds([F1, F2], fun([FD1, FD2]) ->
        file:write(FD1, <<"foo">>),
        file:write(FD2, <<"bar">>)
      end),
    [{ok, "foo"}, {ok, "bar"}] =
      with_fds([F1, F2], fun([FD1, FD2]) ->
        [ file:read(FD1, 3)
        , file:read(FD2, 3)
        ]
      end)
  end).

with_fds_error_test() ->
  with_temp_files(2, fun([F1, F2]) ->
    ok          = file:change_mode(F2, 8#00000),
    {'EXIT', _} = (catch with_fds([F1, F2], fun(_) -> ok end))
  end).

%%%_ * with_temp_fd ----------------------------------------------------
-spec with_temp_fd(fun(({file(), fd()}) -> A)) -> A.
%% @doc
with_temp_fd(F) ->
  with_temp_fd("with_temp_fd", F).
with_temp_fd(Prefix, F) ->
  File     = s2_sh:mktemp_u(Prefix),
  {ok, FD} = file:open(File, [read, write, exclusive]),
  try F({File, FD})
  after file:close(FD), file:delete(File)
  end.

with_temp_fd_test() ->
  ok = with_temp_fd(fun({_, FD}) ->
    file:write(FD, "foo")
  end).


-spec with_temp_fds(pos_integer() | [file()],
                    fun(([{file(), fd()}]) -> A)) -> A.
%% @doc
with_temp_fds(N, F) when is_integer(N) ->
  with_temp_fds(lists:duplicate(N, "with_temp_fds"), F);
with_temp_fds(Prefixes, F) when is_list(Prefixes) ->
  s2_funs:unwind_with(fun with_temp_fd/2, Prefixes, F).


with_temp_fds_ok_test() ->
  with_temp_fds(2, fun([{_, FD1}, {_, FD2}]) ->
    ok = file:write(FD1, "foo"),
    ok = file:write(FD2, "bar")
  end).

with_temp_fds_error_test() ->
  {F1, F2} =
    (catch with_temp_fds(2, fun([{F1, _}, {F2, _}]) ->
             throw({F1, F2})
           end)),
  false = filelib:is_file(F1),
  false = filelib:is_file(F2).

%%%_ * with_temp_file --------------------------------------------------
-spec with_temp_file(fun((file()) -> A)) -> A.
%% @doc
with_temp_file(F) ->
  with_temp_file("with_temp_file", F).
with_temp_file(Prefix, F) ->
  File = s2_sh:mktemp(Prefix),
  try F(File)
  after file:delete(File)
  end.


-spec with_temp_files([file()], fun(([file()]) -> A)) -> A.
%% @doc
with_temp_files(N, F) when is_integer(N) ->
  with_temp_files(lists:duplicate(N, "with_temp_files"), F);
with_temp_files(Prefixes, F) when is_list(Prefixes) ->
  s2_funs:unwind_with(fun with_temp_file/2, Prefixes, F).


with_temp_files_ok_test() ->
  with_temp_files(2, fun([F1, F2]) ->
    {ok, _} = file:open(F1, [read]),
    {ok, _} = file:open(F2, [read])
  end).

with_temp_files_error_test() ->
  {F1, F2} =
    (catch with_temp_files(2, fun([F1, F2]) -> throw({F1, F2}) end)),
  false = filelib:is_file(F1),
  false = filelib:is_file(F2).

%%%_ * with_temp_dir ---------------------------------------------------
-spec with_temp_dir(fun((file()) -> A)) -> A.
%% @doc
with_temp_dir(F) ->
  with_temp_dir("with_temp_dir", F).
with_temp_dir(Prefix, F) ->
  File = s2_sh:mktemp_d(Prefix),
  try F(File)
  after s2_sh:rm_rf(File)
  end.

-spec with_temp_dirs([file()], fun(([file()]) -> A)) -> A.
%% @doc
with_temp_dirs(N, F) when is_integer(N) ->
  with_temp_dirs(lists:duplicate(N, "with_temp_dirs"), F);
with_temp_dirs(Prefixes, F) when is_list(Prefixes) ->
  s2_funs:unwind_with(fun with_temp_dir/2, Prefixes, F).


with_temp_dirs_ok_test() ->
  with_temp_dirs(2, fun([F1, F2]) ->
    true = filelib:is_dir(F1),
    true = filelib:is_dir(F2)
  end).

with_temp_dirs_error_test() ->
  {F1, F2} =
    (catch with_temp_dirs(2, fun([F1, F2]) -> throw({F1, F2}) end)),
  false = filelib:is_dir(F1),
  false = filelib:is_dir(F2).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
