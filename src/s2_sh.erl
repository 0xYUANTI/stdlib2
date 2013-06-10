%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Erlang versions of common Unix shell commands.
%%% Errors are reported via exceptions. An attempt has been made to enable
%%% pipeline-style function composition.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_sh).

%%%_* Exports ==========================================================
%% Files
-export([ cp/2
        , ls/1
        , ls_l/1
        , mkdir/1
        , mkdir_p/1
        , mv/2
        , rm_rf/1
        , rmdir/1
        , touch/1
        ]).

%% mktemp(1)
-export([ mktemp/0
        , mktemp/1
        , mktemp/2
        , mktemp_d/0
        , mktemp_d/1
        , mktemp_d/2
        , mktemp_u/0
        , mktemp_u/1
        , mktemp_u/2
        ]).

%% Misc
-export([ eval/1
        , eval/2
        , host/1
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * Files -----------------------------------------------------------
-spec cp(file(), file()) -> file().
cp(Src, Dest0)           -> Dest    = unix2erl(Src, Dest0),
                            {ok, _} = file:copy(Src, Dest),
                            Dest.

cp_test()                -> rm_rf(s2_fs:with_temp_file(
                              fun(F) -> cp(F, F ++ ".2") end)).


-spec ls(file())         -> [file()].
ls(Dir)                  -> {ok, Files} = file:list_dir(Dir), Files.

ls_test()                -> [_|_] = ls("/tmp").


-spec ls_l(file())       -> [file()].
ls_l(Dir)                -> [filename:join(Dir, File) || File <- ls(Dir)].

ls_l_test()              -> ["/tmp/" ++ _|_] = ls_l("/tmp").


-spec mkdir(file())      -> file().
mkdir(Dir)               -> ok = file:make_dir(Dir), Dir.

-spec mkdir_p(file())    -> file().
mkdir_p(Dir)             -> ok = filelib:ensure_dir(Dir),
                            filelib:is_dir(Dir) orelse (ok=file:make_dir(Dir)),
                            Dir.

mkdir_test()             -> rmdir(mkdir(mktemp_u())).
mkdir_p_test()           -> D1 = mktemp_u(),
                            D2 = mktemp_u(),
                            mkdir_p(D1 ++ D2),
                            rm_rf(D1).


-spec mv(file(), file()) -> file().
mv(Old, New0)            -> New = unix2erl(Old, New0),
                            ok = file:rename(Old, New),
                            New.

mv_test()                -> rm_rf(s2_fs:with_temp_file(
                              fun(F) -> mv(F, F ++ ".2") end)).


-spec rm_rf(file())      -> file().
rm_rf(Path)              -> rm_rf(filelib:is_dir(Path), Path), Path.
rm_rf(true, Path)        -> [rm_rf(filename:join(Path, F)) || F <- ls(Path)],
                            ok = file:del_dir(Path);
rm_rf(false, Path)       -> case file:delete(Path) of
                              ok              -> ok;
                              {error, enoent} -> ok
                            end.

rm_rf_test()             -> rm_rf("nosuchfile").


-spec rmdir(file())      -> file().
rmdir(Dir)               -> ok = file:del_dir(Dir), Dir.

rmdir_test()             -> rmdir(mktemp_d()).


-spec touch(file())      -> file().
touch(File)              -> ok = file:change_time(File, s2_time:datetime()),
                            File.

touch_test() ->
  true = s2_fs:with_temp_file(fun(F) ->
    F = touch(F),
    {ok, #file_info{mtime=Mtime1}} = file:read_file_info(F),
    timer:sleep(1000),
    F = touch(F),
    {ok, #file_info{mtime=Mtime2}} = file:read_file_info(F),
    Mtime1 < Mtime2
  end).

%%
%% Internal
%%
unix2erl(Src, Dest) ->
  case filelib:is_dir(Dest) of
    true  -> filename:join(Dest, filename:basename(Src));
    false -> Dest
  end.

%%%_ * mktemp(1) -------------------------------------------------------
-spec mktemp()         -> file().
mktemp()               -> mktemp("mktemp").
mktemp(Prefix)         -> mktemp(Prefix, "/tmp").
mktemp(Prefix, Dir)    -> File     = temp_name(Dir, Prefix),
                          {ok, FD} = file:open(File, [write, exclusive]),
                          ok       = file:close(FD),
                          File.

-spec mktemp_u()       -> string().
mktemp_u()             -> mktemp_u("mktemp_u").
mktemp_u(Prefix)       -> mktemp_u(Prefix, "/tmp").
mktemp_u(Prefix, Dir)  -> temp_name(Dir, Prefix).

-spec mktemp_d()       -> string().
mktemp_d()             -> mktemp_d("mktemp_d").
mktemp_d(Prefix)       -> mktemp_d(Prefix, "/tmp").
mktemp_d(Prefix, Dir0) -> Dir = temp_name(Dir0, Prefix),
                          ok  = file:make_dir(Dir),
                          Dir.

mktemp_test()          -> rm_rf("/tmp/mktemp" ++ _ = mktemp()),
                          rm_rf("/tmp/"       ++ _ = mktemp("", "/tmp")).
mktemp_u_test()        -> "/tmp/mktemp_u" ++ _ = mktemp_u().
mktemp_d_test()        -> rmdir(mktemp_d()).


temp_name(Dir, "")     -> temp_name(Dir ++ "/");
temp_name(Dir, Prefix) -> temp_name(filename:join(Dir, Prefix)).
temp_name(Stem)        -> Stem ++ ?i2l(crypto:rand_uniform(0, 1 bsl 127)).

temp_name_test()       -> "/tmp/prefix" ++ N = temp_name("/tmp", "prefix"),
                          ?l2i(N).

%%%_ * Misc ------------------------------------------------------------
eval(Fmt, Args) ->
  eval(lists:flatten(io_lib:format(Fmt, Args))).

-spec eval(string()) -> maybe(string(), {non_neg_integer(), string()}).
eval(Cmd) ->
  s2_fs:with_temp_file(fun(F) ->
    S0         = os:cmd(io_lib:format("~s > ~s 2>&1; echo $?", [Cmd, F])),
    {S, "\n"}  = string:to_integer(S0),
    {ok, Out0} = file:read_file(F),
    Out        = ?b2l(Out0),
    case S of
      0 -> {ok, Out};
      N -> {error, {N, Out}}
    end
  end).

eval_test() ->
  {ok, _}         = eval("ls /tmp"),
  {error, {_, _}} = eval("ls nosuchfile").


-spec host(string() | inet:ip_address()) -> inet:ip_address() | string().
host(Host) when ?is_string(Host) ->
  ?unlift(inet:getaddr(Host, inet));
host(IP) when is_tuple(IP) ->
  s2_lists:to_list((?unlift(inet:gethostbyaddr(IP)))#hostent.h_name).

host_test() ->
  {127, 0, 0, 1} = host("localhost"),
  "localhost"    = host({127, 0, 0, 1}).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
