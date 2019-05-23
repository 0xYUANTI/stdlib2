%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc IO formatter
%%%      Similar to io_lib:format/2 with correct indentation but limits depths
%%%      of data structures by replacing ~p with ~P and ~w with ~W. It also
%%%      enforces a strict maximum length of the formatted message.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_io).

%%%_* Exports ==========================================================
%%%_ * API -------------------------------------------------------------
-export([format/2]).

%%%_* Defines ==========================================================
-define(P_DEPTH,   50).
-define(W_DEPTH,   50).
-define(MAX_LEN, 8192).

%%%_* Code =============================================================
-spec format(io:format(), [term()]) -> io_lib:chars().
format(Format, Args) ->
    format(Format, Args, ?P_DEPTH, ?W_DEPTH, ?MAX_LEN).

format(Format, Args, P_DEPTH, W_DEPTH, MAX_LEN) ->
    Chars =
        try lists:flatten(
                io_lib:build_text(
                    lists:map( fun(Elem) -> update(Elem, P_DEPTH, W_DEPTH) end
                             , io_lib:scan_format(Format, Args))))
        catch
            _:_ ->
                lists:flatten(
                    io_lib:format("FORMAT ERROR: ~p ~p", [Format, Args]))
        end,
    if
        length(Chars) =< MAX_LEN -> Chars;
        true                     -> lists:sublist(Chars, MAX_LEN - 3) ++ "..."
    end.

update(M = #{control_char := $p, args := [Arg]}, P_DEPTH, _) ->
    M#{control_char := $P, args := [Arg, P_DEPTH]};
update(M = #{control_char := $w, args := [Arg]}, _, W_DEPTH) ->
    M#{control_char := $W, args := [Arg, W_DEPTH]};
update(X, _, _) -> X.

%%%_* Tests ============================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

format_test() ->
    ?assertEqual( "Foo: bar"
              , format("Foo: ~p", [bar])),

    %% Depth limitation of binaries
    Alphabet = list_to_binary(lists:seq($a, $z)),
    ?assertEqual( "Too long: ..."
                , format("Too long: ~p", [Alphabet], 0, na, 100)),
    ?assertEqual( "Too long: <<...>>"
                , format("Too long: ~p", [Alphabet], 1, na, 100)),
    ?assertEqual( "Too long: <<\"abcd\"...>>"
                , format("Too long: ~p", [Alphabet], 2, na, 100)),
    ?assertEqual( "Too long: <<\"abcdefgh\"...>>"
                , format("Too long: ~p", [Alphabet], 3, na, 100)),
    ?assertEqual( "Too long: <<\"abcdefghijkl\"...>>"
                , format("Too long: ~p", [Alphabet], 4, na, 100)),

    %% No limitation of lists
    ?assertEqual( "Long list: \"abcdefghijklmnopqrstuvwxyz\""
                , format("Long list: ~p", [lists:seq($a, $z)], 2, na, 100)),

    %% Depth limitation of nested structures
    ?assertEqual( "Data: [...]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 1, na, 100)),
    ?assertEqual( "Data: [{...}|...]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 2, na, 100)),
    ?assertEqual( "Data: [{k1,...},{...}]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 3, na, 100)),
    ?assertEqual( "Data: [{k1,[...]},{k2,...}]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 4, na, 100)),
    ?assertEqual( "Data: [{k1,[{...}]},{k2,v2}]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 5, na, 100)),
    ?assertEqual( "Data: [{k1,[{nk1,...}]},{k2,v2}]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 6, na, 100)),
    ?assertEqual( "Data: [{k1,[{nk1,nv1}]},{k2,v2}]"
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 7, na, 100)),

    %% Limitation using ~w
    ?assertEqual( "Too long: <<97,98,99,100,...>>"
                , format("Too long: ~w", [Alphabet], na, 5, 100)),
    ?assertEqual( "Long list: [97,98,99,100|...]"
                , format("Long list: ~w", [lists:seq($a, $z)], na, 5, 100)),
    ?assertEqual( "Data: [{k1,[{...}]},{k2,v2}]"
                , format("Data: ~w", [[{k1, [{nk1, nv1}]}, {k2, v2}]], na, 5, 100)),

    %% Maximum length cutoff
    ?assertEqual( "Data: [{k1,[{nk1,nv1}]}..."
                , format("Data: ~p", [[{k1, [{nk1, nv1}]}, {k2, v2}]], 10, 10, 26)),

    %% Format error
    ?assertEqual( "FORMAT ERROR: \"Data: ~p\" wat"
                , format("Data: ~p", wat)),

    ok.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
