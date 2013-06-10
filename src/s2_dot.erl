%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Generate GraphViz input.
%%% http://graphs.grevian.org/reference.html
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(s2_dot).

%%%_* Exports ==========================================================
-export([ digraph/2
        ]).

%%%_* Includes =========================================================
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib2/include/prelude.hrl").

%%%_* Code =============================================================
%%%_ * Types -----------------------------------------------------------
-type graph()   :: {Name::string(), [vertice()], [edge()]}.
-type vertice() :: {Name::string(), Label::string()}.
-type edge()    :: {Vertex1::string(), Vertex2::string(), Label::string()}.

%%%_ * API -------------------------------------------------------------
-spec digraph(graph(), string()) -> ok.
digraph({Name, Vertices, Edges}, Filename) ->
  true = ?is_string(Name),
  [true = ?is_string(Name) andalso
          ?is_string(Label) || {Name, Label} <- Vertices],
  [true = ?is_string(Vertex1) andalso
          ?is_string(Vertex2) andalso
          ?is_string(Label) || {Vertex1, Vertex2, Label} <- Edges],
  ok = file:write_file(Filename, gen_digraph(Name, Vertices, Edges)).

%%%_ * Internals -------------------------------------------------------
gen_digraph(Name, Vertices, Edges) ->
  io_lib:format("digraph ~s {~n"         ++
                  gen_vertices(Vertices) ++
                  gen_edges(Edges)       ++
                "}~n", [Name]).

gen_vertices(Vertices) ->
  lists:flatmap(fun gen_vertex/1, Vertices).

gen_vertex({Vertex, Label}) ->
  io_lib:format("~s [label=~p];~n", [Vertex, Label]).

gen_edges(Edges) ->
  lists:flatmap(fun gen_edge/1, Edges).

gen_edge({Vertex1, Vertex2, Label}) ->
    io_lib:format("~s -> ~s [label=~p];~n", [Vertex1, Vertex2, Label]).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
