-module(test_mod).
-export([]).
-compile({nowarn_unused_function, [{unexported, 0}]}).
unexported() -> ok.
