-module(sudoku_erlang_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sudoku_erlang_sup:start_link().

stop(_State) ->
    ok.
