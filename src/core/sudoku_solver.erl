-module (sudoku_solver).
-export([run/1, update/3, generate_board/1]).

run(Board) ->
    Self = self(),
    spawn(fun() -> run_workers(Board, Self) end ),
    receive 
        {solved, SolvedBoard} -> {ok, SolvedBoard};
        {invalid} -> {invalid, Board};
        {error} -> {error, Board}
    end.

update(Board, {X, Y}, NewValue) ->
  board:update(Board, {X, Y}, NewValue).

generate_board(EmptyCellsAmount) ->
  board_generator:solvable_board(EmptyCellsAmount).
   
run_workers(Board, Parent) ->
    Tasks = lists:seq(1, 12),
    Self = self(),
    Pids = [ spawn(fun() -> scan_and_solve(Self, Board, assign_solver(X)) end ) || X <- Tasks],
    receive 
        {solved, SolvedBoard} -> Parent ! {solved, SolvedBoard},
                                      terminate_workers(Pids);
        {invalid} -> Parent ! {invalid}
    after 5000 -> Parent ! {error}
    end.

assign_solver(X) ->
    case X rem 2 of 
        0 -> simple_solver;
        1 -> backtracking_solver 
    end.

scan_and_solve(From, Board, Solver) ->
    case Solver:solve(scanner:run(Board)) of
        {ok, Solved} -> From ! {solved, Solved};
        {invalid_board} -> From ! {invalid}
    end.

terminate_workers([]) -> ok;
terminate_workers([Pid | Rest]) ->
    exit(Pid, kill),
    terminate_workers(Rest).