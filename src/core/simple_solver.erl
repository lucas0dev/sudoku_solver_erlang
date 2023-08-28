-module (simple_solver).
-export([solve/1]).

solve(Board) ->
    case board:board_correct(Board) of
        true -> 
            case board:check_if_full(Board) of
                {not_full, _} -> solve(Board, 0, Board);
                {full, _} -> {ok, Board}
            end;
        false -> {invalid_board}
    end.

solve(Board, Step, InitialBoard) ->
    BoardCells = cells_coordinates(Board),
    {X, Y} = lists:nth(Step + 1, BoardCells),

    case board:board_at(Board, {X, Y}) == 0 of
        true -> 
                case board:next_possible_value(Board, {X, Y}) of 
                    {ok, Value} -> UpdatedBoard = board:update(Board, {X, Y}, Value),
                                   next_step(UpdatedBoard, Step ,InitialBoard);
                    _ -> solve(InitialBoard)
                end;
        false -> solve(Board, Step + 1, InitialBoard)
    end.

next_step(Board, Step, InitialBoard) ->
    case board:check_if_full(Board) of
        {not_full, _} -> solve(Board, Step + 1, InitialBoard);
        {full, _} -> {ok, Board}
    end.

cells_coordinates(Board) ->
    LastRow = length(lists:nth(1, Board)) - 1,
    LastCol = length(Board) - 1,
    XList = lists:seq(0, LastRow),
    YList = lists:seq(0, LastCol),
    [{X, Y} || X <- XList, Y <- YList].
