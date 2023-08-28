-module (scanner).
-export([run/1]).

run(Board) ->
    CellsToScan = board:empty_cells(Board),
    UpdatedBoard = lists:foldl(fun(Cell, Acc) -> scan_cell(Acc, Cell) end, Board, CellsToScan),

    case Board /= UpdatedBoard of
      true -> run(UpdatedBoard);
      _ -> UpdatedBoard
    end.

scan_cell(Board, Cell) ->
    PossibleForCell = possible_for_cell(Board, Cell),
    PossibleForGroups = possible_for_groups(Board, Cell),
    Result = get_from_allowed(1, lists:uniq(lists:flatten(lists:map(fun(Values) -> PossibleForCell -- Values end, PossibleForGroups)))),
  
    case Result of
      nil -> Board;
      _ -> board:update(Board, Cell, Result)
    end.

possible_for_groups(Board, Cell) ->
    BoxToCheck = board:box_coordinates(Cell) -- [Cell],
    RowToCheck = board:row_coordinates(Board, Cell) -- [Cell],
    ColToCheck = board:col_coordinates(Board, Cell) -- [Cell],
    Col = get_values_for(Board, ColToCheck),
    Row = get_values_for(Board, RowToCheck),
    Box = get_values_for(Board, BoxToCheck),
    [Col, Row, Box].

get_values_for(Board, CellsToCheck) ->
    PossibleValues = [possible_for_cell(Board, {X, Y}) || {X, Y} <- CellsToCheck, board:board_at(Board, {X, Y}) == 0],
    lists:uniq(lists:flatten(PossibleValues)).

possible_for_cell(Board, Cell) ->
    RowPossible = board:possible_in_row(Board, Cell),
    ColPossible = board:possible_in_col(Board, Cell),
    BoxPossible = board:possible_in_box(Board, Cell),
    lists:filter(fun(X) -> lists:member(X, ColPossible) andalso lists:member(X, BoxPossible) end, RowPossible).

get_from_allowed(Index, List) ->
    case Index > length(List) of
        true -> {[]};
        _ -> lists:nth(Index, List)    
    end.