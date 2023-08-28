-module (board).
-compile(export_all).
-define(PossibleValues, [0,1,2,3,4,5,6,7,8,9]).
-define(EmptyBoard, [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]).

empty_board() -> ?EmptyBoard.

board_correct(Board) ->
    LastCol = length(lists:nth(1, Board)) - 1,
    ColsCoords = [col_coordinates(Board, {X, 0}) || X <- lists:seq(0, LastCol)],
    BoxesCoords = [box_coordinates({X, Y}) || {X, Y} <- boxes_start_coordinates(Board)],
    ColsVals = [lists:map(fun({X, Y}) -> board_at(Board, {X, Y}) end, Coords) || Coords <- ColsCoords],
    BoxesVals = [lists:map(fun({X, Y}) -> board_at(Board, {X, Y}) end, Coords) || Coords <- BoxesCoords],
    FilteredVals = lists:map(fun(Values) -> remove_zeroes(Values) end, [ColsVals, BoxesVals, Board]),
    lists:all(fun(Status) -> Status == true end, lists:map(fun(Values) -> is_group_valid(Values) end, FilteredVals)).

check_if_full(Board) ->
    Cells = lists:flatten(Board),
    case lists:all(fun(Cell) -> Cell > 0 end, Cells) of
        true -> {full, Board};
        _ -> {not_full, Board}
    end.

all_allowed_values(Board) ->
    LastRow = length(Board) - 1,
    LastCol = length(lists:nth(1, Board)) - 1,
    [{{X, Y}, Values} || X <- lists:seq(0, LastCol), Y <- lists:seq(0, LastRow), board_at(Board, {X, Y}) == 0,  
        Values <- case possible_for_cell(Board, {X, Y}) of
        {ok, Values} -> [Values];
        {error, _} -> []
    end].

next_possible_value(Board, {Col, Row}) ->
    case possible_for_cell(Board, {Col, Row}) of
      {ok, Values} ->
        RandomIndex = rand:uniform(length(Values)),
        {ok, lists:nth(RandomIndex, Values)};

      {error, _} -> {error, nil}
    end.

possible_for_cell(Board, Cell) ->
    ForRow = possible_in_row(Board, Cell),
    ForCol = possible_in_col(Board, Cell),
    ForBox = possible_in_box(Board, Cell),
    Values = lists:filter(fun(X) -> lists:member(X, ForCol) andalso lists:member(X, ForBox) end, ForRow),
    case Values of
        [] -> {error, no_values};
        _ -> {ok, Values}
    end.


update(Board, {X, Y}, Value) when Value >= 0, Value =< 9 ->
    Row = lists:nth(Y + 1, Board),
    UpdatedRow = setnth(X, Row, Value),
    setnth(Y, Board, UpdatedRow);

update(Board, _, _) ->
    Board.

remove_cells(Board, CellsToDelete) ->
    lists:map(
        fun({Y, Row}) -> 
            lists:map(
                fun({X, CellValue}) -> 
                    case lists:member({X, Y}, CellsToDelete) of
                    true -> 0;
                    _ -> CellValue
                    end 
                end, 
                lists:enumerate(0, Row)
            ) 
        end, 
    lists:enumerate(0, Board)).


empty_cells(Board) ->
    LastRow = length(Board) - 1,
    LastCol = length(lists:nth(1, Board)) - 1,
    [{X, Y} || X <- lists:seq(0, LastCol), Y <- lists:seq(0, LastRow), board_at(Board, {X, Y}) == 0].

possible_in_row(Board, {_X, Y}) ->
    LastRow = length(lists:nth(Y+1, Board)) - 1,
    RowValues = [board_at(Board, {CellX, Y}) || CellX <- lists:seq(0, LastRow)],
    ?PossibleValues -- RowValues.

possible_in_col(Board, {X, _Y}) ->
    LastCol = length(Board) - 1,
    ColValues = [board_at(Board, {X, CellY}) || CellY <- lists:seq(0, LastCol)],
    ?PossibleValues  -- ColValues.

possible_in_box(Board, {X, Y}) ->
    CellsToCheck = box_coordinates({X, Y}),
    ValuesInBox = [board_at(Board, {CellX, CellY}) || {CellX, CellY} <- CellsToCheck],
    ?PossibleValues -- ValuesInBox.

box_coordinates({X, Y}) ->
    StartX = (X div 3) * 3,
    StartY = (Y div 3) * 3,
    [{OffsetX + StartX, OffsetY + StartY} || OffsetX <- [0, 1, 2], OffsetY <- [0, 1, 2]].
            
boxes_start_coordinates(Board) ->
    case length(Board) div 3 of
        1 -> [{0, 0}];
        2 -> [{0, 0}, {3, 0}, {0, 3}, {3, 3}];
        3 -> [{0, 0}, {3, 0}, {6, 0}, {0, 3}, {3, 3}, {6, 3}, {0, 6}, {3, 6}, {6, 6}]
    end.
    
row_coordinates(Board, {_X, Y}) ->
    LastCol = length(Board) - 1,
    [{X, Y} || X <- lists:seq(0, LastCol)].
  
col_coordinates(Board, {X, _Y}) ->
    LastRow = length(lists:nth(1, Board)) - 1,
    [{X, Y} || Y <- lists:seq(0, LastRow)].

remove_zeroes(NestedList) ->
    lists:map(fun(List) -> lists:filter(fun(X) -> X /= 0 end, List) end, NestedList).  

is_group_valid(NestedList) ->
    lists:all(fun(List) -> List == lists:uniq(List) end, NestedList).

board_at(Board, {X, Y}) ->
  lists:nth(X+1, lists:nth(Y+1, Board)).

setnth(0, [_|Rest], NewElement) -> [NewElement|Rest];
setnth(Index, [Element|Rest], NewElement) -> [Element|setnth(Index-1, Rest, NewElement)].