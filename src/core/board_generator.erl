-module (board_generator).
-export([solvable_board/1, full_board/0, get_random_cells/1]).

solvable_board(AmountOfZeroes) ->
    CellsToDelete = get_random_cells(AmountOfZeroes),
    board:remove_cells(full_board(), CellsToDelete).

full_board() ->
    full_board(board:empty_board(), 0, []).

full_board(_Board, _CellToFill, []) ->
 full_board(board:empty_board(), 0, shuffled_values({1, 9}));

 full_board(Board, CellToFill,  [Value | Rest]) ->
    case CellToFill of
      81 -> Board;
      _ ->
        Row = CellToFill div 9,
        Col = CellToFill rem 9,
        UpdatedBoard = board:update(Board, {Col, Row}, Value),

        IsRowOk = {check_ok} == check_row(Board, {Col, Row}, Value),
        IsColOk = {check_ok} == check_col(Board, {Col, Row}, Value),
        IsBoxOk = {check_ok} == check_box(Board, {Col, Row}, Value),

        if IsRowOk andalso IsColOk andalso IsBoxOk
            -> full_board(UpdatedBoard, CellToFill + 1, shuffled_values({1, 9}));
        true -> full_board(Board, CellToFill, Rest)
        end
    end.

check_row(Board, {_X, Y}, Value) ->
    RowValues = lists:nth(Y + 1, Board),

    case lists:member(Value, RowValues) of
      false -> {check_ok};
      _ -> {check_error, row}
    end.

check_col(Board, {X, _Y}, Value) ->
    ColValues = lists:map(fun(Row) -> lists:nth(X + 1, Row) end, Board),

    case lists:member(Value, ColValues) of
      false -> {check_ok};
      _ -> {check_error, col}
    end.

check_box(Board, {X, Y}, Value) ->
    StartY = (Y div 3) * 3,
    StartX = (X div 3) * 3,
    BoxValues = [lists:nth(OffsetX + StartX + 1, lists:nth(OffsetY + StartY + 1, Board)) || OffsetX <- [0, 1, 2], OffsetY <- [0, 1, 2]],

    case lists:member(Value, BoxValues) of
        false -> {check_ok};
        _ -> {check_error, box}
    end.

get_random_cells(Amount) -> get_random_cells(Amount, []).
get_random_cells(Amount, List) ->
    X = rand:uniform(9) - 1,
    Y = rand:uniform(9) - 1,
    UpdatedList = lists:uniq([{X, Y} | List]),

    if 
        Amount == 0 -> [];
        length(UpdatedList) < Amount -> get_random_cells(Amount, UpdatedList);
        true -> UpdatedList
    end.

shuffled_values({From, To}) ->
    AllValues = lists:seq(From, To),
    shuffled_values(AllValues, []).
    
shuffled_values([], List) ->
    List;

shuffled_values(ToAdd, List) ->
    Length = length(ToAdd),
    X = rand:uniform(Length),
    RandomVal = lists:nth(X, ToAdd),
    UpdatedList = ToAdd -- [RandomVal],
    shuffled_values(UpdatedList, [RandomVal | List]).
