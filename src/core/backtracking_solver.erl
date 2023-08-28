-module (backtracking_solver).
-compile(export_all).

solve(Board) ->
    case board:board_correct(Board) of
        true -> case board:check_if_full(Board) of
                    {full, _} -> {ok, Board};
                    {not_full, _} -> prepare_storage(Board),
                        {ok, find_solution(1)}
                end;
        false -> {invalid_board}
    end.

find_solution(Depth) ->
    get_possible_value(Depth).
      
get_possible_value(Depth) ->
    case next_to_check(Depth) of
        {ok, {Coordinates, Value}} -> get_possible_for_board(Coordinates, Depth, Value);
        {error, no_next_value} -> find_solution(choose_next_step(Depth))
    end.

get_possible_for_board(Coordinates, Depth, Value) -> 
    case board:possible_for_cell(get_from_storage(board), Coordinates) of
        {ok, PossibleForCell} -> check_if_valid(Depth, Coordinates, Value, PossibleForCell);
        {error, no_values} -> find_solution(go_back(Depth))
    end.

check_if_valid(Depth, Coordinates, Value, PossibleForCell) ->
    case lists:member(Value, PossibleForCell) of
        true -> update_storage(board, board:update(get_from_storage(board), Coordinates, Value)),
                        check_if_full_board(Value, Depth);
        false -> update_storage(checked_values, update_checked_values(Value, Depth)),
                                find_solution(choose_next_step(Depth))
    end.

check_if_full_board(Value, Depth) ->
    case board:check_if_full(get_from_storage(board)) of
        {not_full, _Board} -> update_storage(checked_values, update_checked_values(Value, Depth)),
                             find_solution(Depth + 1);
        {full, Board} -> Board
    end.

go_back(Depth) ->
    update_storage(checked_values, maps:update_with(Depth, fun(_X) -> [] end, [], get_from_storage(checked_values))),
    {Coordinates, _Val} = get_from_allowed(Depth, get_from_storage(allowed_values)),
    update_storage(board, board:update(get_from_storage(board), Coordinates, 0)),
    Depth - 1.

next_to_check(Depth) ->
    {CellCoordinates, AllowedDepthValues} = get_from_allowed(Depth, get_from_storage(allowed_values)),
    CheckedDepthValues = maps:get(Depth, get_from_storage(checked_values), []),
    ValuesToCheck = lists:filter(fun(X) -> not lists:member(X, CheckedDepthValues) end, AllowedDepthValues),
    case ValuesToCheck of
        [] -> {error, no_next_value};
        _ -> {ok, {CellCoordinates, lists:nth(1, ValuesToCheck)}}
    end.
  
update_checked_values(Value, Depth) ->
    AlreadyCheckedValues = maps:get(Depth, get_from_storage(checked_values), []),
    UpdatedCheckedValues = [Value | AlreadyCheckedValues],
    maps:put(Depth, UpdatedCheckedValues, get_from_storage(checked_values)).

choose_next_step(Depth) ->
    CheckedDepthValues = maps:get(Depth, get_from_storage(checked_values), []),
    {Coordinates, AllowedDepthValues} = get_from_allowed(Depth, get_from_storage(allowed_values)),

    case all_values_checked(AllowedDepthValues, CheckedDepthValues) of
        true ->
            update_storage(board, board:update(get_from_storage(board), Coordinates, 0)),
            update_storage(checked_values, maps:update_with(Depth, fun(_X) -> [] end,  [], get_from_storage(checked_values))),
            choose_next_step(Depth - 1);

        _ ->
            Depth
    end.

get_from_storage(Key) ->
    [{Key, Value}] = ets:lookup(?MODULE, Key),
    Value.
          
update_storage(Key, Value) ->
    ets:insert(?MODULE, {Key, Value}).

prepare_storage(Board) ->
    case ets:whereis(?MODULE) of
        undefined -> ets:new(?MODULE, [set, named_table, public]);
        _ -> ok
    end,
    Values = board:all_allowed_values(Board),
    ets:insert(?MODULE, {board, Board}),
    ets:insert(?MODULE, {allowed_values, Values}),
    ets:insert(?MODULE, {checked_values, #{}}).

all_values_checked(PossibleValues, CheckedValues) ->
    length(PossibleValues) == length(CheckedValues).

get_from_allowed(Depth, AllowedValues) ->
    case Depth > length(AllowedValues) of
        true -> {[], []};
        _ -> lists:nth(Depth, AllowedValues)    
    end.