%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file="./priv/templates/bare.html" }.

title() -> "Welcome to Sudoku!".

body() ->
    add_listeners(),
    gen_board(),
    #main { class="container", body = [
        #panel {class="messages", body= [
            #p {id="result-msg"}
        ]},
        #panel { class="main", body = [
            #table {class="table", rows = [
                #tablerow {class=["row", "row" ++ integer_to_list(RowNum)], cells = [
                    #tablecell {id="cell" ++ integer_to_list(RowNum*9 + CellNum), class=["cell", "cell" ++ integer_to_list(CellNum)], body = [
                        #textbox {text="", maxlength="1",  class="cell_value"}
                    ]} || CellNum <- lists:seq(0, 8)]
                } || RowNum <- lists:seq(0, 8)]
            },
            #panel {class="side-panel", body= [
                #restful_form  {id="actions_form", class="gen-form", method=post, body = [
                    #label {class="amount-label", text="Amount of empty cells:"},
                    #textbox {maxlength="2", id="emptycells", class="amount", text="20"},
                    #button { class="button", text="Generate", postback=generate}
                ]},
                #panel {class="btn-container", body = [
                    #button {class="button", text="Solve", postback=solve},
                    #button {class="button", text="Reset", postback=reset}
                ]}
            ]}
        ]}
    ]}.
    
event(solve) ->
    Board = wf:session(board),
    {Result, SolvedBoard} = sudoku_solver:run(Board),
    render_message(Result),
    render_board(SolvedBoard);

event(reset) ->
    hide_message(),
    InitialBoard = wf:session(old_board),
    wf:session(board, InitialBoard),
    render_board(InitialBoard);
  
event(generate) ->
    hide_message(),
    EmptyCellsAmount = list_to_integer(wf:q(emptycells)),
    gen_board(EmptyCellsAmount);

event(empty_cells) ->
    io:format("Empty ells: ~p ~n", [wf:q(emptycells)]),
    case is_input_integer(wf:q(emptycells)) of
        false -> NewElement =  [ #label {class="amount-label", text="Amount of empty cells:"},
                                #textbox {maxlength="2", id="emptycells", class="amount", text="0"},
                                #button { class="button", text="Generate", postback=generate}],
                wf:update("actions_form", NewElement);
        true -> ok
    end;

event({new_value, []}) ->
    ok;

event({new_value, CellNumber}) ->
    hide_message(),
    Y = list_to_integer(CellNumber) div 9,
    X = list_to_integer(CellNumber) rem 9,
    OldBoard = wf:session(board),
    Val = wf:q(list_to_atom("cell"++CellNumber)),
    
    case is_input_integer(Val) orelse is_input_empty(Val) of
        false -> CellId = "cell" ++ CellNumber,
                NewElement = #textbox {text="", maxlength="1", class="cell_value"},
                wf:update(CellId, NewElement),
                Board = board:update(OldBoard, {X, Y}, 0),
                wf:session(board, Board);
        true -> Board = board:update(OldBoard, {X, Y}, input_to_integer(Val)),
                wf:session(board, Board)
    end.

gen_board() ->
    gen_board(20).

gen_board(EmptyCellsAmount) when EmptyCellsAmount < 82 ->
    NewBoard = board_generator:solvable_board(EmptyCellsAmount),
    wf:session(old_board, NewBoard),
    wf:session(board, NewBoard),
    render_board(NewBoard);

gen_board(_) ->
    gen_board(81).

render_board(Board) ->
    render_row(Board, 0).

render_row([Row|T], N) ->
    render_cell(Row, N),
    render_row(T, N + 1);

render_row([], _N) -> ok.

render_cell(Row, RowNum) ->
    render_cell(Row, RowNum, 0).

render_cell([Value|T], RowNum, CellNum) ->
    CellId = "cell" ++ integer_to_list(RowNum*9 + CellNum),
    NewElement = #textbox {text=hide_zero(Value), maxlength="1", class="cell_value"},
    wf:update(CellId, NewElement),
    render_cell(T, RowNum, CellNum + 1);

render_cell([], _N, _M) -> ok.

add_listeners() ->
    wf:wire("actions_form", #event { type=input, postback=empty_cells}),
    [wf:wire(list_to_atom("cell"++integer_to_list(X)), #event { type=input, postback={new_value, integer_to_list(X)} }) 
        || X <- lists:seq(0, 80)].
  
render_message(Type) ->
    MessageElement = case Type of
        ok -> #p {id="result-msg", text=message(Type), class="msg-positive"};
        error -> #p {id="result-msg", text=message(Type), class="msg-neutral"};
        invalid -> #p {id="result-msg", text=message(Type), class="msg-invalid"};
        invalid_value -> #p {id="result-msg", text=message(Type), class="msg-invalid"}
    end,
    wf:update("result-msg", MessageElement).

hide_message() ->
    MessageElement =  #p {id="result-msg", text=""},
    wf:update("result-msg", MessageElement).

is_input_integer("") ->
    false;
is_input_integer(Input) ->
    lists:all(fun (D) -> D >= $0 andalso D =< $9 end, Input).       

is_input_empty(Val) ->
    case string:trim(Val) of
        [] -> true;
        _ -> false
    end.

input_to_integer(Input) ->
    case string:trim(Input) of
        [] -> 0;
        _ -> list_to_integer(Input)
    end.

hide_zero(Value) ->
    case Value of
        0 -> "";
        _ -> Value
    end.

message(ok) ->
    "Sudoku solved!";

message(error) ->
    "Current sudoku is hard to solve. Try solving it again or generate a new one.";

message(invalid) ->
    "Cells are filled incorectly. Sudoku can't be solved.";

message(invalid_value) ->
    "Cell is filled incorrectly, you can use only numbers from 0 to 9.".