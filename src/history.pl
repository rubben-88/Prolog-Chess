:- module(history, [
    execute_init_history/4, % +History, -Board, -LastMove, -Turn
    has_not_moved_yet/2     % +History, +Beginpos
]).

:- use_module(board).
:- use_module(position).
:- use_module(move).

%=============================================================================================================================================%
%===  execute_init_history  ==================================================================================================================%
%=============================================================================================================================================%

execute_init_history(History, Board, LastMove, Turn) :-
    board:init_board(BeginBoard),
    eih(History, BeginBoard, white, Board, LastMove, Turn),
    !.

eih([], Board, Turn, Board, _, Turn).

eih([LastMove], BeginBoard, Turn, EndBoard, LastMove, NewTurn) :-
    move:execute_move(BeginBoard, LastMove, Turn, EndBoard),
    pieces:opposite_color(Turn, NewTurn).

eih([Move | History], BeginBoard, Turn, EndBoard, LastMove, NewTurn) :-
    move:execute_move(BeginBoard, Move, Turn, TmpBoard),
    pieces:opposite_color(Turn, OppositeTurn),
    eih(History, TmpBoard, OppositeTurn, EndBoard, LastMove, NewTurn).


%=============================================================================================================================================%
%===  has_not_moved_yet  =====================================================================================================================%
%=============================================================================================================================================%

has_not_moved_yet(History, BeginPos) :-
    board:init_board(Board),
    hnmy(History, BeginPos, Board, white),
    !.

% no more moves in History --> stop, success
hnmy([], _, _, _).

% castling
hnmy([Move | History], BeginPos, Board, Color) :-
    Move = move([castling(Type)]),
    king:castling_positions(Color, Type, KingPos, _, RookPos, _, _),
    (
        KingPos \= BeginPos, RookPos \= BeginPos
        ->
        move:execute_move(Board, Move, Color, NewBoard),
        pieces:opposite_color(Color, OppositeColor),
        hnmy(History, BeginPos, NewBoard, OppositeColor)
        ;
        !,      % red cut: make sure that on a fail we redo to the last case
        fail
    ).


% BeginPos was moved --> stop, fail
hnmy([Move | _], BeginPos, Board, Color) :-
    position:begin_position(Board, Move, Color, BeginPos),
    fail.

% BeginPos not moved --> check next
hnmy([Move | History], BeginPos, Board, Color) :-
    \+ position:begin_position(Board, Move, Color, BeginPos),
    move:execute_move(Board, Move, Color, NewBoard),
    pieces:opposite_color(Color, OppositeColor),
    hnmy(History, BeginPos, NewBoard, OppositeColor).
    