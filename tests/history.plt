:- begin_tests(history_tests).
:- use_module('../src/history.pl').

%=============================================================================================================================================%
%===  execute_init_history  ==================================================================================================================%
%=============================================================================================================================================%

board_execute_init_history_result([
    [b_rook, b_knight, b_bishop, b_queen, b_king,  b_bishop, b_knight, b_rook],
    [b_pawn, b_pawn,   b_pawn,   b_pawn,  empty,   b_pawn,   b_pawn,   b_pawn],
    [empty,  empty,    empty,    empty,   b_pawn,  empty,    empty,    empty ],
    [empty,  empty,    empty,    empty,   empty,   empty,    empty,    empty ],
    [empty,  empty,    empty,    w_pawn,  empty,   empty,    empty,    empty ],
    [empty,  empty,    empty,    empty,   empty,   empty,    empty,    empty ],
    [w_pawn, w_pawn,   w_pawn,   w_queen, w_pawn,  w_pawn,   w_pawn,   w_pawn],
    [w_rook, w_knight, w_bishop, empty,   w_king,  w_bishop, w_knight, w_rook] 
]).

test(execute_init_history) :-
    board_execute_init_history_result(EndBoard),
    History = [
        move([piece(pawn), position((4,4))]),
        move([piece(pawn), position((5,6))]),
        move([piece(queen), position((4,2))])
    ],
    execute_init_history(History, ResultBoard, LastMove, Turn),
    assertion(ResultBoard == EndBoard),
    assertion(LastMove == move([piece(queen), position((4,2))])),
    assertion(Turn == black).

%=============================================================================================================================================%
%===  has_not_moved_yet  =====================================================================================================================%
%=============================================================================================================================================%

test(has_not_moved_yet_true) :-
    History = [
        move([piece(pawn), position((2,3))])
    ],
    has_not_moved_yet(History, (1,2)).

test(has_not_moved_yet_false) :-
    History = [
        move([piece(pawn), position((2,3))])
    ],
    \+ has_not_moved_yet(History, (2,2)).

test(has_not_moved_yet_castling_true) :-
    History = [
        move([piece(pawn), position((5,3))]),
        move([piece(pawn), position((1,6))]),
        move([piece(knight), position((8,3))]),
        move([piece(pawn), position((2,6))]),
        move([piece(bishop), position((5,2))]),
        move([piece(pawn), position((4,6))]),
        move([castling(short)])
    ],
    has_not_moved_yet(History, (2,2)).

test(has_not_moved_yet_castling_king_false) :-
    History = [
        move([piece(pawn), position((5,3))]),
        move([piece(pawn), position((1,6))]),
        move([piece(knight), position((8,3))]),
        move([piece(pawn), position((2,6))]),
        move([piece(bishop), position((5,2))]),
        move([piece(pawn), position((4,6))]),
        move([castling(short)]),
        move([piece(pawn), position((5,6))])
    ],
    \+ has_not_moved_yet(History, (5,1)).

test(has_not_moved_yet_castling_rook_false) :-
    History = [
        move([piece(pawn), position((5,2))]),
        move([piece(pawn), position((1,6))]),
        move([piece(knight), position((8,3))]),
        move([piece(pawn), position((2,6))]),
        move([piece(bishop), position((5,2))]),
        move([piece(pawn), position((4,6))]),
        move([castling(short)]),
        move([piece(pawn), position((5,6))])
    ],
    \+ has_not_moved_yet(History, (8,1)).

:- end_tests(history_tests).
