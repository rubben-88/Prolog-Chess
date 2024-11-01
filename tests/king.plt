:- begin_tests(king_tests).
:- use_module('../src/king.pl').

%=============================================================================================================================================%
%===  is_check  ==============================================================================================================================%
%=============================================================================================================================================%

board_is_check([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    w_king,   empty,    empty,    b_rook,   empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_check_true) :-
    board_is_check(Board),
    is_check(Board, white).

test(is_check_false) :-
    board_is_check(Board),
    \+ is_check(Board, black).

%=============================================================================================================================================%
%===  is_checkmate  ==========================================================================================================================%
%=============================================================================================================================================%

board_is_checkmate_true([
    [w_king,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [b_rook,   b_rook,   empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_checkmate_true) :-
    board_is_checkmate_true(Board),
    is_checkmate(Board, white).

board_is_checkmate_true_cant_take_checker([
    [w_king,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_queen,  empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_rook,   empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_checkmate_true_cant_take_checker) :-
    board_is_checkmate_true_cant_take_checker(Board),
    is_checkmate(Board, white).

board_is_checkmate_false_not_check([
    [w_king,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    b_rook,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_rook,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_checkmate_false_not_check) :-
    board_is_checkmate_false_not_check(Board),
    \+ is_checkmate(Board, white).

board_is_checkmate_false_still_moveable([
    [w_king,   empty,    b_queen,  empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_checkmate_false_still_moveable) :-
    board_is_checkmate_false_still_moveable(Board),
    \+ is_checkmate(Board, white).

board_is_checkmate_false_take_checker([
    [w_king,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_queen,  empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_checkmate_false_take_checker) :-
    board_is_checkmate_false_take_checker(Board),
    \+ is_checkmate(Board, white).

board_is_checkmate_false_take_checker_2([
    [w_king,   empty,    empty,    b_rook,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    b_rook,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_rook,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_checkmate_false_take_checker_2) :-
    board_is_checkmate_false_take_checker_2(Board),
    \+ is_checkmate(Board, white).

%=============================================================================================================================================%
%===  is_king_center  ========================================================================================================================%
%=============================================================================================================================================%

board_is_king_center_true([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_king,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_king_center_true) :-
    board_is_king_center_true(Board),
    is_king_center(Board, white).

board_is_king_center_false([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    b_king,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(is_king_center_false) :-
    board_is_king_center_false(Board),
    \+ is_king_center(Board, black).

%=============================================================================================================================================%
%===  filtered_king_positions  =================================================================================================================%
%=============================================================================================================================================%

board_filtered_king_positions([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    b_king,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [w_king,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    b_rook,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(filtered_king_positions) :-
    board_filtered_king_positions(Board),
    findall(X, filtered_king_positions(Board, white, X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(1,5),(2,4)]).

%=============================================================================================================================================%
%===  castling  ==============================================================================================================================%
%=============================================================================================================================================%

board_castling_success([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    w_king,   empty,    empty,    w_rook  ] 
]).
history_castling_success([
    move([piece(pawn), position((5,3))]),
    move([piece(pawn), position((1,6))]),
    move([piece(knight), position((8,3))]),
    move([piece(pawn), position((2,6))]),
    move([piece(bishop), position((5,2))]),
    move([piece(pawn), position((3,6))])
]).

test(castling_success) :-
    board_castling_success(Board),
    history_castling_success(History),
    castling(History, Board, white, X),
    assertion(X == move([castling(short)])).

history_castling_king_already_moved([
    move([piece(pawn), position((5,3))]),
    move([piece(pawn), position((1,6))]),
    move([piece(knight), position((8,3))]),
    move([piece(pawn), position((2,6))]),
    move([piece(bishop), position((5,2))]),
    move([piece(pawn), position((3,6))]),
    move([piece(king), position((6,1))]),
    move([piece(pawn), position((4,6))]),
    move([piece(king), position((5,1))]),
    move([piece(pawn), position((5,6))])
]).

test(castling_king_already_moved) :-
    board_castling_success(Board),
    history_castling_king_already_moved(History),
    \+ castling(History, Board, white, _).

history_castling_rook_already_moved([
    move([piece(pawn), position((5,3))]),
    move([piece(pawn), position((1,6))]),
    move([piece(knight), position((8,3))]),
    move([piece(pawn), position((2,6))]),
    move([piece(bishop), position((5,2))]),
    move([piece(pawn), position((3,6))]),
    move([piece(rook), position((7,1))]),
    move([piece(pawn), position((4,6))]),
    move([piece(rook), position((8,1))]),
    move([piece(pawn), position((5,6))])
]).

test(castling_rook_already_moved) :-
    board_castling_success(Board),
    history_castling_rook_already_moved(History),
    \+ castling(History, Board, white, _).

board_castling_king_checked_before([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    b_rook,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    w_king,   empty,    empty,    w_rook  ] 
]).

test(castling_king_checked_before) :-
    board_castling_king_checked_before(Board),
    \+ castling([], Board, white, _).

board_castling_king_checked_after([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    b_rook,   empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    w_king,   empty,    empty,    w_rook  ] 
]).

test(castling_king_checked_after) :-
    board_castling_king_checked_after(Board),
    \+ castling([], Board, white, _).

board_castling_king_passes_dangerous_square([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    b_rook,   empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    w_king,   empty,    empty,    w_rook  ] 
]).

test(castling_king_passes_dangerous_square) :-
    board_castling_king_passes_dangerous_square(Board),
    \+ castling([], Board, white, _).

board_castling_piece_between([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    w_king,   w_bishop, empty,    w_rook  ] 
]).

test(castling_piece_between) :-
    board_castling_piece_between(Board),
    \+ castling([], Board, white, _).

:- end_tests(king_tests).
