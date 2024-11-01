:- begin_tests(board_tests).
:- use_module('../src/board.pl').

:- use_module(library(lists)).  % subset/2

%=============================================================================================================================================%
%===  place  =================================================================================================================================%
%=============================================================================================================================================%

board_place_before([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).
board_place_after([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(place) :-
    board_place_before(BeforeBoard),
    board_place_after(AfterBoard),
    place(BeforeBoard, (4,4), w_pawn, ResultBoard),
    assertion(ResultBoard == AfterBoard).

%=============================================================================================================================================%
%===  remove  ================================================================================================================================%
%=============================================================================================================================================%

board_remove_before([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).
board_remove_after([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(remove) :-
    board_remove_before(BeforeBoard),
    board_remove_after(AfterBoard),
    remove(BeforeBoard, (4,4), ResultBoard),
    assertion(ResultBoard == AfterBoard).

%=============================================================================================================================================%
%===  go_to  =================================================================================================================================%
%=============================================================================================================================================%

board_go_to_before([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_rook,   empty,    b_pawn,   empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).
board_go_to_after([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    w_rook,   empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(go_to) :-
    board_go_to_before(BeforeBoard),
    board_go_to_after(AfterBoard),
    go_to(BeforeBoard, (4,4), (6,4), ResultBoard),
    assertion(ResultBoard == AfterBoard).

%=============================================================================================================================================%
%===  piece_position  ========================================================================================================================%
%=============================================================================================================================================%

board_piece_position([
    [b_pawn,   b_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    b_king,   empty   ],
    [empty,    w_knight, empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    b_rook,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    w_queen,  empty,    empty,    empty   ],
    [empty,    empty,    empty,    b_bishop, empty,    empty,    empty,    empty   ] 
]).

test(piece_at_position_pos_given) :-
    board_piece_position(Board),

    piece_at_position(Board, (1,8), A),
    assertion(A == b_pawn),

    piece_at_position(Board, (5,2), B),
    assertion(B == w_queen),

    piece_at_position(Board, (6,1), C),
    assertion(C == empty).

test(piece_at_position_piece_given) :-
    board_piece_position(Board),

    findall(A, piece_at_position(Board, A, b_pawn), As),
    sort(As, Sorted_As),
    assertion(Sorted_As == [(1,8),(2,8),(3,8)]),

    findall(B, piece_at_position(Board, B, empty), Bs),
    length(Bs, N_Bs),
    assertion(N_Bs == 56),

    findall(C, piece_at_position(Board, C, w_king), Cs),
    assertion(Cs == []).

%=============================================================================================================================================%
%===  board_position  ========================================================================================================================%
%=============================================================================================================================================%

test(board_position_confirm_middle) :-
    board_position((4,5)).

test(board_position_confirm_edges) :-
    board_position((1,1)),
    board_position((8,8)).

test(board_position_confirm_out_of_range) :-
    \+ board_position((-1,0)),
    \+ board_position((9,3)).

test(board_position_provide_full_position) :-
    findall(X, board_position(X), Xs),
    subset([(4,5),(1,1),(8,8)], Xs),
    length(Xs, 64).

test(board_position_provide_column) :-
    findall(X, board_position((1,X)), Xs),
    subset([1,4,8], Xs),
    length(Xs, 8).

test(board_position_provide_row) :-
    findall(X, board_position((X,1)), Xs),
    subset([1,4,8], Xs),
    length(Xs, 8).

%=============================================================================================================================================%
%===  dangerous_square  ======================================================================================================================%
%=============================================================================================================================================%

board_dangerous_square([
    [empty,    empty,    empty,    empty,    empty,    empty,    b_rook,   empty   ],
    [empty,    w_rook,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    w_king  ],
    [empty,    empty,    empty,    empty,    empty,    b_pawn,   empty,    empty   ],
    [empty,    empty,    w_pawn,   empty,    w_pawn,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(dangerous_square_confirm_dangerous) :-
    board_dangerous_square(Board),
    dangerous_square(Board, black, (1,7)).

test(dangerous_square_confirm_not_dangerous) :-
    board_dangerous_square(Board),
    \+ dangerous_square(Board, black, (1,8)).

test(dangerous_square_generate_dangerous) :-
    board_dangerous_square(Board),
    findall(X, dangerous_square(Board, black, X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [
        (1,7),
        (2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,8),
        (3,7),
        (4,7),
        (5,7),
        (6,4),(6,7),
        (7,4),(7,5),(7,6),(7,7),
        (8,4),(8,6),(8,7)
    ]).

board_dangerous_square_after_take([
    [w_king,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_queen,  empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_rook,   empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(dangerous_square_after_take) :-
    board_dangerous_square_after_take(Board),
    dangerous_square(Board, white, (2,7)).

:- end_tests(board_tests).
