:- begin_tests(pieces_tests).
:- use_module('../src/pieces.pl').

:- use_module(library(lists)).  % subset/2

%=============================================================================================================================================%
%===  relative_move_cell  ====================================================================================================================%
%=============================================================================================================================================%

test(relative_move_cell_no_pawns) :-
    \+ relative_move_cell(w_pawn, _).

test(relative_move_cell_white_rook) :-
    findall(X, relative_move_cell(w_rook, X), Xs),
    subset([[(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)]], Xs),
    length(Xs, 4).

test(relative_move_cell_black_queen) :-
    findall(X, relative_move_cell(b_queen, X), Xs),
    subset([[(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7)]], Xs),
    length(Xs, 8).

%=============================================================================================================================================%
%===  apply_cell  ============================================================================================================================%
%=============================================================================================================================================%

board_apply_cell([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    w_rook,   empty,    b_pawn,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(apply_cell) :-
    board_apply_cell(Board),
    findall(X, apply_cell(Board, (3,5), [(1,0),(2,0),(3,0),(4,0)], white, X), Xs),
    assertion(Xs == [(4,5),(5,5)]).

%=============================================================================================================================================%
%===  rel_abs_conv  ==========================================================================================================================%
%=============================================================================================================================================%

test(rel_abs_conv_get_abs_pos) :-
    rel_abs_conv((1,1),(2,2),X),
    X = (3,3).

test(rel_abs_conv_get_rel_transform) :-
    rel_abs_conv((1,1),X,(3,3)),
    X = (2,2).

test(rel_abs_conv_get_begin_pos) :-
    rel_abs_conv(X,(2,2),(3,3)),
    X = (1,1).

%=============================================================================================================================================%
%===  cell_rel_to_abs  =======================================================================================================================%
%=============================================================================================================================================%

test(cell_rel_to_abs) :-
    cell_rel_to_abs((1,1), [(1,1),(2,2),(3,3)], X),
    X = [(2,2),(3,3),(4,4)].

%=============================================================================================================================================%
%===  cell_until_blocked  ====================================================================================================================%
%=============================================================================================================================================%

board_cell_until_blocked([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    w_rook,   w_rook,   w_rook,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    w_pawn,    b_pawn,  empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(cell_until_blocked_not_blocked) :-
    board_cell_until_blocked(Board),
    cell_until_blocked(Board, [(2,4),(2,3),(2,2)], white, X),
    assertion(X == [(2,4),(2,3),(2,2)]).

test(cell_until_blocked_blocked_by_same_color) :-
    board_cell_until_blocked(Board),
    cell_until_blocked(Board, [(3,4),(3,3),(3,2)], white, X),
    assertion(X == [(3,4)]).

test(cell_until_blocked_blocked_by_other_color) :-
    board_cell_until_blocked(Board),
    cell_until_blocked(Board, [(4,4),(4,3),(4,2)], white, X),
    assertion(X == [(4,4),(4,3)]).

test(cell_until_blocked_blocked_out_of_range) :-
    board_cell_until_blocked(Board),
    cell_until_blocked(Board, [(2,6),(2,7),(2,8),(2,9),(2,10)], white, X),
    assertion(X == [(2,6),(2,7),(2,8)]).

:- end_tests(pieces_tests).