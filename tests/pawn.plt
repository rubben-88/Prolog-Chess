:- begin_tests(pawn_tests).
:- use_module('../src/pawn.pl').

%=============================================================================================================================================%
%===  pawn_available_position  ===============================================================================================================%
%=============================================================================================================================================%

board_pawn_available_position([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    w_pawn  ],
    [empty,    empty,    b_pawn,   b_pawn,   b_pawn,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_king,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_king,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    w_king,   empty,    b_king,   w_king,   b_king,   w_pawn  ],
    [w_pawn,   w_pawn,   w_pawn,   empty,    empty,    w_pawn,   w_pawn,   empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(pawn_available_position_no_blocking) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (1,2), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(1,3),(1,4)]).

test(pawn_available_position_blocking_two_steps) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (2,2), X), Xs),
    assertion(Xs == [(2,3)]).
    
test(pawn_available_position_blocking_one_step) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (3,2), X), Xs),
    assertion(Xs == []).

test(pawn_available_position_take) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (6,2), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(5,3),(7,3)]).

test(pawn_available_position_no_takeable) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (7,2), X), Xs),
    assertion(Xs == []).

test(pawn_available_position_not_on_init_row) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (8,3), X), Xs),
    assertion(Xs == [(8,4)]).

test(pawn_available_position_out_of_range) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, white, (8,8), X), Xs),
    assertion(Xs == []).

test(pawn_available_position_generate_begin_pos) :-
    board_pawn_available_position(Board),
    findall(X, pawn_available_position(Board, black, X, (4,6)), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(3,7),(5,7)]).

%=============================================================================================================================================%
%===  en_passant  ============================================================================================================================%
%=============================================================================================================================================%

test(en_passant_white_begin_pos_given) :-
    History = [
        % ...
        move([piece(pawn), position((5,4))]),
        move([piece(pawn), position((8,6))]),
        move([piece(pawn), position((5,5))]),
        move([piece(pawn), position((6,5))])
    ],
    en_passant(History, white, (5,5), X),
    assertion(X == (6,6)).

test(en_passant_black_begin_pos_given) :-
    History = [
        % ...
        move([piece(pawn), position((1,3))]), % always start with white
        move([piece(pawn), position((6,5))]),
        move([piece(pawn), position((8,3))]),
        move([piece(pawn), position((6,4))]),
        move([piece(pawn), position((5,4))])
    ],
    en_passant(History, black, (6,4), X),
    assertion(X == (5,3)).

test(en_passant_white_end_pos_given) :-
    History = [
        % ...
        move([piece(pawn), position((5,4))]),
        move([piece(pawn), position((8,6))]),
        move([piece(pawn), position((5,5))]),
        move([piece(pawn), position((6,5))])
    ],
    en_passant(History, white, X, (6,6)),
    assertion(X == (5,5)).

test(en_passant_black_end_pos_given) :-
    History = [
        % ...
        move([piece(pawn), position((1,3))]), % always start with white
        move([piece(pawn), position((6,5))]),
        move([piece(pawn), position((8,3))]),
        move([piece(pawn), position((6,4))]),
        move([piece(pawn), position((5,4))])
    ],
    en_passant(History, black, X, (5,3)),
    assertion(X == (6,4)).

:- end_tests(pawn_tests).