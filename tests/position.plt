:- begin_tests(position_tests).
:- use_module('../src/position.pl').

%=============================================================================================================================================%
%===  moveable_position  =====================================================================================================================%
%=============================================================================================================================================%

board_moveable_position([
    [w_rook,   w_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [w_pawn,   empty,    empty,    b_pawn,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    b_pawn,   empty,    empty,    empty   ],
    [w_rook,   w_pawn,   empty,    empty,    empty,    w_pawn,   b_pawn,   w_pawn  ],
    [w_pawn,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    b_rook,   empty,    empty,    empty   ],
    [w_rook,   w_pawn,   empty,    b_bishop, w_king,   empty,    empty,    empty   ] 
]).

test(moveable_position_no_moveable) :-
    board_moveable_position(Board),
    findall(X, moveable_position([], Board, (1,8), X), Xs),
    assertion(Xs == []).

test(moveable_position_single_moveable) :-
    board_moveable_position(Board),
    findall(X, moveable_position([], Board, (1,5), X), Xs),
    assertion(Xs == [(1,6)]).

test(moveable_position_multiple_moveable) :-
    board_moveable_position(Board),
    findall(X, moveable_position([], Board, (1,1), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(1,2),(1,3)]).

test(moveable_position_pawn_two_steps) :-
    board_moveable_position(Board),
    findall(X, moveable_position([], Board, (4,7), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(4,5),(4,6)]).

test(moveable_position_pawn_take) :-
    board_moveable_position(Board),
    findall(X, moveable_position([], Board, (5,6), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(5,5),(6,5)]).

test(moveable_position_pawn_en_passant) :-
    board_moveable_position(Board),
    History = [
        % ...
        move([piece(pawn), position((8,4))]),
        move([piece(pawn), position((1,6))]),
        move([piece(pawn), position((8,5))]),
        move([piece(pawn), position((7,5))])
    ],
    findall(X, moveable_position(History, Board, (8,5), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(7,6),(8,6)]).

test(moveable_position_king) :-
    board_moveable_position(Board),
    findall(X, moveable_position([], Board, (5,1), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [(4,1),(6,1)]).

board_moveable_position_king_no_moveable([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    w_rook,   empty   ],
    [empty,    empty,    empty,    empty,    empty,    w_rook,   empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    b_king  ] 
]).

test(moveable_position_king_no_moveable) :-
    board_moveable_position_king_no_moveable(Board),
    findall(X, moveable_position([], Board, (8,1), X), Xs),
    assertion(Xs == []).

%=============================================================================================================================================%
%===  begin_position  ========================================================================================================================%
%=============================================================================================================================================%

board_begin_position([
    [b_rook,   empty,    b_rook,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_rook,   empty,    empty,    b_pawn,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    w_pawn,   b_pawn,   empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [b_rook,   empty,    empty,    empty,    b_rook,   empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(begin_position_no_from_position) :-
    board_begin_position(Board),
    Move = move([
        piece(rook),
        position((2,6))
    ]),
    begin_position(Board, Move, black, X),
    assertion(X == (2,7)).

test(begin_position_column_provided) :-
    board_begin_position(Board),
    Move = move([
        piece(rook),
        from_position(column(1)),
        position((4,2))
    ]),
    begin_position(Board, Move, black, X),
    assertion(X == (1,2)).

test(begin_position_row_provided) :-
    board_begin_position(Board),
    Move = move([
        piece(rook),
        from_position(row(2)),
        position((1,4))
    ]),
    begin_position(Board, Move, black, X),
    assertion(X == (1,2)).

test(begin_position_both_provided) :-
    board_begin_position(Board),
    Move = move([
        piece(rook),
        from_position(both(2,7)),
        position((2,8))
    ]),
    begin_position(Board, Move, black, X),
    assertion(X == (2,7)).

test(begin_position_pawn) :-
    board_begin_position(Board),
    Move = move([
        piece(pawn),
        position((5,5))
    ]),
    begin_position(Board, Move, black, X),
    assertion(X == (5,7)).

test(begin_position_en_passant) :-
    board_begin_position(Board),
    Move = move([
        piece(pawn),
        from_position(column(7)),
        capture,
        position((6,3))
    ]),
    begin_position(Board, Move, black, X),
    assertion(X == (7,4)).

:- end_tests(position_tests).
