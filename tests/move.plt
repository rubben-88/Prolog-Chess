:- begin_tests(move_tests).
:- use_module('../src/move.pl').

%=============================================================================================================================================%
%===  next_move  =============================================================================================================================%
%=============================================================================================================================================%

board_next_move([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [w_pawn,   empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [w_knight, empty,    empty,    empty,    empty,    empty,    empty,    empty   ] 
]).

test(next_move) :-
    board_next_move(Board),
    findall(X, next_move(classic, [], Board, white, X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [
        move([piece(knight),capture,position((2,3))]),
        move([piece(knight),position((3,2))]),
        move([piece(pawn),position((1,7))])
    ]).

board_next_move_king_checked([
    [b_rook, b_knight, b_bishop, empty,   b_king,  b_bishop, b_knight, b_rook  ],
    [b_pawn, b_pawn,   b_pawn,   b_pawn,  empty,   b_pawn,   b_pawn,   b_pawn  ],
    [empty,  empty,    empty,    empty,   b_pawn,  empty,    empty,    empty   ],
    [empty,  empty,    empty,    empty,   empty,   empty,    empty,    empty   ],
    [empty,  empty,    empty,    empty,   empty,   w_pawn,   empty,    b_queen ],
    [empty,  empty,    empty,    w_pawn,  empty,   empty,    empty,    empty   ],
    [w_pawn, w_pawn,   w_pawn,   empty,   w_pawn,  empty,    w_pawn,   w_pawn  ],
    [w_rook, w_knight, w_bishop, w_queen, w_king,  w_bishop, w_knight, w_rook  ] 
]).

history_next_move_king_checked([
    move([piece(pawn),position((6,4))]),
    move([piece(pawn),position((5,6))]),
    move([piece(pawn),position((4,3))]),
    move([piece(queen),position((8,4)),check(check)])
]).

test(next_move_king_checked) :-
    board_next_move_king_checked(Board),
    history_next_move_king_checked(History),
    findall(X, next_move(classic, History, Board, white, X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [
        move([piece(king),position((4,2))]),
        move([piece(pawn),position((7,3))])
    ]).

%=============================================================================================================================================%
%===  form_move  =============================================================================================================================%
%=============================================================================================================================================%

board_form_move([
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    b_king  ],
    [empty,    empty,    empty,    w_pawn,   empty,    w_rook,   empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    w_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    w_rook,   empty,    empty,    empty   ],
    [empty,    empty,    b_pawn,   empty,    empty,    empty,    empty,    empty   ],
    [w_rook,   w_rook,   w_rook,   w_rook,   empty,    empty,    empty,    empty   ] 
]).

test(form_move_no_special) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, w_rook, (1,1), (1,2), X), Xs),
    assertion(Xs == [move([piece(rook),position((1,2))])]).

test(form_move_check) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, w_rook, (6,7), (6,8), X), Xs),
    assertion(Xs == [move([piece(rook),position((6,8)),check(check)])]).

test(form_move_checkmate) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, w_rook, (2,1), (2,8), X), Xs),
    assertion(Xs == [move([piece(rook),position((2,8)),check(checkmate)])]).

test(form_move_capture) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, w_rook, (3,1), (3,2), X), Xs),
    assertion(Xs == [move([piece(rook),capture,position((3,2))])]).

test(form_move_promotion) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, w_pawn, (4,7), (4,8), X), Xs),
    sort(Xs, Sorted_Xs),
    assertion(Sorted_Xs == [
        move([piece(pawn),position((4,8)),promotion(knight)]),
        move([piece(pawn),position((4,8)),promotion(queen)])
    ]).

test(form_move_from_position_column) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, w_rook, (4,1), (4,3), X), Xs),
    assertion(Xs == [move([piece(rook),from_position(column(4)),position((4,3))])]).

test(form_move_en_passant) :-
    board_form_move(Board),
    findall(X, form_move(classic, Board, b_pawn, (8,4), (7,3), X), Xs),
    assertion(Xs == [move([piece(pawn),from_position(column(8)),capture,position((7,3))])]).

%=============================================================================================================================================%
%===  execute_move  ==========================================================================================================================%
%=============================================================================================================================================%

board_execute_move_before([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   b_pawn,   empty   ],
    [w_rook,   w_rook,   w_rook,   empty,    w_pawn,   empty,    empty,    empty   ] 
]).

board_execute_move_after_normal([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [w_rook,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   b_pawn,   empty   ],
    [empty,    w_rook,   w_rook,   empty,    w_pawn,   empty,    empty,    empty   ] 
]).

test(execute_move_normal) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_normal(AfterBoard),
    Move = move([piece(rook), position((1,4))]),
    execute_move(BeforeBoard, Move, white, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_capture([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    w_rook,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   b_pawn,   empty   ],
    [w_rook,   empty,   w_rook,    empty,    w_pawn,   empty,    empty,    empty   ] 
]).

test(execute_move_capture) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_capture(AfterBoard),
    Move = move([piece(rook), capture, position((2,4))]),
    execute_move(BeforeBoard, Move, white, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_from_position([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   w_rook,   empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   b_pawn,   empty   ],
    [w_rook,   w_rook,   empty,    empty,    w_pawn,   empty,    empty,    empty   ] 
]).

test(execute_move_from_position) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_from_position(AfterBoard),
    Move = move([piece(rook), from_position(row(1)), position((3,4))]),
    execute_move(BeforeBoard, Move, white, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_two_steps([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    w_pawn,   empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    b_pawn,   b_pawn,   empty   ],
    [w_rook,   w_rook,   w_rook,   empty,    w_pawn,   empty,    empty,    empty   ] 
]).

test(execute_move_pawn_two_steps) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_two_steps(AfterBoard),
    Move = move([piece(pawn), position((4,4))]),
    execute_move(BeforeBoard, Move, white, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_take([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    w_pawn,   b_pawn,   empty   ],
    [w_rook,   w_rook,   w_rook,   empty,    empty,    empty,    empty,    empty   ] 
]).

test(execute_move_pawn_take) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_take(AfterBoard),
    Move = move([piece(pawn), capture, position((6,2))]),
    execute_move(BeforeBoard, Move, white, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_promotion([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   empty,    empty   ],
    [w_rook,   w_rook,   w_rook,   empty,    w_pawn,   empty,    b_queen,  empty   ] 
]).

test(execute_move_promotion) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_promotion(AfterBoard),
    Move = move([piece(pawn), position((7,1)), promotion(queen)]),
    execute_move(BeforeBoard, Move, black, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_castling([
    [empty,    empty,    empty,    empty,    empty,    b_rook,   b_king,   empty   ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    empty,    empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [w_pawn,   b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   b_pawn,   empty   ],
    [w_rook,   w_rook,   w_rook,   empty,    w_pawn,   empty,    empty,    empty   ] 
]).

test(execute_move_castling) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_castling(AfterBoard),
    Move = move([castling(short)]),
    execute_move(BeforeBoard, Move, black, ResultBoard),
    assertion(ResultBoard == AfterBoard).

board_execute_move_after_en_passant([
    [empty,    empty,    empty,    empty,    b_king,   empty,    empty,    b_rook  ],
    [empty,    empty,    w_rook,   empty,    b_bishop, b_pawn,   b_pawn,   b_pawn  ],
    [empty,    w_pawn,   empty,    empty,    b_pawn,   b_knight, empty,    w_pawn  ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    b_pawn,   empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    empty,    empty,    empty,    empty,    empty   ],
    [empty,    empty,    empty,    w_pawn,   empty,    b_pawn,   b_pawn,   empty   ],
    [w_rook,   w_rook,   w_rook,   empty,    w_pawn,   empty,    empty,    empty   ] 
]).

test(execute_move_en_passant) :-
    board_execute_move_before(BeforeBoard),
    board_execute_move_after_en_passant(AfterBoard),
    Move = move([piece(pawn), from_position(column(1)), capture, position((2,6))]),
    execute_move(BeforeBoard, Move, white, ResultBoard),
    assertion(ResultBoard == AfterBoard).

:- end_tests(move_tests).