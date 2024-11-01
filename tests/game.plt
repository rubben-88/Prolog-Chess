:- begin_tests(game_tests).
:- use_module('../src/game.pl').

:- use_module('../src/board.pl').
:- use_module('../src/move.pl').

%=============================================================================================================================================%
%===  next_game_state  =======================================================================================================================%
%=============================================================================================================================================%

board_move_pawn([
    [b_rook, b_knight, b_bishop, b_queen, b_king, b_bishop, b_knight, b_rook],
    [b_pawn, b_pawn,   b_pawn,   b_pawn,  b_pawn, b_pawn,   b_pawn,   b_pawn],
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    w_pawn,  empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
    [w_pawn, w_pawn,   w_pawn,   empty,   w_pawn, w_pawn,   w_pawn,   w_pawn],
    [w_rook, w_knight, w_bishop, w_queen, w_king, w_bishop, w_knight, w_rook] 
]).

test(next_game_state) :-
    board:init_board(Board1),
    GameBefore = game([
        mode(classic),
        result(unfinished),
        turn(white),
        history([]),
        board(Board1)
    ]),
    board_move_pawn(Board2),
    GamePossibility = game([
        mode(classic),
        result(unfinished),
        turn(black),
        history([move([piece(pawn),position((4,4))])]),
        board(Board2)
    ]),
    findall(X, next_game_state(GameBefore, X), Xs),
    length(Xs, N),
    assertion(N == 20),
    member(GamePossibility, Xs), !.

board_castling_before([
    [b_rook, b_knight, b_bishop, b_queen,  b_king, empty,    empty,    b_rook],
    [b_pawn, b_pawn,   b_pawn,   b_pawn,   empty,  b_pawn,   b_pawn,   b_pawn],
    [empty,  empty,    empty,    b_bishop, b_pawn, b_knight, empty,    empty ],
    [empty,  empty,    empty,    empty,    empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    empty,    empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    w_bishop, w_pawn, w_knight, empty,    empty ],
    [w_pawn, w_pawn,   w_pawn,   w_pawn,   empty,  w_pawn,   w_pawn,   w_pawn],
    [w_rook, w_knight, w_bishop, w_queen,  w_king, empty,    empty,    w_rook] 
]).
board_castling_after([
    [b_rook, b_knight, b_bishop, b_queen,  b_king, empty,    empty,    b_rook],
    [b_pawn, b_pawn,   b_pawn,   b_pawn,   empty,  b_pawn,   b_pawn,   b_pawn],
    [empty,  empty,    empty,    b_bishop, b_pawn, b_knight, empty,    empty ],
    [empty,  empty,    empty,    empty,    empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    empty,    empty,  empty,    empty,    empty ],
    [empty,  empty,    empty,    w_bishop, w_pawn, w_knight, empty,    empty ],
    [w_pawn, w_pawn,   w_pawn,   w_pawn,   empty,  w_pawn,   w_pawn,   w_pawn],
    [w_rook, w_knight, w_bishop, w_queen,  empty,  w_rook,   w_king,   empty ] 
]).

test(next_game_state_castling) :-

    board_castling_before(BoardBefore),
    GameBefore = game([
        mode(classic),
        result(unfinished),
        turn(white),
        history([
            move([piece(knight), position((6,3))]),
            move([piece(knight), position((6,6))]),
            move([piece(pawn), position((5,3))]),
            move([piece(pawn), position((5,6))]),
            move([piece(bishop), position((4,3))]),
            move([piece(bishop), position((4,6))])
        ]),
        board(BoardBefore)
    ]),

    board_castling_after(BoardAfter),
    GamePossibility = game([
        mode(classic),
        result(unfinished),
        turn(black),
        history([
            move([piece(knight), position((6,3))]),
            move([piece(knight), position((6,6))]),
            move([piece(pawn), position((5,3))]),
            move([piece(pawn), position((5,6))]),
            move([piece(bishop), position((4,3))]),
            move([piece(bishop), position((4,6))]),
            move([castling(short)])
        ]),
        board(BoardAfter)
    ]),

    findall(X, next_game_state(GameBefore, X), Xs),
    member(GamePossibility, Xs), !.

:- end_tests(game_tests).   