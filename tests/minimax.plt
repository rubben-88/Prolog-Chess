:- begin_tests(minimax_tests).
:- use_module('../src/minimax.pl').

:- use_module('../src/board.pl').

%=============================================================================================================================================%
%===  minimax_next_game_state  ===============================================================================================================%
%=============================================================================================================================================%

game_minimax_before(game([
    mode(classic),
    result(unfinished),
    turn(white),
    history([
        move([piece(knight),position((6,3))]),
        move([piece(pawn),position((5,5))])
    ]),
    board([
        [b_rook, b_knight, b_bishop, b_queen, b_king, b_bishop, b_knight, b_rook],
        [b_pawn, b_pawn,   b_pawn,   b_pawn,  empty,  b_pawn,   b_pawn,   b_pawn],
        [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   b_pawn, empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   empty,  w_knight, empty,    empty ],
        [w_pawn, w_pawn,   w_pawn,   w_pawn,  w_pawn, w_pawn,   w_pawn,   w_pawn],
        [w_rook, w_knight, w_bishop, w_queen, w_king, w_bishop, empty,    w_rook] 
    ])
])).

game_minimax_after(game([
    mode(classic),
    result(unfinished),
    turn(black),
    history([
        move([piece(knight),position((6,3))]),
        move([piece(pawn),position((5,5))]),
        move([piece(knight),capture,position((5,5))])
    ]),
    board([
        [b_rook, b_knight, b_bishop, b_queen, b_king,   b_bishop, b_knight, b_rook],
        [b_pawn, b_pawn,   b_pawn,   b_pawn,  empty,    b_pawn,   b_pawn,   b_pawn],
        [empty,  empty,    empty,    empty,   empty,    empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   w_knight, empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   empty,    empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   empty,    empty,    empty,    empty ],
        [w_pawn, w_pawn,   w_pawn,   w_pawn,  w_pawn,   w_pawn,   w_pawn,   w_pawn],
        [w_rook, w_knight, w_bishop, w_queen, w_king,   w_bishop, empty,    w_rook] 
    ])
])).

% This test is for depth limit 2, by only applying the minimax algorithm and nothing extra.

test(minimax_next_game_state) :-
    game_minimax_before(GameBefore),
    game_minimax_after(GameAfter),
    profile(minimax_next_game_state(GameBefore, NextGameState)),
    assertion(NextGameState == GameAfter).

game_minimax_koth_before(game([
    mode(koth),
    result(unfinished),
    turn(white),
    history([
        move([piece(pawn),position((5,3))]),
        move([piece(pawn),position((8,6))]),
        move([piece(king),position((5,2))]),
        move([piece(pawn),position((8,5))]),
        move([piece(king),position((4,3))]),
        move([piece(pawn),position((6,5))])
    ]),
    board([
        [b_rook, b_knight, b_bishop, b_queen, b_king, b_bishop, b_knight, b_rook],
        [b_pawn, b_pawn,   b_pawn,   b_pawn,  b_pawn, empty,    b_pawn,   empty ],
        [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   empty,  b_pawn,   empty,    b_pawn],
        [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
        [empty,  empty,    empty,    w_king,  w_pawn, empty,    empty,    empty ],
        [w_pawn, w_pawn,   w_pawn,   w_pawn,  empty,  w_pawn,   w_pawn,   w_pawn],
        [w_rook, w_knight, w_bishop, w_queen, empty,  w_bishop, w_knight, w_rook] 
    ])
])).

game_minimax_koth_after(game([
    mode(koth),
    result(white),
    turn(black),
    history([
        move([piece(pawn),position((5,3))]),
        move([piece(pawn),position((8,6))]),
        move([piece(king),position((5,2))]),
        move([piece(pawn),position((8,5))]),
        move([piece(king),position((4,3))]),
        move([piece(pawn),position((6,5))]),
        move([piece(king),position((4,4)),check(kingcenter)])
    ]),
    board([
        [b_rook, b_knight, b_bishop, b_queen, b_king, b_bishop, b_knight, b_rook],
        [b_pawn, b_pawn,   b_pawn,   b_pawn,  b_pawn, empty,    b_pawn,   empty ],
        [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   empty,  b_pawn,   empty,    b_pawn],
        [empty,  empty,    empty,    w_king,  empty,  empty,    empty,    empty ],
        [empty,  empty,    empty,    empty,   w_pawn, empty,    empty,    empty ],
        [w_pawn, w_pawn,   w_pawn,   w_pawn,  empty,  w_pawn,   w_pawn,   w_pawn],
        [w_rook, w_knight, w_bishop, w_queen, empty,  w_bishop, w_knight, w_rook] 
    ])
])).

test(minimax_next_game_state_koth) :-
    game_minimax_koth_before(GameBefore),
    game_minimax_koth_after(GameAfter),
    minimax_next_game_state(GameBefore, NextGameState),
    assertion(NextGameState == GameAfter).
    

%=============================================================================================================================================%
%===  evaluate_game_state  ===================================================================================================================%
%=============================================================================================================================================%

test(evaluate_game_state) :-
    board:init_board(Board),
    Game = game([mode(classic), result(unfinished), turn(white), history([]), board(Board)]),
    evaluate_game_state(Game, Score),
    assertion(Score == 0).

:- end_tests(minimax_tests).