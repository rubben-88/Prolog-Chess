:- begin_tests(parser_tests).
:- use_module('../src/parser.pl').

game_no_specials(
    game([
        mode(classic),
        result(unfinished),
        turn(white),
        history([
            move([ piece(pawn), position((5,4)) ]),
            move([ piece(pawn), position((4,5)) ]),
            move([ piece(pawn), from_position(column(5)), capture, position((4,5)) ]),
            move([ piece(queen), capture, position((4,5)) ]),
            move([ piece(queen), position((6,3)) ]),
            move([ piece(knight), position((6,6)) ]),
            move([ piece(queen), capture, position((4,5)) ]),
            move([ piece(knight), capture, position((4,5)) ]),
            move([ piece(pawn), position((7,4)) ]),
            move([ piece(bishop), position((6,5)) ]),
            move([ piece(knight), position((3,3)) ]),
            move([ piece(king), position((4,8)) ]),
            move([ piece(pawn), position((6,3)) ]),
            move([ piece(knight), position((1,6)) ]),
            move([ piece(rook), position((2,1)) ]),
            move([ piece(knight), from_position(column(1)), position((2,4)) ])
        ]),    
        board([
            [b_rook,  empty,    empty,    b_king,   empty,   b_bishop, empty,    b_rook ],
            [b_pawn,  b_pawn,   b_pawn,   empty,    b_pawn,  b_pawn,   b_pawn,   b_pawn ],
            [empty,   empty,    empty,    empty,    empty,   empty,    empty,    empty  ],
            [empty,   empty,    empty,    b_knight, empty,   b_bishop, empty,    empty  ],
            [empty,   b_knight, empty,    empty,    empty,   empty,    w_pawn,   empty  ],
            [empty,   empty,    w_knight, empty,    empty,   w_pawn,   empty,    empty  ],
            [w_pawn,  w_pawn,   w_pawn,   w_pawn,   empty,   empty,    empty,    w_pawn ],
            [empty,   w_rook,   w_bishop, empty,    w_king,  w_bishop, w_knight, w_rook ]
        ])
    ])
).

%=============================================================================================================================================%
%===  pgn_to_game  ===========================================================================================================================%
%=============================================================================================================================================%

test(pgn_to_game) :-
    pgn_to_game("tests/examples/no_specials.pgn", Game),
    game_no_specials(ShouldBeGame),
    assertion(Game == ShouldBeGame).


%=============================================================================================================================================%
%===  game_to_pgn  ===========================================================================================================================%
%=============================================================================================================================================%

test(game_to_pgn) :-
    game_no_specials(Game),
    ShouldBeGameLine = "1. e4 d5 2. exd5 Qxd5 3. Qf3 Nf6 4. Qxd5 Nxd5 5. g4 Bf5 6. Nc3 Kd8 7. f3 Na6 8. Rb1 Nab4 *",
    game_to_pgn(Game, GameLine),
    assertion(GameLine == ShouldBeGameLine).

:- end_tests(parser_tests).   