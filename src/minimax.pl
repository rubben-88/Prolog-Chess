:- module(minimax, [
    minimax_next_game_state/2,  % +Game, -NextGameState
    evaluate_game_state/2       % +Game, -Score
]).

:- use_module(board).
:- use_module(game).

:- use_module(library(lists)).  % sum_list/2
:- use_module(library(apply)).  % include/3

depth_limit(2).

debug(false).

%write_at_depth(0) :- write("                ").
%write_at_depth(1) :- write("            ").
%write_at_depth(2) :- write("        ").
%write_at_depth(3) :- write("    ").
%write_at_depth(4).

write_at_depth(0) :- write("        ").
write_at_depth(1) :- write("    ").
write_at_depth(2).

% debug test %

%game_minimax_before(game([
%    mode(classic),
%    result(unfinished),
%    turn(white),
%    history([move([piece(pawn),position((5,3))]),move([piece(pawn),position((5,6))]),move([piece(queen),position((6,3))]),move([piece(queen),position((7,5))]),move([piece(queen),position((8,5))]),move([piece(queen),capture,position((7,2))]),move([piece(queen),capture,position((8,7))]),move([piece(queen),capture,position((8,2))]),move([piece(queen),capture,position((8,8))]),move([piece(queen),capture,position((8,1))]),move([piece(queen),capture,position((7,7))]),move([piece(queen),capture,position((7,1))]),move([piece(queen),capture,position((7,8))]),move([piece(queen),position((7,4))]),move([piece(queen),position((7,6))]),move([piece(bishop),position((7,7))]),move([piece(bishop),position((7,2))]),move([piece(king),position((6,8))]),move([piece(king),position((6,1))]),move([piece(king),position((7,8))]),move([piece(king),position((7,1))]),move([piece(king),position((8,8))]),move([piece(king),position((8,1))]),move([piece(bishop),position((6,6))]),move([piece(bishop),position((6,3))]),move([piece(queen),capture,position((6,3)),check(check)]),move([piece(king),position((8,2))]),move([piece(queen),capture,position((6,2)),check(check)]),move([piece(king),position((8,1))]),move([piece(queen),capture,position((5,3))]),move([piece(queen),capture,position((6,7))]),move([piece(bishop),position((5,7))]),move([piece(queen),capture,position((5,7))]),move([piece(queen),capture,position((4,2))]),move([piece(queen),capture,position((5,6))]),move([piece(queen),capture,position((3,1)),check(check)]),move([piece(king),position((8,2))]),move([piece(queen),capture,position((2,1))]),move([piece(queen),capture,position((4,7))]),move([piece(queen),capture,position((1,1))]),move([piece(queen),capture,position((3,8)),check(check)]),move([piece(king),position((8,7))]),move([piece(queen),capture,position((2,8))]),move([piece(queen),capture,position((1,2))]),move([piece(queen),capture,position((1,8))]),move([piece(queen),position((3,4))]),move([piece(queen),capture,position((1,7))]),move([piece(queen),capture,position((3,2)),check(check)]),move([piece(king),position((8,1))]),move([piece(queen),position((4,3))]),move([piece(queen),capture,position((2,7))]),move([piece(queen),position((4,5)),check(check)]),move([piece(queen),capture,position((4,5))]),move([piece(king),position((7,7))]),move([piece(queen),position((7,8)),check(check)]),move([piece(king),capture,position((7,8))]),move([piece(king),position((8,2))]),move([piece(king),position((8,7))]),move([piece(pawn),position((2,4))]),move([piece(pawn),position((3,6))]),move([piece(pawn),position((2,5))]),move([piece(king),position((7,8))]),move([piece(king),position((7,3))]),move([piece(king),position((6,8))]),move([piece(king),position((7,4))]),move([piece(king),position((5,8))]),move([piece(king),position((7,5))]),move([piece(king),position((6,8))]),move([piece(king),position((8,6))]),move([piece(king),position((5,7))]),move([piece(king),position((8,7))]),move([piece(king),position((6,7))]),move([piece(king),position((8,8))]),move([piece(king),position((5,8))])]),
%    board([
%        [empty,    empty,   empty,    empty,   b_king,   empty,    empty,    w_king  ],
%        [empty,    empty,   empty,    empty,   empty,    empty,    empty,    empty   ],
%        [empty,    empty,   b_pawn,   empty,   empty,    empty,    empty,    empty   ],
%        [empty,    w_pawn,  empty,    empty,   empty,    empty,    empty,    empty   ],
%        [empty,    empty,   empty,    empty,   empty,    empty,    empty,    empty   ],
%        [empty,    empty,   empty,    empty,   empty,    empty,    empty,    empty   ],
%        [empty,    empty,   empty,    empty,   empty,    empty,    empty,    empty   ],
%        [empty,    empty,   empty,    empty,   empty,    empty,    empty,    empty   ] 
%    ])
%])).

%test_debug :-
%    game_minimax_before(Game),
%    minimax_next_game_state(Game, _).

%:- use_module(parser).

%test_print :-
%    pgn_to_game("presentatie-files/debug_test.pgn", game(Attributes)),
%    member(history(History), Attributes),
%    writeln(History).
%
%=============================================================================================================================================%
%===  minimax_next_game_state  ===============================================================================================================%
%=============================================================================================================================================%

minimax_next_game_state(Game, BestGame) :-
    Game = game(Attributes),
    member(turn(Turn), Attributes),
    init_min_or_max(Turn, MinOrMax),
    depth_limit(Depth),
    
    (debug(true) -> writeln("Start minimax algorithm!");true),
    minimax(MinOrMax, Game, Depth, MinOrMax, -inf, inf, _, _, _, BestGame),
    !.

%---------------------+
%--- minimax ---------|
%---------------------+

minimax(_, Game, 0, MinOrMax, Alpha, Beta, NewAlpha, NewBeta, Score, Game) :-
    % depth limit reached
    evaluate_game_state(Game, Score),
    set_new_alpha_beta(MinOrMax, Score, Alpha, Beta, NewAlpha, NewBeta),
    (debug(true) -> write_at_depth(0), write("leaf score "), writeln(Score);true).
minimax(_, Game, Depth, MinOrMax, Alpha, Beta, NewAlpha, NewBeta, Score, Game) :-
    % no more possible moves
    Game = game(Attributes),
    member(result(Result), Attributes),
    is_finished(Result),
    evaluate_game_state(Game, Score),
    set_new_alpha_beta(MinOrMax, Score, Alpha, Beta, NewAlpha, NewBeta),
    (debug(true) -> write_at_depth(Depth), write("forecedleaf "), writeln(Score);true).

minimax(BaseMinOrMax, Game, Depth, MinOrMax, Alpha, Beta, NewAlpha, NewBeta, BestScore, BestGame) :-
    % go a level deeper
    NewDepth is Depth - 1,
    switch_minmax(MinOrMax, NewMinOrMax),
    % find all next posible game states
    findall(X, game:next_game_state(Game, X), NextGames),
    (debug(true) -> length(NextGames, N), write_at_depth(Depth), write("looking at "), write(N), writeln(" children:");true),
    % get the best score / game state
    apply_minimax(
        BaseMinOrMax,
        NextGames, 
        NewDepth, 
        NewMinOrMax, 
        Alpha, 
        Beta,
        NewAlpha, 
        NewBeta,
        _, 
        _,
        BestScore,
        BestGame
    ),
    (debug(true) -> write_at_depth(Depth), write("best score amongst children: "), writeln(BestScore);true).

set_new_alpha_beta(min, Score, Alpha, Beta, Alpha, Score) :- Score < Beta.
set_new_alpha_beta(min, Score, Alpha, Beta, Alpha, Beta) :- Score >= Beta.
set_new_alpha_beta(max, Score, Alpha, Beta, Score, Beta) :- Score > Alpha.
set_new_alpha_beta(max, Score, Alpha, Beta, Alpha, Beta) :- Score =< Alpha.

init_min_or_max(white, max).
init_min_or_max(black, min).

is_finished(white).
is_finished(black).
is_finished(tie).

switch_minmax(max, min).
switch_minmax(min, max).

%---------------------+
%--- apply_minimax ---|
%---------------------+

% This predicate goes through all the next game states and selects the best one.

apply_minimax(
    BaseMinOrMax,
    [Game | NextGames], 
    Depth, 
    MinOrMax, 
    Alpha, 
    Beta,
    NewAlpha, 
    NewBeta,
    CurrentScore,
    CurrentGame,
    BestScore,
    BestGame
) :-
    (debug(true) -> length(NextGames, N), X is N + 1, write_at_depth(Depth), write("applying minimax on child "), writeln(X);true),
    minimax(BaseMinOrMax, Game, Depth, MinOrMax, Alpha, Beta, TmpAlpha, TmpBeta, TryScore, _),

    (
        length(NextGames, 0)
        ->
        % stop. end of the list reached
        NewAlpha = TmpAlpha,
        NewBeta = TmpBeta,
        is_better(BaseMinOrMax, CurrentScore, CurrentGame, TryScore, Game, BestScore, BestGame)
        ;
        (
            TmpAlpha >= TmpBeta
            ->
            (debug(true) -> write_at_depth(Depth), writeln("Cut rest of children");true),
            % stop. we know this branch wont be any good
            NewAlpha = TmpAlpha,
            NewBeta = TmpBeta,
            is_better(BaseMinOrMax, CurrentScore, CurrentGame, TryScore, Game, BestScore, BestGame)
            ;
            is_better(BaseMinOrMax, CurrentScore, CurrentGame, TryScore, Game, TmpScore, TmpGame),
            apply_minimax(
                BaseMinOrMax,
                NextGames, 
                Depth, 
                MinOrMax, 
                TmpAlpha, 
                TmpBeta,
                NewAlpha, 
                NewBeta,
                TmpScore,
                TmpGame,
                BestScore,
                BestGame
            )
        )

    ).
    
is_better(_, CurrentScore, _, TryScore, TryGame, TryScore, TryGame) :- var(CurrentScore).
is_better(max, CurrentScore, _, TryScore, TryGame, TryScore, TryGame) :- TryScore > CurrentScore.
is_better(max, CurrentScore, CurrentGame, TryScore, _, CurrentScore, CurrentGame) :- TryScore =< CurrentScore.
is_better(min, CurrentScore, _, TryScore, TryGame, TryGame, TryGame) :- TryScore < CurrentScore.
is_better(min, CurrentScore, CurrentGame, TryScore, _, CurrentScore, CurrentGame) :- TryScore >= CurrentScore.

%=============================================================================================================================================%
%===  evaluate_game_state  ===================================================================================================================%
%=============================================================================================================================================%

evaluate_game_state(game(Attributes), Score) :-
    member(board(Board), Attributes),
    findall(Piece, board:piece_at_position(Board, _, Piece), Pieces),
    include(is_not_empty, Pieces, RealPieces),
    maplist(material_score, RealPieces, IndividualScores),
    sum_list(IndividualScores, MaterialScore),
    (
        member(mode(koth), Attributes)
        ->
        center_king_score(Board, CenterKingScore),
        Score is MaterialScore + CenterKingScore
        ;
        Score = MaterialScore
    ).
    
is_not_empty(Piece) :- Piece \= empty.

material_score(w_pawn,    10).
material_score(w_knight,  30).
material_score(w_bishop,  30).
material_score(w_rook,    50).
material_score(w_queen,   90).
material_score(w_king,   900).
material_score(b_pawn,   -10).
material_score(b_knight, -30).
material_score(b_bishop, -30).
material_score(b_rook,   -50).
material_score(b_queen,  -90).
material_score(b_king,  -900).

%  -20 -10  -5   0   0  -5 -10 -20
%  -10  -5   0   5   5   0  -5 -10
%   -5   0   5  10  10   5   0  -5
%    0   5  10 100 100  10   5   0
%    0   5  10 100 100  10   5   0
%   -5   0   5  10  10   5   0  -5
%  -10  -5   0   5   5   0  -5 -10
%  -20 -10  -5   0   0  -5 -10 _20

center_king_score(Board, -20) :- board:piece_at_position(Board, (1,1), w_king).
center_king_score(Board, -10) :- board:piece_at_position(Board, (1,2), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (1,3), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (1,4), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (1,5), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (1,6), w_king).
center_king_score(Board, -10) :- board:piece_at_position(Board, (1,7), w_king).
center_king_score(Board, -20) :- board:piece_at_position(Board, (1,8), w_king).

center_king_score(Board, -10) :- board:piece_at_position(Board, (2,1), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (2,2), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (2,3), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (2,4), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (2,5), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (2,6), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (2,7), w_king).
center_king_score(Board, -10) :- board:piece_at_position(Board, (2,8), w_king).

center_king_score(Board,  -5) :- board:piece_at_position(Board, (3,1), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (3,2), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (3,3), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (3,4), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (3,5), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (3,6), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (3,7), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (3,8), w_king).

center_king_score(Board,   0) :- board:piece_at_position(Board, (4,1), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (4,2), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (4,3), w_king).
center_king_score(Board, 100) :- board:piece_at_position(Board, (4,4), w_king).
center_king_score(Board, 100) :- board:piece_at_position(Board, (4,5), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (4,6), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (4,7), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (4,8), w_king).

center_king_score(Board,   0) :- board:piece_at_position(Board, (5,1), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (5,2), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (5,3), w_king).
center_king_score(Board, 100) :- board:piece_at_position(Board, (5,4), w_king).
center_king_score(Board, 100) :- board:piece_at_position(Board, (5,5), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (5,6), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (5,7), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (5,8), w_king).

center_king_score(Board,  -5) :- board:piece_at_position(Board, (6,1), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (6,2), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (6,3), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (6,4), w_king).
center_king_score(Board,  10) :- board:piece_at_position(Board, (6,5), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (6,6), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (6,7), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (6,8), w_king).

center_king_score(Board, -10) :- board:piece_at_position(Board, (7,1), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (7,2), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (7,3), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (7,4), w_king).
center_king_score(Board,   5) :- board:piece_at_position(Board, (7,5), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (7,6), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (7,7), w_king).
center_king_score(Board, -10) :- board:piece_at_position(Board, (7,8), w_king).

center_king_score(Board, -20) :- board:piece_at_position(Board, (8,1), w_king).
center_king_score(Board, -10) :- board:piece_at_position(Board, (8,2), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (8,3), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (8,4), w_king).
center_king_score(Board,   0) :- board:piece_at_position(Board, (8,5), w_king).
center_king_score(Board,  -5) :- board:piece_at_position(Board, (8,6), w_king).
center_king_score(Board, -10) :- board:piece_at_position(Board, (8,7), w_king).
center_king_score(Board, -20) :- board:piece_at_position(Board, (8,8), w_king).