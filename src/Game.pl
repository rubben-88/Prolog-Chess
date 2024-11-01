:- module(game, [
    next_game_state/2,  % +Game, -NextGameState
    get_result/6        % +Mode, +History, +Board, +LastMove, +Turn, -Result
]).

:- use_module(move).
:- use_module(pieces).

% game
%   - mode
%       classic | koth
%   - result
%       black | white | tie | unfinished
%   - turn
%       black | white
%   - history
%       [move(...), move(...), ...]
%   - board
%       [[w_pawn, b_queen, empty, ...],[...], ...]

%=============================================================================================================================================%
%===  next_game_state  =======================================================================================================================%
%=============================================================================================================================================%

next_game_state(game(Attributes), NextGameState) :-
    member(mode(Mode), Attributes),
    member(history(History1), Attributes),
    member(board(Board1), Attributes),
    member(turn(Turn1), Attributes),
    member(result(Result1), Attributes),

    % check result is unfinished
    Result1 = unfinished,

    % new board
    move:next_move(Mode, History1, Board1, Turn1, NextMove),
    move:execute_move(Board1, NextMove, Turn1, Board2),

    % new turn
    pieces:opposite_color(Turn1, Turn2),

    % new history
    append(History1, [NextMove], History2),

    % new result
    get_result(Mode, History2, Board2, NextMove, Turn2, Result2),

    % all together
    NextGameState = game([
        mode(Mode),
        result(Result2),
        turn(Turn2),
        history(History2),
        board(Board2)
    ]).

%=============================================================================================================================================%
%===  get_result  ============================================================================================================================%
%=============================================================================================================================================%

get_result(koth, _, _, move(Attributes), Turn, Result) :-
    member(check(kingcenter), Attributes),
    pieces:opposite_color(Turn, Result),
    !. % red cut

get_result(_, _, _, move(Attributes), Turn, Result) :-
    member(check(checkmate), Attributes),
    pieces:opposite_color(Turn, Result),
    !. % red cut

get_result(Mode, History, Board, _, Turn, tie) :-
    \+ move:next_move(Mode, History, Board, Turn, _),
    !. % red cut

get_result(_, _, _, _, _, unfinished).