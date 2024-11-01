:- module(board, [
    init_board/1,           % -Board
    print_board/1,          % +Board
    place/4,                % +Board, +Pos, +Piece, -ResultBoard
    remove/3,               % +Board, +Pos, -ResultBoard
    go_to/4,                % +Board, +BeginPos, +EndPos, -ResultBoard
    piece_at_position/3,    % +Board, ?Pos, ?Piece
    board_position/1,       % ?Pos
    dangerous_square/3      % +Board, +Color, ?Pos
]).

:- use_module(utils).
:- use_module(pawn).
:- use_module(pieces).

:- use_module(library(apply)).  % maplist/3

%=============================================================================================================================================%
%===  init_board  ============================================================================================================================%
%=============================================================================================================================================%

init_board([
    %  A        B        C         D        E        F         G       H
    [b_rook, b_knight, b_bishop, b_queen, b_king, b_bishop, b_knight, b_rook], % 8
    [b_pawn, b_pawn,   b_pawn,   b_pawn,  b_pawn, b_pawn,   b_pawn,   b_pawn], % 7
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ], % 6
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ], % 5
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ], % 4
    [empty,  empty,    empty,    empty,   empty,  empty,    empty,    empty ], % 3
    [w_pawn, w_pawn,   w_pawn,   w_pawn,  w_pawn, w_pawn,   w_pawn,   w_pawn], % 2
    [w_rook, w_knight, w_bishop, w_queen, w_king, w_bishop, w_knight, w_rook]  % 1
]).

%=============================================================================================================================================%
%===  print_board  ===========================================================================================================================%
%=============================================================================================================================================%

print_board(Board) :-
    Positions = [
        (1,8),(2,8),(3,8),(4,8),(5,8),(6,8),(7,8),(8,8),
        (1,7),(2,7),(3,7),(4,7),(5,7),(6,7),(7,7),(8,7),
        (1,6),(2,6),(3,6),(4,6),(5,6),(6,6),(7,6),(8,6),
        (1,5),(2,5),(3,5),(4,5),(5,5),(6,5),(7,5),(8,5),
        (1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),
        (1,3),(2,3),(3,3),(4,3),(5,3),(6,3),(7,3),(8,3),
        (1,2),(2,2),(3,2),(4,2),(5,2),(6,2),(7,2),(8,2),
        (1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1)
    ],
    maplist(piece_at_position(Board), Positions, Pieces),
    print_abbreviations(Pieces, 1),
    writeln(""),
    !.

print_abbreviations([], _).
print_abbreviations([H|T], 8) :- print_abbreviation(H), nl, print_abbreviations(T, 1).
print_abbreviations([H|T], Counter) :-
    Counter \= 8,
    NewCounter is Counter + 1,
    print_abbreviation(H),
    print_abbreviations(T, NewCounter).

print_abbreviation(empty)       :- write(". ").
print_abbreviation(w_pawn)      :- write("P ").
print_abbreviation(w_rook)      :- write("R ").
print_abbreviation(w_knight)    :- write("N ").
print_abbreviation(w_bishop)    :- write("B ").
print_abbreviation(w_queen)     :- write("Q ").
print_abbreviation(w_king)      :- write("K ").
print_abbreviation(b_pawn)      :- write("p ").
print_abbreviation(b_rook)      :- write("r ").
print_abbreviation(b_knight)    :- write("n ").
print_abbreviation(b_bishop)    :- write("b ").
print_abbreviation(b_queen)     :- write("q ").
print_abbreviation(b_king)      :- write("k ").

%=============================================================================================================================================%
%===  place  =================================================================================================================================%
%=============================================================================================================================================%

place(Board, (C,R), Piece, ResultBoard) :-
    R_ is 8 - R,
    R__ is R_ + 1,
    utils:take(R_, Board, FirstRows),
    nth0(R_, Board, Row),
    utils:drop(R__, Board, LastRows),
    C_ is C - 1,
    utils:take(C_, Row, FirstRowElements),
    utils:drop(C, Row, LastRowElements),
    append(FirstRowElements, [Piece | LastRowElements], NewRow),
    append(FirstRows, [NewRow | LastRows], ResultBoard).

%=============================================================================================================================================%
%===  remove  ================================================================================================================================%
%=============================================================================================================================================%

remove(Board, (C,R), ResultBoard) :- place(Board, (C,R), empty, ResultBoard).

%=============================================================================================================================================%
%===  go_to  =================================================================================================================================%
%=============================================================================================================================================%

go_to(Board, BeginPos, EndPos, ResultBoard) :-
    piece_at_position(Board, BeginPos, Piece),
    remove(Board, BeginPos, TmpBoard),
    place(TmpBoard, EndPos, Piece, ResultBoard).

%=============================================================================================================================================%
%===  piece_at_position  =====================================================================================================================%
%=============================================================================================================================================%

piece_at_position(Board, (C,R), Piece) :-
    board_position((C,R)),
    plus(R_, R, 8),
    plus(C_, 1, C),
    nth0(R_, Board, BoardRow),
    nth0(C_, BoardRow, Piece).

%=============================================================================================================================================%
%===  board_position  ========================================================================================================================%
%=============================================================================================================================================%

board_position((C,R)) :-
    between(1, 8, C),
    between(1, 8, R).

%=============================================================================================================================================%
%===  dangerous_square  ======================================================================================================================%
%=============================================================================================================================================%

% Note: this predicate is not the same as moveable_position.
%   Moveable_position includes pawn-one/two-step moves and excludes positions
%   where the king can be taken.
%   Dangerous_square includes positions where the king can be taken, but excludes
%   pawn-one/two-step moves.

% Note: technically speaking, a square could be dangerous for a pawn if 
%   the the other player can execute en_passant after moving to that square.
%   However, we will not include it here.
dangerous_square(Board, Color, DangerousPos) :-
    (nonvar(DangerousPos) -> Cut = true; true),

    % get the opposite color
    pieces:opposite_color(Color, OppositeColor),

    % generate all piece-position pairs of the opposite color on the board
    piece_at_position(Board, BeginPos, Piece),
    pieces:color_of_piece(Piece, OppositeColor),

    % get the available positions for these pieces
    ds(Board, OppositeColor, Piece, BeginPos, DangerousPos),

    (nonvar(Cut) -> !; true).


% Note: ds differs from bp_ in that ds includes positions where 
%   one of you own pieces resides.

% rook, knight, bishop, queen, king
ds(Board, Color, Piece, BeginPos, EndPos) :-
    pieces:relative_move_cell(Piece, RelCell),
    pieces:apply_cell_include_own(Board, BeginPos, RelCell, Color, EndPos).

% pawn
ds(Board, Color, Piece, BeginPos, EndPos) :-
    pieces:colored_uncolored_piece(Piece, pawn),
    pawn:pawn_take(Board, Color, BeginPos, EndPos). 
