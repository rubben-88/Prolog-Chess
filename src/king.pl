:- module(king, [
    is_check/2,                 % +Board, +Color
    is_checkmate/2,             % +Board, +Color
    is_king_center/2,           % +Board, +Color
    filtered_king_positions/3,  % +Board, +Color, -AvailablePos
    castling/4,                 % +History, +Board, +Color, -Move
    castling_positions/7        % +Color, +Type, ?KingBeforePos, ?KingAfterPos, ?RookBeforePos, ?RookAfterPos, ?OrderedBetweenPositions
]).

:- use_module(pieces).
:- use_module(board).
:- use_module(history).
:- use_module(move).

:- use_module(library(apply)).  % maplist/3

%=============================================================================================================================================%
%===  is_check  ==============================================================================================================================%
%=============================================================================================================================================%

% Note: is_check can succeed even when the king is actually checkmate.

is_check(Board, Color) :-
    pieces:colored_uncolored_piece(King, king),
    pieces:color_of_piece(King, Color),
    board:piece_at_position(Board, Pos, King),

    board:dangerous_square(Board, Color, Pos),
    !.

%=============================================================================================================================================%
%===  is_checkmate  ==========================================================================================================================%
%=============================================================================================================================================%

is_checkmate(Board, Color) :-
    pieces:colored_uncolored_piece(King, king),
    pieces:color_of_piece(King, Color),
    board:piece_at_position(Board, Pos, King),

    % king is in danger
    board:dangerous_square(Board, Color, Pos),
    % king can't move
    \+ filtered_king_positions(Board, Color, _),
    % there are no moves after which the king isn't checked
    \+ move:next_move(classic, [], Board, Color, _, false),
    !.

%=============================================================================================================================================%
%===  is_king_center  ========================================================================================================================%
%=============================================================================================================================================%

is_king_center(Board, Color) :- 
    pieces:colored_uncolored_piece(King, king),
    pieces:color_of_piece(King, Color),
    board:piece_at_position(Board, Pos, King),

    is_center(Pos),
    !.

is_center((4,4)).
is_center((5,4)).
is_center((4,5)).
is_center((5,5)).

%=============================================================================================================================================%
%===  filtered_king_positions  =================================================================================================================%
%=============================================================================================================================================%

filtered_king_positions(Board, Color, AvailablePos) :-
    pieces:colored_uncolored_piece(King, king),
    pieces:color_of_piece(King, Color),
    board:piece_at_position(Board, BeginPos, King),

    pieces:relative_move_cell(King, RelCell),
    apply_cell(Board, BeginPos, RelCell, Color, AvailablePos),

    \+ board:dangerous_square(Board, Color, AvailablePos).

%=============================================================================================================================================%
%===  castling  ==============================================================================================================================%
%=============================================================================================================================================%

castling(History, Board, Color, Move) :-

    % get position information
    castling_type(Type),
    castling_positions(Color, Type, KingBeginPos, KingEndPos, RookBeginPos, _, OrderedBetweenPositions),

    % check if the king and rook are at their corresponding places
    pieces:colored_uncolored_piece(King, king),
    pieces:color_of_piece(King, Color),
    board:piece_at_position(Board, KingBeginPos, King),
    pieces:colored_uncolored_piece(Rook, rook),
    pieces:color_of_piece(Rook, Color),
    board:piece_at_position(Board, RookBeginPos, Rook),

    % check if king has moved already
    history:has_not_moved_yet(History, KingBeginPos),

    % check if rook has moved already
    history:has_not_moved_yet(History, RookBeginPos),

    % check if king is checked
    \+ board:dangerous_square(Board, Color, KingBeginPos),

    % check if pieces between
    positions_all_empty(Board, OrderedBetweenPositions),

    % check if king passes dangerous position
    nth0(0, OrderedBetweenPositions, PassingPos),
    \+ board:dangerous_square(Board, Color, PassingPos),

    % check if king will be checked after the move
    \+ board:dangerous_square(Board, Color, KingEndPos),

    % all checks passed -> create move
    Move = move([castling(Type)]),
    !.

castling_type(short).
castling_type(long).

positions_all_empty(_, []).
positions_all_empty(Board, [Pos | T]) :-
    board:piece_at_position(Board, Pos, empty),
    positions_all_empty(Board, T).

%=============================================================================================================================================%
%===  castling_positions  ====================================================================================================================%
%=============================================================================================================================================%

castling_positions(white, short, (5,1), (7,1), (8,1), (6,1), [(6,1), (7,1)]).
castling_positions(black, short, (5,8), (7,8), (8,8), (6,8), [(6,8), (7,8)]).
castling_positions(white, long, (5,1), (3,1), (1,1), (4,1), [(4,1), (3,1), (2,1)]).
castling_positions(black, long, (5,8), (3,8), (1,8), (4,8), [(4,8), (3,8), (2,8)]).