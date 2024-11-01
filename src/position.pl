:- module(position, [
    moveable_position/4,    % +History, +Board, +BeginPos, -MoveablePosition
    begin_position/4        % +Board, +Move, +Color, -BeginPos
]).

:- use_module(pieces).
:- use_module(board).
:- use_module(pawn).
:- use_module(king).

%=============================================================================================================================================%
%===  moveable_position  =====================================================================================================================%
%=============================================================================================================================================%

% Note: this predicate is not the same as dangerous_square.
%   Moveable_position includes pawn-one/two-step moves and excludes positions
%   where the king can be taken.
%   Dangerous_square includes positions where the king can be taken, but excludes
%   pawn-one/two-step moves.

% Note: this predicate does not include castling (you can't really provide a single position for castling).

moveable_position(History, Board, BeginPos, MoveablePosition) :-
    piece_at_position(Board, BeginPos, Piece),
    pieces:color_of_piece(Piece, Color),
    mp(History, Board, Color, Piece, BeginPos, MoveablePosition).
    
% rook, knight, bishop, queen
mp(_, Board, Color, Piece, BeginPos, EndPos) :-
    \+ pieces:colored_uncolored_piece(Piece, king),
    pieces:relative_move_cell(Piece, RelCell),
    pieces:apply_cell(Board, BeginPos, RelCell, Color, EndPos).

% king
mp(_, Board, Color, Piece, _, EndPos) :-
    pieces:colored_uncolored_piece(Piece, king),
    king:filtered_king_positions(Board, Color, EndPos).

% pawn
mp(_, Board, Color, Piece, BeginPos, EndPos) :-
    pieces:colored_uncolored_piece(Piece, pawn),
    pawn:pawn_available_position(Board, Color, BeginPos, EndPos). 

% en-passant
mp(History, _, Color, Piece, BeginPos, EndPos) :-
    pieces:colored_uncolored_piece(Piece, pawn),
    pawn:en_passant(History, Color, BeginPos, EndPos).

%=============================================================================================================================================%
%===  begin_position  ========================================================================================================================%
%=============================================================================================================================================%

% en-passant
begin_position(Board, move(Attributes), Color, (C1,R1)) :-
    member(position(EndPos), Attributes), 
    
    % en-passant is the only way a capture can happen while still moving to an empty space
    member(capture, Attributes), 
    board:piece_at_position(Board, EndPos, empty),

    EndPos = (_, R2),
    pawn:en_passant_to_rel_row(Color, R_),
    plus(R1, R_, R2),
    member(from_position(column(C1)), Attributes),
    !.


% no from_position
begin_position(Board, move(Attributes), Color, (C,R)) :-
    \+ member(from_position(_), Attributes),
    bp(Board, move(Attributes), Color, (C,R)),
    !.

% column provided
begin_position(Board, move(Attributes), Color, (C,R)) :-
    member(from_position(column(C)), Attributes),
    bp(Board, move(Attributes), Color, (C,R)),
    !.

% row provided
begin_position(Board, move(Attributes), Color, (C,R)) :-
    member(from_position(row(R)), Attributes),
    bp(Board, move(Attributes), Color, (C,R)),
    !.

% both column and row provided
begin_position(_, move(Attributes), _, (C,R)) :-
    member(from_position(both(C,R)), Attributes),
    !.

bp(Board, move(Attributes), Color, (C,R)) :-

    % get the piece of this move
    member(piece(UncoloredPiece), Attributes),
    pieces:colored_uncolored_piece(Piece, UncoloredPiece),
    pieces:color_of_piece(Piece, Color),
    
    % get all possible positions on the board for this piece
    board:piece_at_position(Board, (C,R), Piece),

    % for every possible position, check if EndPos is available
    member(position(EndPos), Attributes),
    bp_(Board, Color, Piece, (C,R), EndPos).

% rook, knight, bishop, queen, king
bp_(Board, Color, Piece, BeginPos, EndPos) :-
    pieces:relative_move_cell(Piece, RelCell),
    pieces:apply_cell(Board, BeginPos, RelCell, Color, EndPos).

% pawn
bp_(Board, Color, Piece, BeginPos, EndPos) :-
    pieces:colored_uncolored_piece(Piece, pawn),
    pawn:pawn_available_position(Board, Color, BeginPos, EndPos). 
