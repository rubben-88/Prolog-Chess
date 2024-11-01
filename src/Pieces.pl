:- module(pieces, [
    is_piece/1,                 % ?Piece
    is_uncolored_piece/1,       % ?UncoloredPiece
    colored_uncolored_piece/2,  % ?Piece, ?UncoloredPiece
    color_of_piece/2,           % ?Piece, ?Color
    opposite_color/2,           % ?Color, ?OppositeColor
    relative_move_cell/2,       % +Piece, -RelCell
    apply_cell/5,               % +Board, +BeginPos, +RelCell, +Color, -AvailablePos
    apply_cell_include_own/5,   % +Board, +BeginPos, +RelCell, +Color, -AvailablePos
    rel_abs_conv/3,             % ?BeginPos, ?RelTransform, ?AbsPos
    cell_rel_to_abs/3,          % +BeginPos, +RelCell, -AbsCell
    cell_until_blocked/4        % +Board, +AbsCell, +Color, -AvailablePos
]).

:- use_module(board).

:- use_module(library(apply)).  % maplist/3

%=============================================================================================================================================%
%===  is_piece  ==============================================================================================================================%
%=============================================================================================================================================%

is_piece(w_pawn).
is_piece(w_rook).
is_piece(w_knight).
is_piece(w_bishop).
is_piece(w_queen).
is_piece(w_king).
is_piece(b_pawn).
is_piece(b_rook).
is_piece(b_knight).
is_piece(b_bishop).
is_piece(b_queen).
is_piece(b_king).

%=============================================================================================================================================%
%===  is_uncolored_piece  ====================================================================================================================%
%=============================================================================================================================================%

is_uncolored_piece(pawn).
is_uncolored_piece(rook).
is_uncolored_piece(knight).
is_uncolored_piece(bishop).
is_uncolored_piece(queen).
is_uncolored_piece(king).

%=============================================================================================================================================%
%===  colored_uncolored_piece  ===============================================================================================================%
%=============================================================================================================================================%

colored_uncolored_piece(w_pawn,   pawn).
colored_uncolored_piece(w_rook,   rook).
colored_uncolored_piece(w_knight, knight).
colored_uncolored_piece(w_bishop, bishop).
colored_uncolored_piece(w_queen,  queen).
colored_uncolored_piece(w_king,   king).
colored_uncolored_piece(b_pawn,   pawn).
colored_uncolored_piece(b_rook,   rook).
colored_uncolored_piece(b_knight, knight).
colored_uncolored_piece(b_bishop, bishop).
colored_uncolored_piece(b_queen,  queen).
colored_uncolored_piece(b_king,   king).

%=============================================================================================================================================%
%===  color_of_piece  ========================================================================================================================%
%=============================================================================================================================================%

color_of_piece(w_pawn,   white).
color_of_piece(w_rook,   white).
color_of_piece(w_knight, white).
color_of_piece(w_bishop, white).
color_of_piece(w_queen,  white).
color_of_piece(w_king,   white).
color_of_piece(b_pawn,   black).
color_of_piece(b_rook,   black).
color_of_piece(b_knight, black).
color_of_piece(b_bishop, black).
color_of_piece(b_queen,  black).
color_of_piece(b_king,   black).


%=============================================================================================================================================%
%===  opposite_color  ========================================================================================================================%
%=============================================================================================================================================%

opposite_color(white, black).
opposite_color(black, white).

%=============================================================================================================================================%
%===  relative_move_cell  ====================================================================================================================%
%=============================================================================================================================================%

% Note: everything board or history dependent is not included, but gets added later.
%   (The basic king moves are included, but still need to be filtered 
%    as to not include moves after which it could be captured)

% rook
relative_move_cell(w_rook, [( 0, 1),( 0, 2),( 0, 3),( 0, 4),( 0, 5),( 0, 6),( 0, 7)]). % up
relative_move_cell(w_rook, [( 0,-1),( 0,-2),( 0,-3),( 0,-4),( 0,-5),( 0,-6),( 0,-7)]). % down
relative_move_cell(w_rook, [(-1, 0),(-2, 0),(-3, 0),(-4, 0),(-5, 0),(-6, 0),(-7, 0)]). % left
relative_move_cell(w_rook, [( 1, 0),( 2, 0),( 3, 0),( 4, 0),( 5, 0),( 6, 0),( 7, 0)]). % right
relative_move_cell(b_rook, X) :- relative_move_cell(w_rook, X).

% knight
relative_move_cell(w_knight, [( 1, 2)]). % 2 up 1 right
relative_move_cell(w_knight, [(-1, 2)]). % 2 up 1 left
relative_move_cell(w_knight, [( 1,-2)]). % 2 down 1 right
relative_move_cell(w_knight, [(-1,-2)]). % 2 down 1 left
relative_move_cell(w_knight, [(-2, 1)]). % 2 left 1 up
relative_move_cell(w_knight, [(-2,-1)]). % 2 left 1 down
relative_move_cell(w_knight, [( 2, 1)]). % 2 right 1 up
relative_move_cell(w_knight, [( 2,-1)]). % 2 right 1 down
relative_move_cell(b_knight, X) :- relative_move_cell(w_knight, X).

% bishop
relative_move_cell(w_bishop, [(-1, 1),(-2, 2),(-3, 3),(-4, 4),(-5, 5),(-6, 6),(-7, 7)]). % left up
relative_move_cell(w_bishop, [( 1, 1),( 2, 2),( 3, 3),( 4, 4),( 5, 5),( 6, 6),( 7, 7)]). % right up
relative_move_cell(w_bishop, [(-1,-1),(-2,-2),(-3,-3),(-4,-4),(-5,-5),(-6,-6),(-7,-7)]). % left down
relative_move_cell(w_bishop, [( 1,-1),( 2,-2),( 3,-3),( 4,-4),( 5,-5),( 6,-6),( 7,-7)]). % right down
relative_move_cell(b_bishop, X) :- relative_move_cell(w_bishop, X).

% queen
relative_move_cell(w_queen, X) :- relative_move_cell(w_rook, X).
relative_move_cell(w_queen, X) :- relative_move_cell(w_bishop, X). 
relative_move_cell(b_queen, X) :- relative_move_cell(w_queen, X).

 % king
relative_move_cell(w_king, [(-1, 1)]). % up left
relative_move_cell(w_king, [( 0, 1)]). % up
relative_move_cell(w_king, [( 1, 1)]). % up right
relative_move_cell(w_king, [(-1, 0)]). % left                  
relative_move_cell(w_king, [( 1, 0)]). % right
relative_move_cell(w_king, [(-1,-1)]). % down left
relative_move_cell(w_king, [( 0,-1)]). % down
relative_move_cell(w_king, [( 1,-1)]). % down right
relative_move_cell(b_king, X) :- relative_move_cell(w_king, X).

%=============================================================================================================================================%
%===  apply_cell  ============================================================================================================================%
%=============================================================================================================================================%

apply_cell(Board, BeginPos, RelCell, Color, AvailablePos) :-
    cell_rel_to_abs(BeginPos, RelCell, AbsCell),
    cell_until_blocked(Board, AbsCell, Color, AvailablePosList),
    nth0(_, AvailablePosList, AvailablePos).

%---------------------+

% Note: we also provide a similar predicate that includes positions where
%   one of you own pieces resides. This in usefull in case you want to determine
%   dangerous positions instead of just moveable positions.

apply_cell_include_own(Board, BeginPos, RelCell, Color, AvailablePos) :-
    cell_rel_to_abs(BeginPos, RelCell, AbsCell),
    cell_until_blocked_include_own(Board, AbsCell, Color, AvailablePosList),
    nth0(_, AvailablePosList, AvailablePos).

%=============================================================================================================================================%
%===  rel_abs_conv  ==========================================================================================================================%
%=============================================================================================================================================%

rel_abs_conv((B1, B2), (R1, R2), (E1, E2)) :- 
    plus(B1, R1, E1),
    plus(B2, R2, E2).

%=============================================================================================================================================%
%===  cell_rel_to_abs  =======================================================================================================================%
%=============================================================================================================================================%

cell_rel_to_abs(BeginPos, RelCell, AbsCell) :-
    maplist(rel_abs_conv(BeginPos), RelCell, AbsCell).

%=============================================================================================================================================%
%===  cell_until_blocked  ====================================================================================================================%
%=============================================================================================================================================%
  
% end of cell reached -> stop
cell_until_blocked(_, [], _, []) :- !. 

% out-of-range -> stop
cell_until_blocked(_, [AbsPos|_], _, []) :- 
    \+ board:board_position(AbsPos),
    !.

% empty -> add and continue
cell_until_blocked(Board, [AbsPos | AbsCell], Color, [AbsPos | AvailablePosList]) :- 
    board:piece_at_position(Board, AbsPos, empty),
    cell_until_blocked(Board, AbsCell, Color, AvailablePosList),
    !.

% own color -> stop and don't add
cell_until_blocked(Board, [AbsPos | _], Color, []) :- 
    board:piece_at_position(Board, AbsPos, Piece),
    pieces:color_of_piece(Piece, Color),
    !.

% opponent color -> stop and add
cell_until_blocked(Board, [AbsPos | _], Color, [AbsPos]) :- 
    board:piece_at_position(Board, AbsPos, Piece),
    opposite_color(Color, OppositeColor),
    pieces:color_of_piece(Piece, OppositeColor),
    !.

%---------------------+

% bsaically an exact copy

cell_until_blocked_include_own(_, [], _, []) :- !. 
cell_until_blocked_include_own(_, [AbsPos|_], _, []) :- 
    \+ board:board_position(AbsPos),
    !.
cell_until_blocked_include_own(Board, [AbsPos | AbsCell], Color, [AbsPos | AvailablePosList]) :- 
    board:piece_at_position(Board, AbsPos, empty),
    cell_until_blocked_include_own(Board, AbsCell, Color, AvailablePosList),
    !.
cell_until_blocked_include_own(Board, [AbsPos | _], Color, [AbsPos]) :-  % except this
    board:piece_at_position(Board, AbsPos, Piece),
    pieces:color_of_piece(Piece, Color),
    !.
cell_until_blocked_include_own(Board, [AbsPos | _], Color, [AbsPos]) :- 
    board:piece_at_position(Board, AbsPos, Piece),
    opposite_color(Color, OppositeColor),
    pieces:color_of_piece(Piece, OppositeColor),
    !.
