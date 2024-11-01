:- module(pawn, [
    pawn_available_position/4,  % +Board, +Color, ?BeginPos, ?AvailablePos
    pawn_step/4,                % +Board, +Color, ?BeginPos, ?AvailablePos
    pawn_one_step/4,            % +Board, +Color, ?BeginPos, ?AvailablePos
    pawn_two_steps/4,           % +Board, +Color, ?BeginPos, ?AvailablePos
    pawn_take/4,                % +Board, +Color, ?BeginPos, ?AvailablePos
    pawn_take_left/4,           % +Board, +Color, ?BeginPos, ?AvailablePos
    pawn_take_right/4,          % +Board, +Color, ?BeginPos, ?AvailablePos
    en_passant/4,               % +History, +Color, ?BeginPos, ?AvailablePos
    en_passant_to_rel_row/2     % +Color, -RelTransform
]).

:- use_module(board).
:- use_module(pieces).
:- use_module(history).

%=============================================================================================================================================%
%===  pawn_available_position  ===============================================================================================================%
%=============================================================================================================================================%

% Note: this predicate does not include en_passant

pawn_available_position(Board, Color, BeginPos, AvailablePos) :- pawn_step(Board, Color, BeginPos, AvailablePos).
pawn_available_position(Board, Color, BeginPos, AvailablePos) :- pawn_take(Board, Color, BeginPos, AvailablePos).

%=============================================================================================================================================%
%===  pawn_step  =============================================================================================================================%
%=============================================================================================================================================%

pawn_step(Board, Color, BeginPos, AvailablePos) :- pawn_one_step(Board, Color, BeginPos, AvailablePos).
pawn_step(Board, Color, BeginPos, AvailablePos) :- pawn_two_steps(Board, Color, BeginPos, AvailablePos).

%=============================================================================================================================================%
%===  pawn_one_step  =========================================================================================================================%
%=============================================================================================================================================%

rel_one_step(white, (0, 1)).
rel_one_step(black, (0,-1)).

pawn_one_step(Board, Color, (C,R1), (C,R2)) :-
    rel_one_step(Color, (_,R_)),
    plus(R1, R_, R2),
    board:piece_at_position(Board, (C,R2), empty).

%=============================================================================================================================================%
%===  pawn_two_steps  ========================================================================================================================%
%=============================================================================================================================================%

init_pawn_row(white, 2).
init_pawn_row(black, 7).

pawn_two_steps(Board, Color, (C,R1), (C,R2)) :-
    init_pawn_row(Color, R1),
    rel_one_step(Color, (_,R_)),
    plus(R1, R_, R_tmp),
    board:piece_at_position(Board, (C,R_tmp), empty),
    plus(R_tmp, R_, R2),
    board:piece_at_position(Board, (C,R2), empty).

%=============================================================================================================================================%
%===  pawn_take  =============================================================================================================================%
%=============================================================================================================================================%

pawn_take(Board, Color, BeginPos, AvailablePos) :- pawn_take_left(Board, Color, BeginPos, AvailablePos).
pawn_take(Board, Color, BeginPos, AvailablePos) :- pawn_take_right(Board, Color, BeginPos, AvailablePos).

%=============================================================================================================================================%
%===  pawn_take_left  ========================================================================================================================%
%=============================================================================================================================================%

rel_take_left(white, (-1, 1)).
rel_take_left(black, (-1,-1)).

pawn_take_left(Board, Color, (C1,R1), (C2,R2)) :-
    rel_take_left(Color, (C_, R_)),
    plus(C1, C_, C2),
    plus(R1, R_, R2),
    board:piece_at_position(Board, (C2,R2), CapturedPiece),
    pieces: opposite_color(Color, OppositeColor),
    pieces:color_of_piece(CapturedPiece, OppositeColor).

%=============================================================================================================================================%
%===  pawn_take_right  =======================================================================================================================%
%=============================================================================================================================================%

rel_take_right(white, (1, 1)).
rel_take_right(black, (1,-1)).

pawn_take_right(Board, Color, (C1,R1), (C2,R2)) :-
    rel_take_right(Color, (C_, R_)),
    plus(C1, C_, C2),
    plus(R1, R_, R2),
    board:piece_at_position(Board, (C2,R2), CapturedPiece),
    pieces: opposite_color(Color, OppositeColor),
    pieces:color_of_piece(CapturedPiece, OppositeColor).

%=============================================================================================================================================%
%===  en_passant  ============================================================================================================================%
%=============================================================================================================================================%

en_passant(History, Color, (C_LL, R_L), (C_L, R2)) :-

    % get the last and last-last move
    append(PrevHistory, [LastLastMove, LastMove], History),
    LastMove = move(AttributesLast),
    member(position((C_L, R_L)), AttributesLast),
    LastLastMove = move(AttributesLastLast),
    member(position((C_LL, R_L)), AttributesLastLast),

    % check that both the last and last-last move where pawn moves
    member(piece(pawn), AttributesLast),
    member(piece(pawn), AttributesLastLast),

    % check that last-last move ended on a en-passant-valid square
    en_passant_valid_row_info(Color, R_L),

    % check that the last move ended next to that square
    next_to_column(C_LL, C_L),

    % check that the last move was two places at once (not moved before)
    en_passant_to_rel_row(Color, R_),
    R__ is R_ * 2,
    plus(R_L, R__, R_last_from),
    history:has_not_moved_yet(PrevHistory, (C_L, R_last_from)),

    % every condition was met for the en-passant
    plus(R_L, R_, R2),
    !.
    

en_passant_valid_row_info(white, 5).
en_passant_valid_row_info(black, 4).

next_to_column(C, C_next) :- C_next is C - 1, C_next >= 1, C_next =< 8.
next_to_column(C, C_next) :- C_next is C + 1, C_next >= 1, C_next =< 8.

%=============================================================================================================================================%
%===  en_passant_to_rel_row  =================================================================================================================%
%=============================================================================================================================================%

en_passant_to_rel_row(white,  1).
en_passant_to_rel_row(black, -1).