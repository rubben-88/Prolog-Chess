:- module(move, [
    next_move/5,        % +Mode, +History, +Board, +Color, -NextMove
    next_move/6,        % +Mode, +History, +Board, +Color, -NextMove, +DoAddCheck
    form_move/6,        % +Mode, +BeginBoard, +Piece, +BeginPos, +EndPos, -Move
    execute_move/4      % +Board, +Move, +Color, -NewBoard
]).

:- use_module(king).
:- use_module(pieces).
:- use_module(board).
:- use_module(position).

:- use_module(library(apply)).  % include/3

% move
%   - piece
%       king | pawn | queen | rook | bishop | knight
%   ~ from_position
%       column(_) | row(_) | both(_,_)
%   ~ capture
%   - position
%       (_,_)
%   ~ promotion
%       queen | rook | bishop | knight
%   ~ check
%       check | checkmate | kingcenter

%=============================================================================================================================================%
%===  next_move  =============================================================================================================================%
%=============================================================================================================================================%

% we use DoAddCheck to prevent an infinite loop in looking for a checkmate

next_move(Mode, History, Board, Color, NextMove) :-
    next_move(Mode, History, Board, Color, NextMove, true).

next_move(_, History, Board, Color, NextMove, _) :-
    castling(History, Board, Color, NextMove).

next_move(Mode, History, Board, Color, NextMove, DoAddCheck) :-
    % generate all positions of own color
    board:piece_at_position(Board, BeginPos, Piece),
    pieces:color_of_piece(Piece, Color),
    % for every piece, generate its available positions and form the corresponding move
    position:moveable_position(History, Board, BeginPos, EndPos),
    form_move(Mode, Board, Piece, BeginPos, EndPos, NextMove, DoAddCheck),

    filter_non_check_moves(Board, NextMove, Color).

filter_non_check_moves(Board, NextMove, Color) :-
    % only allow moves after which the king isn't checked
    execute_move(Board, NextMove, Color, NewBoard),
    \+ is_check(NewBoard, Color).

%=============================================================================================================================================%
%===  form_move  =============================================================================================================================%
%=============================================================================================================================================%

% Note: this predicate doesn't always form just one move.
%   When a promotion happens, we can promote to multiple different pieces,
%   thus generating diffrent moves.

% Note: this predicate can't form a castling move -> king:castling

% Note: use red cuts in the sub-predicates to avoid recalculation.

form_move(Mode, BeginBoard, Piece, BeginPos, EndPos, Move) :- form_move(Mode, BeginBoard, Piece, BeginPos, EndPos, Move, true).

% we use DoAddCheck to prevent an infinite loop in looking for a checkmate

form_move(Mode, BeginBoard, Piece, BeginPos, EndPos, Move, DoAddCheck) :-
    pieces:color_of_piece(Piece, Color),
    
    % check
    (
        DoAddCheck = true
        ->
        pieces:opposite_color(Color, OppositeColor),
        board:go_to(BeginBoard, BeginPos, EndPos, EndBoard),
        add_check(Mode, EndBoard, OppositeColor, [], L1)
        ;
        L1 = []
    ),

    % promotion
    add_promotion(Piece, Color, EndPos, L1, L2), % generative

    % position
    L3 = [position(EndPos) | L2],

    % capture
    add_capture(BeginBoard, Piece, BeginPos, EndPos, L3, L4),

    % from_position
    add_from_position(BeginBoard, Piece, BeginPos, EndPos, L4, L5),

    % piece
    pieces:colored_uncolored_piece(Piece, UncoloredPiece),
    L6 = [piece(UncoloredPiece) | L5],

    Move = move(L6).

%---------------------+
%--- check -----------|
%---------------------+

add_check(koth, EndBoard, OppositeColor, In, [check(kingcenter) | In]) :- pieces:opposite_color(Color, OppositeColor), king:is_king_center(EndBoard, Color), !.
add_check(_, EndBoard, OppositeColor, In, [check(checkmate) | In]) :- king:is_checkmate(EndBoard, OppositeColor), !.
add_check(_, EndBoard, OppositeColor, In, [check(check) | In]) :- king:is_check(EndBoard, OppositeColor), !.
add_check(_, _, _, In, In).

%---------------------+
%--- promotion -------|
%---------------------+

add_promotion(Piece, _, _, In, In) :- 
    \+ pieces:colored_uncolored_piece(Piece, pawn).

add_promotion(Piece, Color, (_,R), In, In) :- 
    pieces:colored_uncolored_piece(Piece, pawn),
    \+ pawn_end_row(Color, R).

add_promotion(Piece, Color, (_,R), In, [promotion(PromotionPiece) |In]) :-
    pieces:colored_uncolored_piece(Piece, pawn),
    pawn_end_row(Color, R),
    promotion_piece(PromotionPiece).

pawn_end_row(white, 8).
pawn_end_row(black, 1).

promotion_piece(queen).
promotion_piece(knight).

% Note: the rook and bishop will never have an additional benefit over the queen,
%   so we don't include them here.
% promotion_piece(rook).
% promotion_piece(bishop).

%---------------------+
%--- capture ---------|
%---------------------+

add_capture(BeginBoard, Piece, (C1, _), (C2, R2), In, [capture |In]) :- 
    % en-passant
    pieces:colored_uncolored_piece(Piece, pawn),
    board:piece_at_position(BeginBoard, (C2, R2), empty), 
    C1 \= C2,
    !.

add_capture(BeginBoard, _, _, EndPos, In, [capture |In]) :- 
    \+ board:piece_at_position(BeginBoard, EndPos, empty), 
    !.

add_capture(_, _, _, _, In, In).

%---------------------+
%--- from_position ---|
%---------------------+

add_from_position(_, Piece, (C1, _), (C2, _), In, [from_position(column(C1)) | In]) :-
    % pawn capture always from_position column
    C1 \= C2,
    pieces:colored_uncolored_piece(Piece, pawn),
    !.

add_from_position(BeginBoard, Piece, BeginPos, EndPos, In, Out) :-
    % find all instances of pieces of the same type and color
    % that also can move to EndPos
    findall(X, same_piece_same_end_pos(BeginBoard, Piece, EndPos, X), Xs),
    % we use a different from_position according to how many instances we found
    length(Xs, N),
    afp(Xs, N, BeginPos, In, Out),
    !.

% Note: we can use an empty history here because 
%   the en-passant case was already captured.
same_piece_same_end_pos(BeginBoard, Piece, EndPos, X) :-
    board:piece_at_position(BeginBoard, X, Piece),
    position:moveable_position([], BeginBoard, X, EndPos).

afp(_, N, _, In, In) :- N =< 1.
afp([(_, R), (_, R)], 2, _, In, [from_position(row(R)) | In]).
afp([(C, _), (C, _)], 2, _, In, [from_position(column(C)) | In]).
afp([(C1, R1), (C2, R2)], 2, (C,_), In, [from_position(column(C)) | In]) :- C1 \= C2, R1 \= R2.
afp(_, N, (C,R), In, [from_position(both(C,R)) | In]) :- N >= 3.



%=============================================================================================================================================%
%===  execute_move  ==========================================================================================================================%
%=============================================================================================================================================%

% castling
execute_move(Board, move([castling(Type)]), Color, NewBoard) :-
    king:castling_positions(Color, Type, KingBeforePos, KingAfterPos, RookBeforePos, RookAfterPos, _),
    board:go_to(Board, KingBeforePos, KingAfterPos, TmpBoard),
    board:go_to(TmpBoard, RookBeforePos, RookAfterPos, NewBoard),
    !.

% normal scenario
execute_move(Board, Move, Color, NewBoard) :-
    Move = move(Attributes),

    % get the begin position of the move
    member(position(EndPos), Attributes),
    position:begin_position(Board, Move, Color, BeginPos),

    % go to the end position
    board:go_to(Board, BeginPos, EndPos, TmpBoard1),

    % if there is a promotion, do that as well
    (
        member(promotion(UncoloredPiece), Attributes),
        pieces:colored_uncolored_piece(Piece, UncoloredPiece),
        pieces:color_of_piece(Piece, Color)
        ->
        board:place(TmpBoard1, EndPos, Piece, TmpBoard2)
        ;
        TmpBoard2 = TmpBoard1
    ),

    % if the move was en-passant, remove the piece we passed
    (
        % en-passant is the only way a capture can happen while still moving to an empty space
        member(capture, Attributes), 
        board:piece_at_position(Board, EndPos, empty)
        ->
        BeginPos = (_,R),
        EndPos = (C,_),
        board:remove(TmpBoard2, (C,R), NewBoard)
        ;
        NewBoard = TmpBoard2
    ),
    !.