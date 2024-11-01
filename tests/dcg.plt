:- begin_tests(dcg_tests).
:- use_module('../src/dcg.pl').

% helper function
lists_of_chars_to_atoms([], []).
lists_of_chars_to_atoms([List|Lists], [Atom|Atoms]) :-
    atom_chars(Atom, List),
    lists_of_chars_to_atoms(Lists, Atoms).

%=============================================================================================================================================%
%===  dcg_tag  ===============================================================================================================================%
%=============================================================================================================================================%

test(dcg_tag_string_2_tag) :-
    atom_chars('[Rules "koth"]', Chars),
    findall(X, phrase(dcg:dcg_tag(X), Chars), Xs),
    assertion(Xs == [tag('Rules', 'koth')]).

%=============================================================================================================================================%
%===  dcg_result  ============================================================================================================================%
%=============================================================================================================================================%

test(dcg_move_result_strin_2_result) :-
    atom_chars('1-0', Chars),
    findall(X, phrase(dcg:dcg_result(X), Chars), Xs),
    Xs = [white].

test(dcg_move_result_result_2_string) :-
    Result = white,
    findall(X, phrase(dcg:dcg_result(Result), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['1-0']).

%=============================================================================================================================================%
%===  dcg_move  ==============================================================================================================================%
%=============================================================================================================================================%

%--------------------------------+
%--- Part 1 (string -> move) ----|
%--------------------------------+

test(dcg_move_pawn_1) :-
    atom_chars('e3', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([
        piece(pawn), 
        position((5,3)) 
    ])].

test(dcg_move_knight_1) :-
    atom_chars('Nf3', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        piece(knight), 
        position((6,3)) 
    ])].

test(dcg_move_short_castling_1) :-
    atom_chars('O-O', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        castling(short)
    ])].

test(dcg_move_long_castling_1) :-
    atom_chars('O-O-O', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        castling(long)
    ])].

test(dcg_move_capture_1) :-
    atom_chars('Qxh4', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs == [move([ 
        piece(queen),
        capture,
        position((8,4)) 
    ])].

test(dcg_move_check_1) :-
    atom_chars('Qh4+', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        piece(queen), 
        position((8,4)),
        check(check)
    ])].

test(dcg_move_check_mate_1) :-
    atom_chars('Qb7#', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        piece(queen), 
        position((2,7)),
        check(checkmate)
    ])].

test(dcg_move_from_positions_two_1) :-
    atom_chars('fxg2+', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        piece(pawn), 
        from_position(column(6)), 
        capture,
        position((7,2)),
        check(check)
    ])].

test(dcg_move_from_positions_three_1) :-
    atom_chars('f2xg2+', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([ 
        piece(pawn),
        from_position(both(6,2)), 
        capture,
        position((7,2)),
        check(check)
    ])].

test(dcg_move_promotion_1) :-
    atom_chars('g1=Q', Chars),
    findall(X, phrase(dcg:dcg_move(X), Chars), Xs),
    Xs = [move([  
        piece(pawn), 
        position((7,1)),
        promotion(queen)
    ])].

%--------------------------------+
%--- Part 2 (move -> string) ----|
%--------------------------------+

test(dcg_move_pawn_2) :-
    Move = move([ 
        piece(pawn), 
        position((5,3)) 
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['e3']).

test(dcg_move_knight_2) :-
    Move = move([ 
        piece(knight), 
        position((6,3)) 
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['Nf3']).

test(dcg_move_short_castling_2) :-
    Move = move([ 
        castling(short)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['O-O']).

test(dcg_move_long_castling_2) :-
    Move = move([ 
        castling(long)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['O-O-O']).

test(dcg_move_capture_2) :-
    Move = move([ 
        piece(queen),
        capture,
        position((8,4)) 
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['Qxh4']).

test(dcg_move_check_2) :-
    Move = move([ 
        piece(queen), 
        position((8,4)),
        check(check)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['Qh4+']).

test(dcg_move_check_mate_2) :-
    Move = move([ 
        piece(queen), 
        position((2,7)),
        check(checkmate)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['Qb7#']).

test(dcg_move_from_positions_two_2) :-
    Move = move([ 
        piece(pawn),
        from_position(column(6)), 
        capture,
        position((7,2)),
        check(check)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['fxg2+']).

test(dcg_move_from_positions_three_2) :-
    Move = move([ 
        piece(pawn),
        from_position(both(6,2)), 
        capture,
        position((7,2)),
        check(check)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['f2xg2+']).

test(dcg_move_promotion_2) :-
    Move = move([  
        piece(pawn), 
        position((7,1)),
        promotion(queen)
    ]),
    findall(X, phrase(dcg:dcg_move(Move), X), Xs),
    lists_of_chars_to_atoms(Xs, Strings),
    assertion(Strings == ['g1=Q']).

:- end_tests(dcg_tests).