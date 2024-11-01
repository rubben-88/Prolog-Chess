:- module(parser, [
    pgn_to_game/2,  % +PgnLocation, -Game
    game_to_pgn/2   % +Game, -GameLine
]).

:- use_module(history).
:- use_module(dcg).
:- use_module(pieces).
:- use_module(game).

%=============================================================================================================================================%
%===  pgn_to_game  ===========================================================================================================================%
%=============================================================================================================================================%

pgn_to_game(PgnLocation, Game) :-
    open(PgnLocation, read, Stream),

    read_stream(Stream, Mode, GameLine),

    % if no mode was specified, select classic
    (var(Mode) -> Mode = classic; true),

    interpret_game_line(GameLine, History),
    history:execute_init_history(History, Board, LastMove, Turn),
    game:get_result(Mode, History, Board, LastMove, Turn, Result),
    Game = game([
        mode(Mode),
        result(Result),
        turn(Turn),
        history(History),
        board(Board)
    ]),

    close(Stream),
    !.  % prevent error in stream on retry

%---------------------+
%--- read_stream -----|
%---------------------+

read_stream(Stream, _, _) :- at_end_of_stream(Stream).
read_stream(Stream, Mode, GameLine) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    string_chars(Line, Chars),

    % mode tag
    (phrase(dcg:dcg_tag(tag('Rules', Value)), Chars) -> Mode = Value; true),

    % game line
    (nth0(1, Chars, '.') -> GameLine = Line; true), % game line if the second character is a dot

    read_stream(Stream, Mode, GameLine).

%---------------------------+
%--- interpret_game_line ---|
%---------------------------+

interpret_game_line(GameLine, History) :-
    split_string(GameLine, " ", " ", SubStrings),
    igl(SubStrings, History).

igl([], []).
igl([SubString | SubStrings], [Move | Moves]) :- 
    string_chars(SubString, Chars),
    phrase(dcg:dcg_move(Move), Chars), 
    igl(SubStrings, Moves), 
    !. % red cut
igl([_ | SubStrings], Moves) :- igl(SubStrings, Moves).

%=============================================================================================================================================%
%===  game_to_pgn  ===========================================================================================================================%
%=============================================================================================================================================%

game_to_pgn(game(Attributes), GameLine) :-
    member(history(History), Attributes),
    pg(History, white, 1, "", TmpGameLineAtom),

    member(result(Result), Attributes),
    phrase(dcg:dcg_result(Result), ResultChars), atom_chars(ResultAtom, ResultChars),

    atomic_concat(TmpGameLineAtom, ResultAtom, GameLineAtom),
    atom_string(GameLineAtom, GameLine),
    !.

pg([], _, _, GameLineAtom, GameLineAtom).
pg([Move | Moves], Color, Counter, BeginGamelineAtom, EndGameLineAtom) :-
    (
        Color = white
        ->
        get_counter_atom(Counter, IncreasedCounter, CounterAtom)
        ;
        IncreasedCounter = Counter,
        CounterAtom = ''
    ),
    phrase(dcg:dcg_move(Move), MoveChars), atom_chars(MoveAtom, MoveChars),
    atomic_concat(CounterAtom, MoveAtom, Tmp),
    atomic_concat(Tmp, " ", TotalAtom),
    
    atomic_concat(BeginGamelineAtom, TotalAtom, TmpGameLineAtom),

    pieces:opposite_color(Color, OppositeColor),
    pg(Moves, OppositeColor, IncreasedCounter, TmpGameLineAtom, EndGameLineAtom).

%--------------------------+
%--- get_counter_atom -----|
%--------------------------+

get_counter_atom(Counter, IncreasedCounter, CounterAtom) :-
    atom_number(Atom, Counter),
    atomic_concat(Atom, '. ', CounterAtom),
    IncreasedCounter is Counter + 1.