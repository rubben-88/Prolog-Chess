:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

:- initialization main.

% Portable Game Notation DCG

pgn(Movetext) --> tags(), string(Movetext), result().
pgn(Movetext) --> string(Movetext), result().

tags() --> "\n".
tags() --> "[", string(_), "]\n", tags().

result() --> "".
result() --> " *", remainder(_).

% Get next move

next("1. e4", "1. e4 e5").
next("1. e4 e5", "1. e4 e5 2. f4").

next("1. d4", "1. d4 d5").
next("1. d4 d5", "1. d4 d5 2. Nf3").
next("1. d4 d5 2. Nf3", "1. d4 d5 2. Nf3 Nf6").
next("1. d4 d5 2. Nf3 Nf6", "1. d4 d5 2. Nf3 Nf6 3. Bf4").

% Parse `.pgn` files

file_contains(File, Movetext) :-
    phrase_from_file(pgn(Movetext), File).

main :-
    current_prolog_flag(argv, Argv),
    Argv = [File | _],
    file_contains(File,Codes),
    string_codes(Move, Codes),
    next(Move, Answer),
    write(Answer),nl,
    halt(0).

