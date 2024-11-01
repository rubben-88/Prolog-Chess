:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

:- initialization main.

:- use_module(parser).
:- use_module(game).
:- use_module(minimax).

%===============================================================================================%

% > cat fool.pgn
%   [White "Schlatt"]
%   [Black "Botez, Alexandra"]
%   [Rules "classic"]
%   [Result "*"]
%
%   1. g4 e5 2. f3 *
%

% > swipl -t halt -f -q -O ./main.pl -- {bestandsnaam}
%   1. g4 e5 2. f3 Qh4# 0-1

%===============================================================================================%

% --> d3, d5, en Nf3 reeds genomen

% > swipl -t halt -f -q -O ./main.pl -- examples/london.pgn TEST
%   1. d4 d5 2. Nf3 Nd7
%   1. d4 d5 2. Nf3 Nc6
%   1. d4 d5 2. Nf3 Na6
%   1. d4 d5 2. Nf3 Bd7
%   ...

%===============================================================================================%

% TODO: findall & sort -> 1 predicate

main :-
    current_prolog_flag(argv, Argv),
    do_main(Argv),
    halt(0).

%---------------+
%--- Normal ----|
%---------------+

do_main([File]) :-
    parser:pgn_to_game(File, Game),
    (
        minimax:minimax_next_game_state(Game, PossibleNextState)
        ->
        parser:game_to_pgn(PossibleNextState, PossibleNextGameLine),
        writeln(PossibleNextGameLine)
        ;
        true
    ).

%---------------+
%--- TEST ------|
%---------------+

do_main([File, _]) :-
    parser:pgn_to_game(File, Game),
    findall(NextState, game:next_game_state(Game, NextState), PossibleNextStates),
    maplist(parser:game_to_pgn(), PossibleNextStates, PossibleNextGameLines),
    maplist(writeln(), PossibleNextGameLines).
