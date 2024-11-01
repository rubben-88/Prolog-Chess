:- module(utils, [
    take/3,     % +N, +List, -FirstElements
    drop/3      % +N, +List, -ListMinFirstN
]).

%=============================================================================================================================================%
%===  take  ==================================================================================================================================%
%=============================================================================================================================================%

take(0, _, []).
take(N, [H | T1], [H | T2]) :-
    N_ is N - 1,
    take(N_, T1, T2),
    !.

%=============================================================================================================================================%
%===  drop  ==================================================================================================================================%
%=============================================================================================================================================%

drop(0, L, L).
drop(N, [_|T], T_) :-
    N_ is N - 1,
    drop(N_, T, T_),
    !.