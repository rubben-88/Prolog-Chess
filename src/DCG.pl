:- module(dcg, [
    dcg_tag/3,      % use_in_phrase, string_2_tag_only
    dcg_result/3,   % use_in_phrase, both_ways
    dcg_move/3      % use_in_phrase, both_ways
]).

%=============================================================================================================================================%
%===  dcg_tag  ===============================================================================================================================%
%=============================================================================================================================================%

upper_case_(X)      --> [X], {between(65, 90, Code), char_code(X, Code)}.
lower_case_(X)      --> [X], {between(97, 122, Code), char_code(X, Code)}.
letter_(X)          --> upper_case_(X) | lower_case_(X).
char_(X)            --> [X], {between(0, 33, Code), char_code(X, Code)}.
char_(X)            --> [X], {between(35, 127, Code), char_code(X, Code)}.
chars_([])          --> [].
chars_([H|T])       --> char_(H), chars_(T).

% name
name_([])           --> [].
name_([H|T])        --> letter_(H), name_(T).

% value
value_(X)           --> ['"'], chars_(X), ['"'].

% tag
dcg_tag(tag(Name, Value)) 
    --> ['['], name_(Name_), [' '], value_(Value_), [']'], {atom_chars(Name, Name_), atom_chars(Value, Value_)}.

%=============================================================================================================================================%
%===  dcg_result  ============================================================================================================================%
%=============================================================================================================================================%

dcg_result(black)          --> ['0'], ['-'], ['1'].
dcg_result(white)          --> ['1'], ['-'], ['0'].
dcg_result(tie)            --> ['1'], ['/'], ['2'], ['-'], ['1'], ['/'], ['2'].
dcg_result(unfinished)     --> ['*'].

%=============================================================================================================================================%
%===  dcg_move  ==============================================================================================================================%
%=============================================================================================================================================%

% piece
piece_(pawn)            --> [].
piece_(knight)          --> ['N']. 
piece_(bishop)          --> ['B']. 
piece_(rook)            --> ['R']. 
piece_(queen)           --> ['Q']. 
piece_(king)            --> ['K'].

% position
position_(C, R)         --> column_(C), row_(R).

column_(1)              --> ['a']. 
column_(2)              --> ['b']. 
column_(3)              --> ['c']. 
column_(4)              --> ['d']. 
column_(5)              --> ['e']. 
column_(6)              --> ['f']. 
column_(7)              --> ['g']. 
column_(8)              --> ['h'].

row_(1)                 --> ['1']. 
row_(2)                 --> ['2']. 
row_(3)                 --> ['3']. 
row_(4)                 --> ['4']. 
row_(5)                 --> ['5']. 
row_(6)                 --> ['6']. 
row_(7)                 --> ['7']. 
row_(8)                 --> ['8'].

% capture
capture_                --> ['x'].

% pawn promotion
promotion_(X)           --> ['='], piece_(X).

% check / checkmate
check_(X)               --> just_check_(X) | check_mate_(X).
just_check_(check)      --> ['+'].
check_mate_(checkmate)  --> ['#'].

% castling
castling_(Type)         --> short_castling_(Type) | long_castling_(Type).
short_castling_(short)  --> ['O'], ['-'], ['O'].
long_castling_(long)    --> ['O'], ['-'], ['O'], ['-'], ['O'].

% move
dcg_move(move([castling(Type)]))    --> castling_(Type).

dcg_move(move([piece(C), from_position(column(A)), capture,  position((D,E)), promotion(F), check(G)])) --> piece_(C), column_(A),     capture_, position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C), from_position(column(A)), capture,  position((D,E)), promotion(F)          ])) --> piece_(C), column_(A),     capture_, position_(D,E), promotion_(F)           .
dcg_move(move([piece(C), from_position(column(A)), capture,  position((D,E)),               check(G)])) --> piece_(C), column_(A),     capture_, position_(D,E),                check_(G).
dcg_move(move([piece(C), from_position(column(A)), capture,  position((D,E))                        ])) --> piece_(C), column_(A),     capture_, position_(D,E)                          .
dcg_move(move([piece(C), from_position(column(A)),           position((D,E)), promotion(F), check(G)])) --> piece_(C), column_(A),               position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C), from_position(column(A)),           position((D,E)), promotion(F)          ])) --> piece_(C), column_(A),               position_(D,E), promotion_(F)           .
dcg_move(move([piece(C), from_position(column(A)),           position((D,E)),               check(G)])) --> piece_(C), column_(A),               position_(D,E),                check_(G).
dcg_move(move([piece(C), from_position(column(A)),           position((D,E))                        ])) --> piece_(C), column_(A),               position_(D,E)                          .
dcg_move(move([piece(C), from_position(row(B)),    capture,  position((D,E)), promotion(F), check(G)])) --> piece_(C), row_(B),        capture_, position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C), from_position(row(B)),    capture,  position((D,E)), promotion(F)          ])) --> piece_(C), row_(B),        capture_, position_(D,E), promotion_(F)           .
dcg_move(move([piece(C), from_position(row(B)),    capture,  position((D,E)),               check(G)])) --> piece_(C), row_(B),        capture_, position_(D,E),                check_(G).
dcg_move(move([piece(C), from_position(row(B)),    capture,  position((D,E))                        ])) --> piece_(C), row_(B),        capture_, position_(D,E)                          .
dcg_move(move([piece(C), from_position(row(B)),              position((D,E)), promotion(F), check(G)])) --> piece_(C), row_(B),                  position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C), from_position(row(B)),              position((D,E)), promotion(F)          ])) --> piece_(C), row_(B),                  position_(D,E), promotion_(F)           .
dcg_move(move([piece(C), from_position(row(B)),              position((D,E)),               check(G)])) --> piece_(C), row_(B),                  position_(D,E),                check_(G).
dcg_move(move([piece(C), from_position(row(B)),              position((D,E))                        ])) --> piece_(C), row_(B),                  position_(D,E)                          .
dcg_move(move([piece(C), from_position(both(A,B)), capture,  position((D,E)), promotion(F), check(G)])) --> piece_(C), position_(A,B), capture_, position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C), from_position(both(A,B)), capture,  position((D,E)), promotion(F)          ])) --> piece_(C), position_(A,B), capture_, position_(D,E), promotion_(F)           .
dcg_move(move([piece(C), from_position(both(A,B)), capture,  position((D,E)),               check(G)])) --> piece_(C), position_(A,B), capture_, position_(D,E),                check_(G).
dcg_move(move([piece(C), from_position(both(A,B)), capture,  position((D,E))                        ])) --> piece_(C), position_(A,B), capture_, position_(D,E)                          .
dcg_move(move([piece(C), from_position(both(A,B)),           position((D,E)), promotion(F), check(G)])) --> piece_(C), position_(A,B),           position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C), from_position(both(A,B)),           position((D,E)), promotion(F)          ])) --> piece_(C), position_(A,B),           position_(D,E), promotion_(F)           .
dcg_move(move([piece(C), from_position(both(A,B)),           position((D,E)),               check(G)])) --> piece_(C), position_(A,B),           position_(D,E),                check_(G).
dcg_move(move([piece(C), from_position(both(A,B)),           position((D,E))                        ])) --> piece_(C), position_(A,B),           position_(D,E)                          .
dcg_move(move([piece(C),                           capture,  position((D,E)), promotion(F), check(G)])) --> piece_(C),                 capture_, position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C),                           capture,  position((D,E)), promotion(F)          ])) --> piece_(C),                 capture_, position_(D,E), promotion_(F)           .
dcg_move(move([piece(C),                           capture,  position((D,E)),               check(G)])) --> piece_(C),                 capture_, position_(D,E),                check_(G).
dcg_move(move([piece(C),                           capture,  position((D,E))                        ])) --> piece_(C),                 capture_, position_(D,E)                          .
dcg_move(move([piece(C),                                     position((D,E)), promotion(F), check(G)])) --> piece_(C),                           position_(D,E), promotion_(F), check_(G).
dcg_move(move([piece(C),                                     position((D,E)), promotion(F)          ])) --> piece_(C),                           position_(D,E), promotion_(F)           .
dcg_move(move([piece(C),                                     position((D,E)),               check(G)])) --> piece_(C),                           position_(D,E),                check_(G).
dcg_move(move([piece(C),                                     position((D,E))                        ])) --> piece_(C),                           position_(D,E)                          .
