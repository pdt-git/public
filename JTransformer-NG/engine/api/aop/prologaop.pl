% Autor:
% Datum: 19.11.2003

%:- dynamic test/1.

test(2).
test(_1) :-
         _1 \= 2.
         
:- redefine_system_predicate(assert/1).

assert(_1) :-
           print(_1),
           assertz(_1).

