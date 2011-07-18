sublist(S, L) :-
  append(_, L2, L),
  append(S, _, L2).

contains(A, B) :-
  atom(A),
  atom(B),
  name(A, AA),
  name(B, BB),
  contains(AA, BB).

contains(A, B) :-
  atom(B),
  name(B, BB),
  contains(A, BB).
  
contains(A, B) :-
  atom(A),
  name(A, AA),
  contains(AA, B).

contains(A, B) :-
  sublist(B, A),
  B \= [].
