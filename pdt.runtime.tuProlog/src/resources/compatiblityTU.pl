plus(A,B,C) :-
  nonvar(A),
  nonvar(B),
  !,
  C is A + B.
  
plus(A,B,C) :-
	nonvar(A),
	nonvar(C),
	  !,
  B is C - A.

  
plus(A,B,C) :-
	nonvar(B),
	nonvar(C),
	  !,
  A is C - B.
  
