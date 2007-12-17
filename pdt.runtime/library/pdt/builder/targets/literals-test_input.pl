p(A):-A,b.
p(C):-A,b.
p(A):-test(A).
p(A):-test(la+A).
p(A):-test(B+A).

test(M+A):- M:A.
b(0).
b(N):-M is N -1, b(M).

why_oh_why:-test2([b(3),kjh,b(5),writeln(stop)]).

test2([]).
test2([C|Cs]):-C,test2(Cs).
p(A).