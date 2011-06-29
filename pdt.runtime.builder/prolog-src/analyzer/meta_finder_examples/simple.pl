:- module(simple, []).

a(A):-
    call(A).
    
    
b(B):-
    A=B,
    call(A).
    
c(C):-
    a,
    b,
    call(C).
  
d(D):-
    a;
    call(D).
    
e(E):-
    call(call(E)).
    
f(F):-
    blub(A),
    bla(A),
    call(F).
    
g(G):-
    a,
    call(G),
    b.
 
h(H):-
    h(H, b) = h(B, _X),
    call(B).
    
i(H,X):-
    bla(H, b) = bla(B, X),
    call(B).

z(Z):-	%non_meta
	Z = 4.