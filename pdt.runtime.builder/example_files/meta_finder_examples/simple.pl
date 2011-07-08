:- module(simple, []).

a(A):-			%direct meta-argument
    call(A).
    
    
b(B):-			%direct unification
    A=B,
    call(A).
    
c(C):-			%some other stuff happens
    a,
    b,
    call(C).
  
d(D):-			%some other stuff happens with or
    a;
    call(D).
    
e(E):-			%meta call in meta argument
    call(call(E)).
    
f(F):-			%again other stuff happens but with non-atoms
    blub(A),
    bla(A),
    call(F).
    
g(G):-			%stuff happens around (not only before the meta-predicate-call
    a,
    call(G),
    b.
 
h(H):-			%indirect unification
    h(H, b) = h(B, _X),
    call(B).
    
h2(H,X):-		%X is also a meta-argument -> h2(0,0)
    bla(H, H) = bla(X, B),
    call(B).    

i(H,X):-		%another arguement X which is not a meta-argument -> i(0,?)
    bla(H, b) = bla(B, X),
    call(B).
    
j(J):-			%call to user-defined meta-predicate
    d(J).
    
k(K):-			%meta-predicate-call in true-branch of a decision
    (	bla(K,a) = bla(D,a)
    ->	call(D)
    ;	fail
    ).
    
l(L,M,C):-	%(only L not M is meta-argument, even if they would be aliased in the condition)
    (	bla(M,C) = bla(L,a) 	%meta-predicate-call in else-branch of decision
	->	a
	;	call(L)
	).
	
	
z(Z):-			%non_meta
	Z = 4.