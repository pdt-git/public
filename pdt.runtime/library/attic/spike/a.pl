

:- use_module(library('org/cs3/pdt/util/pdt_util_cs')).

:- use_module(b).

fix_right(
	blackx(
		left=xred(left=Ep,key=KD,value=VD,right=De),
		key=KC,
		value=VC,
		right=xred(left=xred(left=Ga,key=KB,value=VB,right=Be),key=KA,value=VA,righ=Al)
	),
	xred(
		left=xblack(left=Ep,key=KD,value=VD,right=De),
		key=KC,
		value=VC,
		right=xblack(left=xred(left=Ga,key=KB,value=VB,right=Be),key=KA,value=VA,right=Al)
	),
	not_done
	) :- 
	!.

/*
WENN:
n ist ein schwarzer knoten 
	left(n) ist ein roter Knoten
	right(n) ist ein roter Knoten
		left(right(n)) ist ein roter Knoten
		
DANN: 		
n' ist n[farbe=rot]
 	left(n') = left(n)[farbe=schwarz]
 	right(n') = right(n)[farbe=schwarz]
und wir sind noch nicht fertig. 	
*/	


fix_right(N,N1,not_done):-
    color(N)=black,
    color(left(N))=red,
    color(right(N))=red,
    color(left(right(N)))=red,
    !,
    derive(N,N1),
    color(N1)=red,
    color(left(N1))=black,
    color(right(N1))=black.
    
fix_right(N,N1,not_done):-
    color(N,black),
    left(N,M), color(M,red),
    right(N,O),color(O,red),
    color(P,red),left(Q,P),right(N,Q),
    !,
    derive(N,N1),
    color(N1,red),
    color(M1,black),left(N1,M1),
    color(O1,black),right(N1,O1).
    



    
/* there is a special type of 
functions are really just named path fragments */
color(node(C,_,_,_,_),C).
left(node(_,L,_,_,_),L).
right(node(_,_,_,_,R),R).

swapxy(f(X,Y,V),f(Y,X,V)).

swapxy(F,FS):-
    x(F) = y(FS),
    y(F) = y(FS),
    v(F) = v(FS).
    
swapxy(F,FS):-
    FS = subst(F,[x,y],[y,x]).
    
/*
  N.B. the "functions" x and y are not applied. We are interested
  in the functions themselfs, as they can be seen as 
  relative positions or paths. Or maybe even pointers.
*/    


%subst(F,[P|Ps],[Q|Qs],FF):-

    
    


derive(O,D):-
    unifiable(O,D,List),
    derive(List).

derive([]).
derive([O=D|List]):-
    put_attr(D,a,O),
    derive(List).

a:attr_unify_hook(O, D):-
	(	var(O)
	->	O=D
	;	true
	).

eval_formular(A=B):- 
    eval_term(A,AA),
    eval_term(B,BB),
    AA=BB.

eval_term(A,A):-
    var(A),!.
eval_term(color(N),C):-
    !,
    eval_term(N,M),
    color(M,C).    
eval_term(left(N),C):-
    !,
    eval_term(N,M),
    left(M,C).    
eval_term(right(N),C):-
    !,
    eval_term(N,M),
    right(M,C).        
eval_term(A,A).    