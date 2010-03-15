wrap(w(writeln(A)),A).

wrap(w(catch(G,_,true)),A):-
    member(G,[writeln(A),writeln(A:A)]).


data(w(writeln(nein))).
text(w(text)).

unify(A,A).

unwrap(w(Goal),Goal).

builtin(A):-
    current_predicate(_,A),
    predicate_property(A,built_in).
seltsam(_):-
    member(_,[1,2,3]).
    
    
gemein:-
    data(D),
    unwrap(D,Goal),    
    call(Goal).

gemein:-
    text(D),
    unwrap(D,Text),    
    writeln(Text).

metacall(Call):-
	call(Call).
	
invisible(Call,A,B):-
    metacall(Call).

%:-invisible(A=B,A,B),A=writeln(ogottogott),B.
    
sonicht(A,B):-
	A=B.	    
sonicht(writeln(asas),C):-
	call(C).  
	
%:-sonicht(D,E),sonicht(E,D).

dada(A).
    
gemeiner:-
    unwrap(_,Goal),
    call(Goal).
    
kacke([1]).
kacke([M,N|Ns]):-
    kacke([N|Ns]),
     M is N+1.
nichsoschlimm(HGoal):-
    unify(Goal,HGoal),
    call(Goal).
    
kompliziert(Module):-
	wrap(w(Goal),Data),
	data(Data),
	Module:Goal.

%:- module_transparent maybe/1.

maybe(Goal):-
		call(Goal).
maybe(_).

write_all([]).
write_all([A|As]):-
    writeln(A),
    write_all(As).



%:-call_all([writeln(a),writeln(b),true]).
%:-call_all([true,_B|_C]).
    
write_n_times(0,_):-
    !.
write_n_times(N,D):-
    writeln(D),
    M is N-1,
	write_n_times(M,D).

unwise([]).
unwise([G|A]):-
    (var(G)->writeln(none);call(G)),
	unwise(A).
	
%:-B=[a(writeln(1)),a(writeln(2)),_,a(writeln(3))|B],unwise(B).	


write_countdown(0):-
    !.
write_countdown(N):-
    writeln(N),
    M is N+1,
	write_countdown(M).	
	

%:-list_of_as(A).
%:-list_of_as([1,2,3]).
	
list_of_as([]).
list_of_as([a|L]):-
    list_of_as(L).
    
call_all([]).
call_all([A|As]):-
    call(A),
    call_all(As).
    