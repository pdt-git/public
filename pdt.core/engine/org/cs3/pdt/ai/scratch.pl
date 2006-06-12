wrap(w(writeln(A)),A).

wrap(w(catch(G,_,true)),A):-
    member(G,[writeln(A),writeln(A:A)]).


data(writeln(nein)).

unify(A,A).

unwrap(w(Goal),Goal).

builtin(A):-
    current_predicate(_,A),
    predicate_property(A,built_in).
seltsam(_):-
    member(_,[1,2,3]).
    
    
gemein:-
    unify(w(writeln(hähä)),WrappedGoal),
    w(Goal)=WrappedGoal,
    call(Goal).
    
    
gemeiner:-
    unwrap(_,Goal),
    call(Goal).
nichsoschlimm(HGoal):-
    unify(Goal,HGoal),
    call(Goal).
    
kompliziert(Module):-
	wrap(w(Goal),Data),
	data(Data),
	Module:Goal.

:- module_transparent maybe/1.

maybe(Goal):-
		call(Goal).
maybe(_).

write_all([]).
write_all([A|As]):-
    writeln(A),
    write_all(As).



:-call_all([writeln(a),writeln(b),true]).
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
	
	
list_of_as([]).
list_of_as([a|L]):-
    list_of_as(L).
    
call_all([]).
call_all([A|As]):-
    call(A),
    call_all(As).
    