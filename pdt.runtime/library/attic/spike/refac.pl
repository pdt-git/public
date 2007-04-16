/*
orig_create(Input,Term).
orig_lookup(Fileref,Termref,Term).
orig_store(Term).
orig_arg(ArgNum,Term,ArgVal).
orig_functor(Term,Name,Arity).
orig_property(Term,Prop,Val).
orig_set_property(Term0,Prop,Val,Term1).

new_create(Input,TID).
new_lookup(Fileref,Termref,TID).
new_store(TID).
new_arg(ArgNum,TID,ArgID).
new_functor(TID,Name,Arity).
new_property(TID,Prop,Val).
new_set_property(TID,Prop,Val).

*/

assuming(T,Goal):-
    assert(T),
    call_cleanup(Goal,retract(T)).
    
    
    
%transaction(Goal):-
    
t(E0):-
	get_nondet(P,X),
	set_prop(E0,P,X,E1),
	check(E1),
	store(E1).
    
tt(Id):-
	get_nondet(P,X),
	set_prop(Id,P,X),
	(	check(Id)
	-> 	store(Id)
	;  	unset_prop(Id,P,X),
		fail
	).
tt(Id):-
	get_nondet(P,X),
	transaction( permanent,
		(	set_prop(Id,P,X),
			check(Id)
		)
	).
%---------------------------------------	
	
t(E0,E1):-
	set_prop(E0,P,X,E1),
	recurse(E1,E2),
	store(E2).

recurse(E1,E2):-
	get_prop(E1,child,C0),
	set_prop(E1,child,C1),
	recurse_child(C0,C1).

recurse_child([],[]):-!.
recurse_child(C0,C2):-
    set_prop(C0,P,X,C1),
    recurse(C1,C2).

tt(Id):-
    transaction(
		(	set_prop(Id,P,X),
			recurse(Id),
			commit(ID)
		)
	).

recurse(Id):- 
% data flow analysis hopefully reveals that C0 and C1 
% refer to different states of the same entity, so the 
% following two lines can be ommited.
%
% In fact, they MUST be ommited in this situation! 
% Property values cannot be further instantiated after 
% set_prop.
% This may proove to be a really tricky case...
%	get_prop(E1,child,C0),
%	set_prop(E1,child,C1),
% see findings below: these two "redundant" lines carry important information!
	get_prop(Id,child,Child),
	commit(Child),
	recurse_child(Child).

% we assume that there is in fact some NIL entity ID, since the NDDS also used this
% convention. 
recurse_child(nil):-!.
recurse_child(Id):-
    set_prop(Id,P,X),
    recurse(Id).

%--------------------------	

% Yet another problem:
% deep vs shallow store.
% a store operation in NDDS always operates on one term and its subterms.
% a PEF transaction may operate on arbitrary terms.
% We have to avoid mixing non-permanent with permanent modifications.
% Actually this may be more simple than it seems. 
% If a property of a child term is updated and if this update is intended to
% be permanent, then - the the ndds - there needs to be an update in the 
% parent term, too. See "redundant" lines in the above example. They actually give
% us an important hint, namely that child term C was updated. So if changes to E 
% are made permanent, we also have to include any changes made to C.
%
% This also shows, that we need to know the property keys that are relevant for the 
% traversal of a term.	




%--------------------------
t(E0):-
    repeat,
    	get_non_det(P,X),
    	set_prop(E0,P,X,E1),
    	check(E1),
    	store(E1),
    	done,
    !.
/*    
t(E0, F1):-
	set_prop(E0,P,X,E1),
   	set_prop(F0,P,X,F1),
   	check(E1),
    store(E1),

   	check(F1),
    store(F1),   	
*/

goal:-writeln(g1).
goal:-writeln(g2).
x:-
	call_cleanup(
		( goal, fail),
		Result,writeln(Result)),
	fail.