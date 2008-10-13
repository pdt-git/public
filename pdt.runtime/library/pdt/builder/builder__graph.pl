:- module(builder__graph,
	[	edge_label/4, 
		set_edge_label/4, 
		clear_edge_label/3,
		target_depends/2,
		clear_dependencies/1,
		net_dependency/2,
		net_dependencies/2,
		dependencies/2,		
		update_target_state/2,
		current_target_state/2,
		clear_graph/0,
		store_graph/1,
		load_graph/1		
	]
).

/*
'$edge_label__by_client'(Client,From,To,Label)
'$edge_label__by_from'(From,To,Client,Label)
'$edge_label__by_to'(To,From,Client,Label)
*/

:- dynamic 
	'$edge_label__by_client'/4,
	'$edge_label__by_from'/4,
	'$edge_label__by_to'/4.

:- dynamic 
	'$edge_label__by_client'/5,
	'$edge_label__by_from'/5,
	'$edge_label__by_to'/5.


:- dynamic '$target_depends'/2,
		   '$target_depends_inv'/2.

:- dynamic '$target_depends'/3,
		   '$target_depends_inv'/3.

:- dynamic '$target_state'/2.

:- dynamic '$target_state'/3.

current_target_state(Target,State):-    
    (	'$target_state'(Target,S)
    *->	State=S
    ;	State=state(idle,obsolete)
    ).

update_target_state(Target,NewState):-	
	%writeln(update_target_state(Target,NewState)),
	thread_self(Me),
	(	Me \== build_arbiter
	->	throw(only_arbiter_should_modify_state(Me,Target,NewState))
	;	NewState==state(idle,obsolete)
    ->  retractall('$target_state'(Target,_))    	
    ;   retractall('$target_state'(Target,_)),    	
    	assert('$target_state'(Target,NewState))
    ).



clear_graph:-
	retractall('$target_state'(_,_)),
	retractall('$edge_label__by_client'(_,_,_,_)),
	retractall('$edge_label__by_from'(_,_,_,_)),
	retractall('$edge_label__by_to'(_,_,_,_)),
	retractall('$target_depends'(_,_)),
	retractall('$target_depends_inv'(_,_)).
store_graph(Num):-
	forall('$target_state'(A,B),assert('$target_state'(Num,A,B))),
	forall('$edge_label__by_client'(A,B,C,D),assert('$edge_label__by_client'(Num,A,B,C,D))),
	forall('$edge_label__by_from'(A,B,C,D),assert('$edge_label__by_from'(Num,A,B,C,D))),
	forall('$edge_label__by_to'(A,B,C,D),assert('$edge_label__by_to'(Num,A,B,C,D))),
	(	Num==0
	->	spyme
	;	true
	),
	forall('$target_depends'(A,B),assert('$target_depends'(Num,A,B))),
	forall('$target_depends_inv'(A,B),assert('$target_depends_inv'(Num,A,B))).
%load_graph(Num):-
%	clear_graph,
%	forall(retract('$target_state'(Num,A,B)),assert('$target_state'(A,B))),
%	forall(retract('$edge_label__by_client'(Num,A,B,C,D)),assert('$edge_label__by_client'(A,B,C,D))),
%	forall(retract('$edge_label__by_from'(Num,A,B,C,D)),assert('$edge_label__by_from'(A,B,C,D))),
%	forall(retract('$edge_label__by_to'(Num,A,B,C,D)),assert('$edge_label__by_to'(A,B,C,D))),
%	forall(retract('$target_depends'(Num,A,B)),assert('$target_depends'(A,B))),
%	forall(retract('$target_depends_inv'(Num,A,B)),assert('$target_depends_inv'(A,B))).	
load_graph(Num):-
	clear_graph,
	(	target_depends(a,b)
	->	spyme
	;	true
	),
	forall('$target_state'(Num,A,B),assert('$target_state'(A,B))),
	forall('$edge_label__by_client'(Num,A,B,C,D),assert('$edge_label__by_client'(A,B,C,D))),
	forall('$edge_label__by_from'(Num,A,B,C,D),assert('$edge_label__by_from'(A,B,C,D))),
	forall('$edge_label__by_to'(Num,A,B,C,D),assert('$edge_label__by_to'(A,B,C,D))),
	forall('$target_depends'(Num,A,B),assert('$target_depends'(A,B))),
	forall('$target_depends_inv'(Num,A,B),assert('$target_depends_inv'(A,B))),
	(	findall(A,(target_depends(a,b),A=1),As),
		length(As,2)
	->	spyme
	;	true
	).	



edge_label(From,To,Client,Label):-
    (	nonvar(From)
    ->	'$edge_label__by_from'(From,To,Client,Label)
    ;	nonvar(To)
    ->	'$edge_label__by_to'(To,From,Client,Label)
    ;	'$edge_label__by_client'(Client,From,To,Label)
    ).

set_edge_label(From,To,Client,Label):-
    (	ground(From-To-Client-Label)
	->	clear_edge_label(From,To,Client),
		add_dependency(From,To),
		assert('$edge_label__by_from'(From,To,Client,Label)),
		assert('$edge_label__by_to'(To,From,Client,Label)),
		assert('$edge_label__by_client'(Client,From,To,Label))
	;	throw(error(instantiation_error,_))
	).

clear_edge_label(From,To,Client):-
    (	ground(From)
    ->	forall(
    		retract('$edge_label__by_from'(From,To,Client,_)),
    		(	retract('$edge_label__by_to'(To,From,Client,_)),
				retract('$edge_label__by_client'(Client,From,To,_))
    		)
    	)
    ;	ground(To)
    ->	forall(
    		retract('$edge_label__by_to'(To,From,Client,_)),
    		(	retract('$edge_label__by_from'(From,To,Client,_)),
				retract('$edge_label__by_client'(Client,From,To,_))
    		)
    	)
    ;	ground(Client)
    ->	forall(
    		retract('$edge_label__by_client'(Client,From,To,_)),
    		(	retract('$edge_label__by_from'(From,To,Client,_)),
				retract('$edge_label__by_to'(To,From,Client,_))
    		)
    	)
  	;   throw(error(instantiation_error,_))
	).
/* 
target-target-client relations:

This relation is a partial function of the form

(FromTarget,ToTarget,Client) --> label

FromTarget is a Target Key or NIL.
ToTarget is a Target Key.
Client is a ThreadAlias
label is EITHER wait, build or lock. 




should only be accessed by the arbiter thread.

query/update label: 
label(?FromTarget,?ToTarget,?Client,-CurrentLabel)
update_label(?FromTarget,?ToTarget,?Client,+NewLabel)

remove label:
clear_label(+FromTarget,+ToTarget,+Client)

invariant: if there is C such that label(A,B,C) != NIL, then there is a Dependency A -> B
updating a label adds a dependency.
clear_dependencies also clears all outgoing labels
*/



/* this will only add an edge if it is not redundant
   in addition it will look for and erase any other edge that may become
   redundant by adding this one.
   This is rather expensive, I yet have to find out
   whether it is worth the effort.
 */
add_dependency__expensive(Target,Dep):-  
	%begin debug
	% checking basic dep invariant.
	%(	available(Target), \+ available(Dep)
	%->	throw(adding_illigal_dep(Target,Dep))
	%;	true
	%),
	%end debug  
    (	net_dependency(Target,Dep)
    ->	true
    ;  	erase_redundant_edge(Target,Dep),
    	assert('$target_depends'(Target,Dep)),
		assert('$target_depends_inv'(Dep,Target))
    ).
  
add_dependency__cheap(Target,Dep):-    
    assert('$target_depends'(Target,Dep)),
	assert('$target_depends_inv'(Dep,Target)).
    
add_dependency(Target,Dep):-
	add_dependency__expensive(Target,Dep).    
    
	


target_depends(T1,T2):-
    (	nonvar(T1)
    ->	'$target_depends'(T1,T2)   	
	;	'$target_depends_inv'(T2,T1)
	).


clear_dependencies(Target):-
    clear_edge_label(Target,_,_),
    forall(
    	clause('$target_depends'(Target,Dep),_,Ref),    	
    	(	uberspy(Target,Dep),
    		retract('$target_depends_inv'(Dep,Target)),
    		erase(Ref)
    	)
    ).

/* This is the old, obsolete implementation. Does not work with cyclic graphs.
   see PDT-305.
   New implementation: see bottom.
erase_redundant_edge(K1,K2):-
    gensym('$visited',V),dynamic( V/1),
    gensym('$visited',W),dynamic( W/1),
	call_cleanup(erase_redundant_edge_x(K1,K2,V,W),(abolish(V/1),abolish(W/1))).
	
erase_redundant_edge_x(K1,K2,V,W):-
	%mark all nodes reachable from K2 and K2 itself
	forall(net_dependency_X(K2,_,V),true),
	VV=..[V,K2],
	assert(VV),
	%If N is K1 or a node that can reach K1
	%and if there is an edge N->M where M is marked,
	%then this edge is redundant.
	%There can be at most one such edge, if
	%we checked each time before adding an edge.
	(	N=K1
	;	net_dependency_X_inv(K1,N,W)
	),
	clause('$target_depends'(N,M),_,Ref),	
	VVV=..[V,M],
	call(VVV),
	!,
	uberspy(N,M),
	erase(Ref),
	retract('$target_depends_inv'(M,N)).	
erase_redundant_edge_x(_,_,_,_).

*/
uberspy(A,B):-
	(	(A==c,B==d)
	->	spyme
	;	true
	).
	
	
net_dependency(T1,T2):-
    gensym('$visited',V),
    dynamic( V/1),    
    (	nonvar(T1)
	->	call_cleanup(net_dependency_X(T1,T2,V),abolish(V/1))		
	;	call_cleanup(net_dependency_X_inv(T2,T1,V),abolish(V/1))
	).
net_dependency_X(T1,T3,V):-
	VV=..[V,T2],
	'$target_depends'(T1,T2),
	\+ VV,
	assert(VV),
	(	T3=T2
	;	net_dependency_X(T2,T3,V)
	).

net_dependency_X_inv(T1,T3,V):-
    VV=..[V,T2],
	'$target_depends_inv'(T1,T2),
	\+ VV,
	assert(VV),
	(	T3=T2
	;	net_dependency_X_inv(T2,T3,V)
	).


net_dependencies(L,R):-
    (	nonvar(L)
    ->	findall(M,net_dependency(L,M),R)
    ;	findall(M,net_dependency(M,R),L)
    ).

dependencies(L,R):-
    (	nonvar(L)
    ->	findall(M,target_depends(L,M),R)
    ;	findall(M,target_depends(M,R),L)
    ).

%----------------------------------------------------------------------------------------------------------
% reimplementation of the dep graph optimization.
% Not thread safe, should only be run by the arbiter!
%
% To understand what's going on, see wiki
% https://sewiki.iai.uni-bonn.de/research/pdt/developers/architecture/subsystems/pdtcore/buildsystem/avoidredundantdependencies
% See PDT-305.

:- dynamic '$mark'/2. %edge marks applied during bw-search. 
:- dynamic '$mark'/1. %node marks applied during fw-search.

% assumes A->B was inserted and is not redundant itsefl.
erase_redundant_edge(A,B):-	
	setup_and_call_cleanup(
		bw_search(A,B), % start with the bw search
		ignore(fw_search(B,A,A)), % then do the fw search
		clear_marks % clean up afterwards.
	).

mark_dep(A,B):-
	(	'$mark'(A,B)
	->	spyme,writeln(doh),
		throw(edge_already_marked(A,B))
	;	assert( '$mark'(A,B) )
	).
	
mark_node(A):-
	(	'$mark'(A)
	->	spyme,writeln(doh),
		throw(node_already_marked(A))
	;	assert( '$mark'(A) )
	).	
	
dep_marked(A,B):-'$mark'(A,B).

node_marked(A):-'$mark'(A).

clear_marks:-
	retractall('$mark'(_,_)),
	retractall('$mark'(_)).


bw_search(A,B):-	
	mark_dep(A,B), % the inserted edge is always marked. See PDT-305.
	bw_search_X(A,B).	
% initial call should be with A->B beeing the inserted edge.	
bw_search_X(A,B):-	
	(	dep_marked(_,A) % A already visited?
	->	true
	;	A==B % did we arrive back at the inserted edge?
	->	true
	;	%otherwise, mark incoming edges (if there are any) and recurse.
		forall(
			target_depends(Pred,A),
			(	mark_dep(Pred,A),
				bw_search_X(Pred,B)
			)
		)
	).


% implementation note:
% This predicate succeeds only if a redundant edge is found and removed.
% It shouldn't leave any choice points. 
% Be sure to fail in terminal cases if no redundancy is found.
% If one is found, succeed and destroy all remaining choices.
% in particular, do not leave choices after a successfull recursive call.
%
% initial call should be with A->B beeing the inserted edge.
% Prev should always be the top node in the path/stack. Initialize with A to 
% avoid the inserted edge being "identified" as redundant.
fw_search(B,Prev,A):-
	(	node_marked(B) % B already visited?
	->	fail
	;	A==B % did we areive back at the inserted edge?
	->	fail
	;	redundant_incoming_edge(X,B,Prev) % is there a redundant incoming edge?
	->  % be done with it.
		% retract should leave no choices. If it does, something is wrong.
		retract('$target_depends'(X,B)),
		retract('$target_depends_inv'(B,X))		
	;	% otherwise, mark the node and check successors. Cut on success.
		mark_node(B),
		target_depends(B,Succ),
		fw_search(Succ,B,A),
		!
	).
	
redundant_incoming_edge(X,Y,Prev):-
	target_depends(X,Y), %X is predecessor of Y
	X \== Prev, %X is not on the current path
	dep_marked(X,Y2), % There is a marked edge X->Y2
	Y2 \== Y. % Y2 is not Y

spyme.	