:- module(builder__edges,
	[	edge_label/4, 
		set_edge_label/4, 
		clear_edge_label/3,
		target_depends/2,
		clear_dependencies/1,
		net_dependency/2,
		net_dependencies/2,
		dependencies/2,
		clear_graph/0
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

:- dynamic '$target_depends'/2,
		   '$target_depends_inv'/2.


clear_graph:-
	retractall('$edge_label__by_client'(_,_,_,_)),
	retractall('$edge_label__by_from'(_,_,_,_)),
	retractall('$edge_label__by_to'(_,_,_,_)),
	retractall('$target_depends'(_,_)),
	retractall('$target_depends_inv'(_,_)).

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
	(	available(Target), \+ available(Dep)
	->	throw(adding_illigal_dep(Target,Dep))
	;	true
	),
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
    	(	retract('$target_depends_inv'(Dep,Target)),
    		erase(Ref)
    	)
    ).


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
	erase(Ref),
	retract('$target_depends_inv'(M,N)).
erase_redundant_edge_x(_,_,_,_).
	
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

/*Zyklentest wird durchgeführt bei jedem request:
ein request X->Y eines obsoleten Targets Y fügt eine "wait"-Kante hinzu.
diese Kante kann typ i oder ii. Sie schließt einen Zyklus, wenn
im Wait-Graphen X von Y aus ereichbar ist.
*/

/*
Beobachtungen:
- Wenn Ein Zyklus gefunden wird, dann gilt per definition: für jeden beteiligten client c:
 entweder c ist der anfragende thread, oder c wartet (direkt oder indirekt) auf den anfragenden thread.
- Daraus folgt: ich kann jedem beteiligten Thread eine Nachricht schicken!!
- Der Graph kann -- so wie das im Prinzip auch im augenblick passiert -- abstrahiert werden zur Betrachtung von Threads
  die aufeinander warten. Nur die Targets an den "Übergängen" sind interessant.
- Unterscheide zwei Fälle:
 a) Zyklus enthält Typ iii Kante. Dann haben wir keinen "echten" Abhängigkeitszyklus.
   Das Problem läßt sich lösen, in dem Das Target mit dem obsoleten Lock sich aus dem Zyklus "zurückzieht", und dann das
   erste Target im Zyklus erneut anfordert.
 b) Zyklus enthält nur Typ i und ii Kanten. Dann haben wir einen "echten" Abhängigkeitszyklus. Um das Problem zu lösen,
   müssen alle beteiligten Targets vom selben Client berechnet werden.
- Die angefragte Kante kann Teil von mehreren Zyklen sein.
  Davon kann aber nur einer ein echter Abhängigkeitszyklus sein.
  Die zum einem Client gehörenden Typ i und ii Kanten bilden jeweils einen linearen Pfad. Anderenfalls würde
  ein Client gerade auf mehrere Dinge gleichzeitig warten, und das ist unmöglich. Der Zyklus setzt sich aber aus fragmenten dieser
  linearen Pfade zusammen. Wenn es nirgendwo eine Verzweigung gibt, kann die angefragte Kante auch nicht mehr als einen Zyklus schließen.

retreat and reserve:
when a real cycle is detected, all but one client must "reatreat" i.e. abort builds of all targets that are part of the cycle and rerequest the outermost one.
At the same time, to keep them from starting to build again, the targets on the cycle need to be "reserved" for the reamining thread that did not retreat.
reservation could be represented by an extra substate of the "building" state.

Doing cycle Builds:
possible solution if executed on a single thread: Idea of two sets Pending and Done.
starting a build: target is put in the pending set. once it is done, put it in the Done set.
once the pending set is empty, mark all targets in the done set as clean *SIMULTANEOUSLY*. (similar as with fp builds.)
This should keep all invariances satisfied during the build and still preserves a correct dependency realtion. 
*/