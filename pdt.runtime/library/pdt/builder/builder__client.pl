:- module(builder__client,
	[	pdt_request_target/1, 
		pdt_request_targets/1, 
		pdt_invalidate_target/1,
		pdt_with_targets/1,
		pdt_with_targets/2,		
		pdt_fp_enqueue/2	
	]
).

:- use_module(library('pef/pef_base')).
:- use_module(library('util/progress')).
:- use_module(builder__messages).




/*
unterschiede zur da:

Ein paar Dinge sind mir gerade klar geworden, die in der DA nicht optimal (teilweise auch falsch!) dargestellt sind.

"Berechnungspfad?"
Der Berechnungs-graph ist nicht notwendig ein pfad - im falle einer BFS-Strategie kann es sich durchaus um einen Baum handeln.-

Man muss sich klar machen, dass die bld-Kanten nicht notwendig bedeuten, dass der Client gerade hieran arbeitet, sondern
lediglich, dass ein entsprechender Auftrag erteilt wurde.

----
"Allgemeines zum Client-Verhalten"
Das kommt in der Arbeit nicht so richtig rüber, deshalb hier noch mal mit anderen Worten:

Der Arbeitsablauf auf der Client seite ist folgender:
Client fragt: Kann ich Target A benutzen?
Server sagt: mach mal erst dies oder das, und dann frag nochmal.
Dieser "Dialog" wiederholt sich solange, bis der Server sagt: klar, hier hast du ein Read-Lock auf A.

Arbeitsaufträge haben die Form
- build(x) - "berechne x neu"
- retreat([x1,x2,...,xn])"zieh dich von den Targets x1,...,xn zurück" (gebe locks frei, annuliere berechnungsaufträge)
 
 Weiterhin kann der Server folgendes antworten:
 - "Ok, du kriegst das Lock"
 - "Du hast das Lock schon (implizit)" Entweder indirektes Lock, oder der Client baut das Lock gerade. 
 - "Es gab ein Problem."

Man beachte, dass während der Client einen Auftrag bearbeitet, er möglicherweise andere Targets benötigt und anfragt, 
infolgedessen er weitere Arbeitsaufträge erhält. Die Reihenfolge, in der diese abgearbeitet werden, ist Sache des Clients.
Hier kommen die erwähnten BFS vs DFS-Strategien zum tragen.

----
"Alte Abhängigkeiten"
Dieses Thema wurde in der DA nicht erwähnt. Es geht hier vor allem um situationen, in denen "Meta-"Targets verwendet 
werden. Beispiel: parse(workspace) repräsentiert das Ergebnis der ersten Parserstufe für ALLE dateien im Workspace. 
parse(workspace) hängt implizit von allen parse(eine/date/im/workspace) targets ab.
Was geschieht, wenn eine solche gelöscht wird? Das konkrete parse(datei) Target wird als obsolet markiert, 
genauso das parse(workspace). Die Strukturen sind aber noch im Model enthalten, sie werden erst entfernt, wenn das parse(datei) 
erneut angefordert wird. 
Aber wann geschieht das? Normalerweise sollte während der berechnung von parse(workspace) die Abhängigkeit zu parse(datei) 
festgestellt werden. Da letztere datei jedoch gelöscht wurde, ist dies hier nicht möglich.
Also: bei der neuberechnung eines Targets müssen auch die vorherigen Abhängigkeiten berücksichtigt werden.
Der Server erreicht das folgenermaßen. Wird ein obsoletes Target angefordert, dann wird zunächst untersucht, ob dieses Target 
obsolete Abhängigkeiten hat. Die erste solche "dep" Kante wird entfernt, dem Client wird aufgetragen, das entsprechende Target 
neu zu berechnen. Der Client wird danach das abhängige Target erneut anfragen, und das Spiel beginnt von vorne, solange bis
alle alten Abhängigkeiten überprüft und entfernt sind. Dann erst erhält der CLient den Auftrag zum Build des 
abhängigen Targets. 

>> Reicht nicht auch das Löschen aus?
Im prinzip ja, nur müssten dann zusätzliche markierungen verwendet werden, um race conditions zu verhindern. 

Ist aber eine Überlegung wert.

>> Warum übergeben wir dem Client nicht direkt eine liste *aller* alten Abhängigkeiten?
Auch das ist prinzipiell möglich. Die andere Lösung erscheint mir einfacher: 

Die Entscheidung, ob eine konfluente BLD-Kante einen Zyklus schließt erübrigt 
sich client seitig: im falle eines DFS Builds ist dies immer der Fall, im Falle eines BFS-Builds spielt es keine Rolle.
Wenn wir dem Client jedoch mehrere Build-Aufträge gleichzeitig erteilen, kann es auch bei einem DFS Build zu konfluenten, 
nicht-zyklischen BLD-Graphen kommen.  

Insgesamt macht die entscheidung für die einzelaufträge das Protokoll einfacher und "lokaler"
Es muss Server- wie Clientseitig wenig über den Zustand der Konversation 
reflektiert werden. Mein Bauch sagt mir, dass das ne gute Sache ist. 
Andererseits ist Overhead  ne schlechte sache. 

Ist auf jedenfalls auch ne überlegung wert.
--------
Build Stack und BFS-Builds

Wir verwenden den Build-Stack auch während BFS-Builds, zusätzlich zu den in 10.4.2 beschriebenen
Datenstrukturen.
Der Stack wird benötigt, um nachzuvollziehen, welches Target gerade im Moment beackert wird.
Diese information ist notwendig, um bei geschachtelten requests ein abhängiges Target angeben zu können.  
*/

/* build strategy hooks */

:- multifile
	build_hook/1,
	fp_process_hook/1,
	fp_seed_hook/1.

/* build target configuration hooks */	

:- multifile		
	target_container_hook/2, % induces containment hierarchie
	target_file_hook/2, % associate file with target.
	target_mutable_hook/2, % mark a target as mutable.
	fix_point_target_hook/1. % whether FP/BFS Strategie should be used for a target.
	
/* client state */	
:- dynamic 
	'$has_lock'/1, % locks we hold.
	'$building'/1, % build stack (DFS-Builds, vgl. 10.4.1)
				   % we also use the build stack during BFS-Build to track the "current" build.
	'$fp_building_target'/1, %build-list (BFS-Builds,  vgl. 10.4.2)
	'$fp_running'/0, %Flag indicating if FP-Build is running.
	'$fp_queue'/2, %Job-Queue (BFS-Builds)
	'$fp_queue_inv'/2.
:- thread_local 
	'$has_lock'/1,
	'$building'/1, 
	'$fp_building_target'/1,
	'$fp_running'/0, 
	'$fp_queue'/2, 
	'$fp_queue_inv'/2.

% meta predicates.	
:- module_transparent pdt_with_targets/2, pdt_with_targets/1.
	


%%
% pdt_invalidate_target(+Target)
% invalidate a target.
% 
% Marks the information associated with Target as obsolete.
pdt_invalidate_target(Target):-
    target_key(Target,Key),
    thread_self(Me),
	client_send_message(Key,mark_dirty(Me)).

%%
% pdt_with_targets(:Goal).
% Holds all locks aquired during the execution of Goal 
% until Goal is finished.
pdt_with_targets(Goal):-
    pdt_with_targets([],Goal).

%%
% pdt_with_targets(+Targets, :Goal).
% Same as pdt_with_targets((pdt_request_targets(Targets,Goal)).
pdt_with_targets(Ts,Goal):-
    builder__client:push_mark,
    call_cleanup(
    	(	builder__client:pdt_request_targets(Ts),
    		Goal
    	),
    	builder__client:pop_locks
    ).
    
%%
% pdt_request_target(+Target)
% request a lock for the given Target.
%
% If the thread is not currently running a pdt_with_targets/1 goal,
% the lock is released imidiatly. The call can still be used to make sure
% that a target is consistent at a given POINT in time.
pdt_request_target(T):-
    pdt_request_targets([T]).

%%
% pdt_request_targets(+Targets).
% Same as pdt_request_target/1, but requests a list of targets.
pdt_request_targets(Ts):-
	(	marked
    ->	request_targets(Ts)
    ;	pdt_with_targets( Ts,true)
    ).


    
request_targets([]).
request_targets([T|Ts]):-
	target_key(T,K),
	request_target(K),
	pdt_report_progress(K),
	request_targets(Ts).




% one request to rule them all,
% i.e. DFS aswell as BFS targets :-)
request_target(RequestedTarget):-
	% TODO:
	% avoid unecessary overhead. If the client already has a lock 
	% on the requested target, it only has to contact the arbiter
	% if it is currently building (so the dep edge is added).
	% Also, at least the client does not have to wait for a reply in this
	% situation. Think this over. Not added for now.	
	(	building_target(From)
	->	true
	;	From = []
	),
	thread_self(Me),
	send_message_client_target(Me,RequestedTarget,req(From,Me)),
	repeat,
		next_client_message(_,Msg),
		(	Msg = build(BuildTarget)
		->	start_build(BuildTarget),
			fail
		;	Msg = error(Error)
		->	throw(Error)
		;	Msg = implied(ImpliedLock)
		->	ImpliedLock == RequestedTarget
		;	Msg = grant(GrantedLock)
		->	push_lock(GrantedLock),
			GrantedLock == RequestedTarget
		;	throw(error(unexpected_message(Msg), requesting(RequestedTarget)))
		),
	!.	
		


push_lock(Target):-
	asserta('$has_lock'(Target)).

push_mark:-
	asserta('$has_lock'(mark)).
	
pop_locks:-
	thread_self(Me),
	repeat,
		retract('$has_lock'(L)),
		(	L == mark
		->	true
		;	send_message_client_target(L,rel(Me)),
			fail
		),
	!.

marked:-
	'$has_lock'(mark).

	
start_build(Target):-
	(	'$fp_target'(Target)
	->	bfs_build(Target)
	;	dfs_build(Target)
	).

%--------------
% Build-Stack
%--------------
push_target(Target):-
    asserta('$building'(Target)).

pop_target:-
    retract('$building'(_)),
    !.

building_target(Target):-
    '$building'(Target),
    !.

%-------------
% DFS-Build (vgl. 10.4.1)
%-------------


dfs_build(Target):-
    target_key(TargetTerm,Target),
    pef_clear_record(Target),
    pef_start_recording(Target),
    push_target(Target),
    (	catch(
    		call_cleanup(
				forall(build_hook(TargetTerm),true),    				
    			(	pop_target,
    				pef_stop_recording
    			)
    		),
    		E,
    		(	
    			send_message_client_target(Target,error(E)),
    			throw(E)
    		)
    	)    		
    ->	send_message_client_target(Target,success)
    ;	send_message_client_target(Target,error(build_failed(Target))),
    	throw(build_failed(Target))
    ).



%-------------
% Build-List
%-------------

fp_building_target(T):-
	'$fp_building_target'(T).

fp_add_target(T):-
	assert('$fp_building_target'(T)).
	
fp_clear_targets(Msg):-	
	findall(
		Target-Msg,
		retract('$fp_building_target'(Target)),
		Block
	),
	send_block_client_target(Block).


%-------------
% Job Queue
%-------------
pdt_fp_enqueue(Job,TargetName):-    
    target_key(TargetName,Target),
    (	'$fp_queue'(Job,Target)
    -> 	true  	
    ;	fp_enqueue(Job,Target)
    ).
fp_enqueue(Job,Target):-            
    assert('$fp_queue'(Job,Target)),assert('$fp_queue_inv'(Target,Job)).    

fp_dequeue(Job,Target):-
    fp_building_target(Target),
    retract('$fp_queue_inv'(Target,Job)),retract('$fp_queue'(Job,Target)),
    !.

fp_job_in_queue(Job):-
    (	var(Job)
    ->	fp_building_target(Target),'$fp_queue_inv'(Target,Job)
    ;  	'$fp_queue'(Job,Target), fp_building_target(Target)
    ).



%-------------
% BFS-Build (vgl. 10.4.2)
%-------------

fp_build_target(Target):- 
	fp_add_target(Target),
	pef_clear_record(Target),    
    target_key(TargetName,Target),
    (	'$fp_running'
    ->  push_target(Target),    
    	forall(fp_seed_hook(TargetName),true),
    	pop_target    	
    ;	assert('$fp_running'),
    	push_target(Target),    
    	forall(fp_seed_hook(TargetName),true),
    	pop_target,
    	fp_run(Target)
    ).



fp_run(Target):-
	(	fp_done
	->	fp_clear_targets(success),
    	retract('$fp_running')
    ;   call_cleanup(
    		fp_run__loop,
    		Catcher,
    		fp_run__cleanup(Target,Catcher)
    	)
    ).

fp_run__loop:-       
	repeat,
		fp_dequeue(Job,TargetKey),		
		push_target(TargetKey),
		pef_start_recording(TargetKey),
		call_cleanup(
			forall(fp_process_hook(Job),true),
			(	pop_target,
				pef_stop_recording
			)
		),		
    	fp_done,
    !.
    
fp_done:-
    \+ fp_job_in_queue(_).
	
fp_run__cleanup(Target,Catcher):-
    retract('$fp_running'),    
    (	Catcher==fail
    ->	Msg=error(build_failed(Target))
    ;	Catcher=exception(E)
    ->	Msg=error(E)
    ;	Msg=success
    ),    
    fp_clear_targets(Msg).
    
    
    
    





/*
idea: moving target terms between stack and heep seems to dominate the build
system overhead, in particular as it happens rather often.
Let's try this:
Instead of identifying a target by a term, use some integer. 
Resolving targets to keys and vice versa is done on the client threads. 
The arbiter is only uses numerical references. No target term ever enters the queue.
This should speed things up noticable.

We usedynamic predicates '$target_key'/2 and '$key_target'/2 to establish the mapping.
in both directions.
*/
:- dynamic '$target_key'/2, '$key_target'/2.


/*
The target configuration hooks will be executed exactly once per target, 
the results will be cached in the following dynamic predicates
*/

% succeeds if the argument is a reference to a mutable target.
:- dynamic '$mutable'/1.


% succeeds if the argument is the key of a target that uses fix point iteration.
:- dynamic '$fp_target'/1.

% succeeds if the first argument is the key of a target 
% is contained in the target with the second key
% The arbiter only reads them.
:- dynamic '$target_container'/2.

:- mutex_create(pdt_builder_client_target_key).

% the single point for obtaining a key for a target or vice versa
%
% about synchronisation:
% easiest thing would be to mux the whole thing. 
% On the other hand, reads will occur far more often than inserts.
% Updates do not happen at all. 
% So I figured it would be best to only mux the actual insert. 
% This is bought by an extra lookup per insert, but should reduce the overhead
% of reads, since no synchronisation is necessary there.
target_key(T,K):-
	(	nonvar(K)
	->	'$key_target'(K,T)
	;	\+ ground(T)
	-> throw(error(instantiation_error,_))
	;	'$target_key'(T,K)
	->	true
	;	with_mutex(
			pdt_builder_client_target_key,
			target_key__new_target(T,K)
		)
	).
	
		
target_key__new_target(T,K):-
	% since only writes are muxed, we need to
	% query the table once more to see if 
	% the target has been created while we were waiting
	% for the mux.
	(	'$target_key'(T,K)
	->	true
	;	gen_target_key(K),
		assert('$target_key'(T,K)),
		assert('$key_target'(K,T)),
		(	target_mutable(T)
    	->	assert('$mutable'(K))
    	;	true
    	),    	
    	(	fix_point_target(T)
    	->	assert('$fp_target'(K))
    	;	true
    	),
    	(	target_container(T,Container)
    	->	target_key(Container,ContainerKey),
    		assert('$target_container'(K,ContainerKey)) 
    	;	true
    	)
    ).	
	
gen_target_key(K):-
	flag('$pdt_builder__client_gen_target_key',K,K+1).	


target_mutable(Target):-
    (	target_mutable_hook(Target,Mutable)
    ->	Mutable==true
    ;   target_file_hook(Target,File),
    	pef_source_path_query([path=Path]),
    	atom_prefix(File,Path)
    ).
    
fix_point_target(T):-
	fix_point_target_hook(T).
	
target_container(T,Container):-
	target_container_hook(T,Container).