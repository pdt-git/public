:- module(builder__client,
	[	pdt_request_target/1, 
		pdt_request_targets/1, 
		pdt_invalidate_target/1,
		pdt_with_targets/1,
		pdt_with_targets/2,		
		pdt_restart_arbiter/0,		
		pdt_fp_enqueue/2	
	]
).

:- use_module(library(pif_observe2)).
:- use_module(library('pef/pef_base')).
:- use_module(library('util/progress')).
:- use_module(library(builder__messages)).


/* build strategy hooks */

:- multifile
	build_hook/1,
	fp_process_hook/1,
	fp_seed_hook/1.

/* build target configuration hooks */	

:- multifile		
	target_container/2, % induces containment hierarchie
	target_file/2, % associatei file with target.
	target_mutable/2, % mark a target as mutable.
	fix_point_target/1 % whether FP/BFS Strategie should be used for a target.
	
/* client state */	
:- dynamic 
	'$has_lock'/1, % cache locks.
	'$building'/1, % build stack (DFS-Builds)
	'$fp_building_target'/1, %build-list (BFS-Builds)
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
	
	
	
pdt_with_targets(Goal):-
    pdt_with_targets([],Goal).

pdt_with_targets(Ts,Goal):-
    pdt_builder:asserta('$has_lock'('$mark'),Ref),
    call_cleanup(
    	(	pdt_request_targets(Ts),
    		Goal
    	),
    	pdt_builder:release_targets(Ref)
    ).
    
release_targets(Ref):-
    clause('$has_lock'(T),_,LockRef),
    (	LockRef==Ref
    ->	erase(Ref),
    	!
    ;  	erase(LockRef),
		thread_self(Me),
		send_message_client_target(T,rel(Me)),% release message with two or one arg?		
		fail
    ).
release_targets(Ref):-
    throw(failed(error(release_targets(Ref)))).

mutable(Target):-
	'$mutable'(Target).    

% this is called only once per target to infer whether it is mutable.
mutable_X(Target):-
    (	target_mutable(Target,Mutable)
    ->	Mutable==true
    ;   target_file(Target,File),
    	pef_source_path_query([path=Path/*,include_pattern=IP,exclude_pattern=EP*/]),
    	atom_prefix(File,Path)
    ).
    


    



fp_request_target(Target):-
    fp_building_target(Target),
    !,
    thread_self(Me),
    client_log(Target,already_building),
     (	building_target(Building),mutable(Target)
    ->	client_send_message(Building,depend(Me,Target))	
    ;	true
    ).
fp_request_target(Target):-    
    '$has_lock'(Target),
    !,
    client_log(Target,already_locked),
    thread_self(Me),
    (	building_target(Building),mutable(Target)
    ->	client_send_message(Building,depend(Me,Target))	
    ;	true
    ),    
    my_debug(builder(debug),"target already granted: ~w ~n",[Target]).
fp_request_target(Target):-
    thread_self(Me),
    (	building_target(Building),mutable(Target)
    ->	client_send_message(Building,depend(Me,Target))	    	
	;	true
	),
    
    client_send_message(Target,request(lock(Me,[]))),	
	client_get_message(Msg),	
	my_debug(builder(debug),"Thread ~w received message ~w.~n",[Me,Msg]),
    (	Msg==grant(Target)
    ->	asserta('$has_lock'(Target),Ref),
	    my_debug(builder(debug),"added lock for target ~w, ref=~w~n",[Target,Ref]) 		    
    ;	Msg==implied(Target)
    ->	true 		    
    ;	Msg=rebuild(Target,OldDeps)
    ->  request_target_keys(OldDeps),
    	fp_build_target(Target)
    ;	Msg=error(Target,E)
    ->	throw(error(target_error(Target,E)))
    ;	Msg=obsolete(Target,Targets)
    ->	throw(error(target_obsolete(Target,Targets)))
    ;	Msg=fail(Target)
    ->	throw(error(target_failed(Target)))
    ;	Msg=fp_busy(Target)
    ->	throw(fp_busy(Target))
    /*;	Msg=cycle(Target) %only for debugging. this should be checked on the client side.
    ->	(	building_target(Target)
    	->	throw(wie_komm_ich_denn_hier_hin)
    	;	throw(nicht_meins(Target))
    	)*/
    ;	throw(error(unexpected_message(Msg,wait_for_read_lock(Target))))
    ).





fp_building_target(T):-'$fp_building_target'(T).

fp_build_target(Target):- 
	assert('$fp_building_target'(Target)),
	client_log(Target,add_to_build),   	       
    pef_clear_record(Target),    
    target_key(TargetName,Target),
    (	'$fp_running'
    ->  push_target(Target),    
    	forall(fp_seed_hook(TargetName),true),
    	pop_target,
    	client_log(Target,not_starting__already_running)
    ;	assert('$fp_running'),
    	push_target(Target),    
    	forall(fp_seed_hook(TargetName),true),
    	pop_target,
    	fp_run(Target)
    ).

pdt_fp_enqueue(Job,TargetName):-    
    target_key(TargetName,Target),
    (	'$fp_queue'(Job,Target)
    -> 	client_log(Target,ignore_already_enqueued(Job))  	
    ;	fp_enqueue(Job,Target)
    ).
fp_enqueue(Job,Target):-            
    assert('$fp_queue'(Job,Target)),assert('$fp_queue_inv'(Target,Job)),
    client_log(Target,add_to_queue(Job)).    

fp_dequeue(Job,Target):-
    fp_building_target(Target),
    retract('$fp_queue_inv'(Target,Job)),retract('$fp_queue'(Job,Target)),
    !,    
    client_log(Target,removed_from_queue(Job)).
fp_job_in_queue(Job):-
    (	var(Job)
    ->	fp_building_target(Target),'$fp_queue_inv'(Target,Job)
    ;  	'$fp_queue'(Job,Target), fp_building_target(Target)
    ).



fp_run(Target):-
	\+ fp_job_in_queue(_),
	!,
	client_log(Target,not_starting__queue_empty),
	thread_self(Me),	
    forall(
    	retract('$fp_building_target'(Target2)),
    	(	client_log(Target2,removed_from_building),
    		client_block_add_message(Target2,mark_clean(Me))
    	)
    ),    
    client_block_commit,
    retract('$fp_running').
fp_run(Target):-
    %assert('$fp_running'),
    call_cleanup(fp_run__loop(Target),Catcher,fp_run__cleanup(Target,Catcher)),
    %client_log(Target,not_starting__queue_empty),
    fp_request_target(Target).

fp_run__loop(Target):-       
	client_log(Target,starting_fp_iteration),
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
    !,   
    client_log(Target,finished_fp_iteration).
    
fp_done:-
    \+ fp_job_in_queue(_).
	
fp_run__cleanup(Target,Catcher):-
    client_log(Target,fp_cleanup(Catcher)),
    retract('$fp_running'),
    thread_self(Me),
    (	Catcher==fail
    ->	Msg=fail
    ;	Catcher=exception(E)
    ->	Msg=error(E)
    ;	Msg=mark_clean(Me)
    ),    
    
    forall(
    	retract('$fp_building_target'(Target2)),
    	(	client_log(Target2,removed_from_building),
    		client_block_add_message(Target2,Msg)
    	)
    ),
    client_block_commit,
    !.
fp_run__cleanup(Target,Catcher):-
	throw(failed(fp_run__cleanup(Target,Catcher))).
    
    
    
    


    
request_target(Target):-
    (	\+ ground(Target)
    ->	throw(error(target_must_be_ground))
    ;	true
    ), 
    
    % (minor?) optimization:
    % if the client does currently not build anything
    % and if it already holds a lock on the requested target,
    % the request can be safely ignored.
    '$has_lock'(Target), 
    \+ building_target(_)
    !.    
request_target(Target):-
    thread_self(Me),   	 	
    (	building_target(From)
    ->	true
    ;	From = []
    ),
    send_message_client_target(Target,req(From,Me)),
	next_client_message(Sender,Msg)
	(	Msg==grant(Target)
    ->	asserta('$has_lock'(Target))	    
    ;	Msg==implied(Target)
    ->	true	    
    ;	Msg=build(Target,OldDeps)
    ->  request_target_keys(OldDeps),
    	build_target(Target),    	
    	request_target(Target)
    ;	Msg=error(Target,E)
    ->	throw(error(target_error(Target,E)))
    ;	Msg=obsolete(Target,Targets)
    ->	throw(error(target_obsolete(Target,Targets)))
    ;	Msg=fail(Target)
    ->	throw(error(target_failed(Target)))
    ;	Msg=cycle(Target)
    ->	throw(error(cycle(Target)))    
    ;	throw(error(unexpected_message(Msg,wait_for_read_lock(Target))))
    ).


%%
% pdt_request_target(+Target)
% request a target.
% 
%
%  Make sure the information associated with Target is present and up to date.
% If necessary, the calling thread will wait until the information is built.
%
% 
% The calling thread obtains a "read lock" on the specified target.
% As long as a target is locked, it can not be built.
% The targets are released once the surrounding pdt_with_targets/1 or pdt_with_targets/2 call exits.
% If no such call exists, this predicate behaves as pdt_with_targets( [Target],true).



pdt_request_target(T):-
    pdt_request_targets([T]).

pdt_request_targets(Ts):-
	(	'$has_lock'('$mark')
    ->	ensure_builder_is_running,
    	request_targets(Ts)
    ;	pdt_with_targets( Ts,true)
    ).
request_targets([]).
request_targets([T|Ts]):-
	target_key(T,K),
	(	'$fp_target'(K)
	->	fp_request_target(K)
	;	request_target(K)
	),
	pdt_report_progress(K),
	request_targets(Ts).
request_target_keys([]).
request_target_keys([K|Ks]):-	
	(	'$fp_target'(K)
	->	fp_request_target(K)
	;	request_target(K)
	),
	pdt_report_progress(K),
	request_target_keys(Ks).






push_target(Target):-
    asserta('$building'(Target)).

pop_target:-
    retract('$building'(_)),
    !.

building_target(Target):-
    '$building'(Target),
    !.

build_target(Target):-
    target_key(TargetTerm,Target),
    pef_clear_record(Target),
    pef_start_recording(Target),
    push_target(Target),
    /*profile_push(build(Target)),*/    
    (	catch(
    		call_cleanup(
    			(	debug(builder(build(TargetTerm)),"Building target ~w~n.",[TargetTerm]),
    				forall(build_hook(TargetTerm),true),
    				debug(builder(build(TargetTerm)),"Done with target ~w~n.",[TargetTerm])
    			),
    			(	/*profile_pop,*/
    				pop_target,
    				pef_stop_recording
    			)
    		),
    		E,
    		(	
    			client_send_message(Target,error(E)),
    			throw(E)
    		)
    	)    		
    ->	thread_self(Me),
    	client_send_message(Target,mark_clean(Me))
    ;	client_send_message(Target,fail)
    ).



%%
% pdt_invalidate_target(+Target)
% invalidate a target.
% 
% Marks the information associated with Target as obsolete.
pdt_invalidate_target(Target):-
    ensure_builder_is_running,
    target_key(Target,Key),
    thread_self(Me),
	client_send_message(Key,mark_dirty(Me)).


/*
idea: moving target terms between stack and heep seems to dominate the build
system overhead, in particular as it happens rather often.
Let's try this:
Instead of identifying a target by a term, use some integer. A clause reference maybe.
Resolving targets to references and vice versa is done on the client threads. 
The arbiter is only uses numerical references. No target term ever enters the queue.
This should speed things up noticable.

One thing we need to take care of, though, is target mutability. Currently, the arbiter
calls hooks to find out wether a target is mutable or not. 
But this  could be avoided. The mutability is a constant property. So we can infer it once and
then cache it on the heap.
*/

% argument is the target term.
% clause references are used by the arbiter to refer to targets.
% the arbiter however does not know nore care it is dealing with clause references.
% these facts never accessed by the arbiter.
:- dynamic '$target'/1.

% succeeds if the argument is a reference to a mutable target.
% these facts are created by the client threads.
% The arbiter only reads them.
:- dynamic '$mutable'/1.

% succeeds if the first argument is the key of a target that uses the second argument as mux.
% these facts are created by the client threads.
% The arbiter only reads them.
:- dynamic '$target_mux'/2.

% succeeds if the argument is the key of a target that uses fix point iteration.
% these facts are created by the client threads.
% The arbiter only reads them.
:- dynamic '$fp_target'/1.

% succeeds if the first argument is the key of a target 
% is contained in the target with the second key
% The arbiter only reads them.
:- dynamic '$target_container'/2.


target_key(T,K):-
    (	clause('$target'(T),_,K)
    ->	true
    ;	ground(T)
    ->	assert('$target'(T),K),
    	(	mutable_X(T)
    	->	assert('$mutable'(K))
    	;	true
    	),
    	(	target_mux(T,Mux)
    	->	assert('$target_mux'(K,Mux))
    	;	true
    	),
    	(	fix_point_target(T)
    	->	assert('$fp_target'(K))
    	;	true
    	),
    	(	target_container(T,Container)
    	->	target_key(Container,ContainerKey),
    		%add_dependency(ContainerKey,K) %FIXME: dangerous! Should only be done by the arbiter.
    		assert('$target_container'(K,ContainerKey)) 
    	;	true
    	)
    ;	throw(doof(T,K))	
    ).
	