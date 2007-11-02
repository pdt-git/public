:- module(pdt_builder,
	[	pdt_request_target/1, 
		pdt_request_targets/1, 
		pdt_invalidate_target/1,
		pdt_with_targets/1,
		pdt_with_targets/2,		
		pdt_restart_arbiter/0,
		pdt_builder_info/2,
		pdt_builder_info/3,
		pdt_print_builder_info/3,
		pdt_print_builder_info/2,
		debugme/0,  
		redirect/2,
		my_debug/3
	]
).     
  
:- use_module(library(pif_observe2)).
:- use_module(library('pef/pef_base')).
 
/* hooks */
:- dynamic 
	build_hook/1,
	invalidate_hook/1,
	estimate_hook/3,
	target_file/2,
	target_mutable/2,
	report_progress/1.

/* only used for debugging */
:- dynamic touched/1,'$target_event'/2.


:- multifile
	build_hook/1,
	invalidate_hook/1, %FIXME: not used anymore.
	estimate_hook/3,
	target_file/2,
	target_mutable/2,
	report_progress/1.

/* estimate_hook(Target,Dependency, Weight),
   report_progress(Target)
Used for progress monitoring. 
Clients can request progress monitoring for a particular target. The builder will send progress information
using the pif_observe api. Progress will be reported for a target iff report_progress(Target) succeeds.
Target definition can add clauses to report_progress(Target).

before rebuilding this target, the arbiter will call the report_progress(Target) hook to find out wheter
progress should be reported for this target. If this is the case, it next   calls the estimate_hook to 
get a list of known "sub-problems" of the target. It will automatically filter out the once that are already up-to-date.
For each of the remaining entries, progress will be reported as soon as each entry becomes available. 

*/

/*  '$progress_subproblem'(SubTarget,Target,Weight)
	'$progress_subproblem_inv'(Target,SubTarget)
	'$progress_total'(Target,Sum)
*/
:- dynamic '$progress_subproblem'/3, '$progress_subproblem_inv'/2, '$progress_total'/2.


:- dynamic '$has_lock'/1,'$building'/1.
:- thread_local '$has_lock'/1,'$building'/1.

/* associate state with targets 
These predicates should only be accessed by the arbiter thread.

*/
:- dynamic '$target_state'/2, '$target_depends'/2,'$target_depends_inv'/2.
%:- dynamic '$net_depends_cache'/2, '$net_depends_inv_cache'/2.

 
  my_debug(Topic,Msg,Args):-
      append("[~w:~w:~w] ",Msg,Msg2),
      get_time(Stamp),
      stamp_date_time(Stamp,DT,local),
      date_time_value(time,DT,time(H,M,S)),
      debug(Topic,Msg2,[H,M,S|Args]).

:- module_transparent pdt_with_targets/2, pdt_with_targets/1.

%% pdt_with_targets(+Targets,+Goal).
% holds read locks for the specified list of Targets while executing Goal.
%
% IMPORTANT NOTE: This predicate uses call_cleanup/2 to take care of releasing 
% the locks. Bear in mind that as long as there are any choice points left in Goal, 
% the locks are NOT released. 
%


pdt_with_targets(Goal):-
    pdt_with_targets([],Goal).

pdt_with_targets(Ts,Goal):-
    my_debug(builder(debug),"pdt_with_targets(~w, ~w): ~n",[Ts, Goal]),
    pdt_builder:asserta('$has_lock'('$mark'),Ref),
    my_debug(builder(debug),"added mark clause, ref= ~w ~n",[Ref]),    
    call_cleanup(
    	(	pdt_request_targets(Ts),
    		Goal
    	),
    	pdt_builder:release_targets(Ref)
    ).
    
release_targets(Ref):-
    my_debug(builder(debug),"erasing all locks until marker ~w~n",[Ref]),
    clause('$has_lock'(T),_,LockRef),
    (	LockRef==Ref
    ->	my_debug(builder(debug),"~t found marker, erasing it: ~w~n",[Ref]),
    	erase(Ref),
    	!
    ;  	my_debug(builder(debug),"~t found lock ~w, ref=~w, erasing it. ~n",[T,LockRef]),
    	erase(LockRef),
		my_debug(builder(debug),"~t sending release message to arbiter (target: ~w) ~n",[T]),
		thread_self(Me),		
		thread_send_message(build_arbiter,msg(T,release(Me))),
		fail
    ).
release_targets(Ref):-
    throw(failed(error(release_targets(Ref)))).


spyme.


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
    
    
request_target(Target):-
    (	\+ ground(Target)
    ->	spyme,throw(error(target_must_be_ground))
    ;	true
    ), 
    '$has_lock'(Target),
    !,
    (	building_target(Building),mutable(Target)
    ->	thread_send_message(build_arbiter,msg(Building,depend(Target)))	
    ;	true
    ),    
    my_debug(builder(debug),"target already granted: ~w ~n",[Target]).
request_target(Target):-
    thread_self(Me),
   	(	\+ ground(Target)
    ->	spyme,throw(error(target_must_be_ground))
    ;	true
    ),
   	thread_send_message(build_arbiter,msg(Target,request(Me))),
	my_debug(builder(debug),"sending request to arbiter:~w.~n",[msg(Target,request(Me))]),   	
	repeat,		
		catch(
			call_with_time_limit(5,	thread_get_message(builder_msg(Msg))),
			time_limit_exceeded,
			(	pdt_check_locks(Target),
				fail
			)
		),
	!,
	  
	
	
	my_debug(builder(debug),"Thread ~w received message ~w.~n",[Me,Msg]),
    (	Msg==grant(Target)
    ->	asserta('$has_lock'(Target),Ref),
	    my_debug(builder(debug),"added lock for target ~w, ref=~w~n",[Target,Ref]), 	
	    (	building_target(Building),mutable(Target)
	    ->	thread_send_message(build_arbiter,msg(Building,depend(Target)))	    	
    	;	true
    	)
    ;	Msg==rebuild(Target)
    ->  build_target(Target),
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
    (	'$has_lock'('$mark')
    ->	ensure_builder_is_running,
    	target_key(T,K),
    	request_target(K)
    ;	pdt_with_targets( [T],true)
    ).

pdt_request_targets(Ts):-
	(	'$has_lock'('$mark')
    ->	ensure_builder_is_running,
    	request_targets(Ts)
    ;	pdt_with_targets( Ts,true)
    ).
request_targets([]).
request_targets([T|Ts]):-
	target_key(T,K),
	request_target(K),
	request_targets(Ts).


%%  
% pdt_check_locks.
% Verify that all targets locked by the current thread are up to date.
% If the current thread holds a lock for a target that is outdated,
% this predicate will throw the exception "obsolete".
pdt_check_locks(WaitTarget):-
    my_debug(builder(obsolete),"checking locks while waiting for target ~w ~n",[WaitTarget]),
    thread_self(Me),      
    (	'$obsolete'(Me,_)
    ->	my_debug(builder(obsolete),"found obsolete locks while waiting for target ~w, ~n",[WaitTarget]),
    	handle_obsolete_locks(WaitTarget)
    ;	my_debug(builder(obsolete),"All locks are up to date. (waiting for ~w)~n",[WaitTarget])    
    ).
    
    
handle_obsolete_locks(WTarget):-    
    thread_self(Me),
    
    % tell the arbiter to remove us from the targets wait list.
    my_debug(builder(obsolete),"sending remove request: ~w, ~n",[msg(WTarget,remove(Me))]),
    thread_send_message(build_arbiter,msg(WTarget,remove(Me))),
    % skip all message until receiving acknowlegement.
    
    repeat,
    	thread_get_message(builder_msg(Msg)),
    	(	Msg==removed(WTarget)
    	->	my_debug(builder(obsolete),"remove ackn. received: ~w, ~n",[builder_msg(Msg)])
    	;	Msg=grant(T)
    	->	asserta('$has_lock'(T),Ref),
	    	my_debug(builder(obsolete),"received grant, added lock for target ~w, ref=~w~n",[T,Ref]),
	    	fail
    	;	my_debug(builder(obsolete),"skipping: ~w, ~n",[builder_msg(Msg)]),
    		fail
    	),
    	
    !,
    
	throw(obsolete).    




push_target(Target):-
    asserta('$building'(Target)).

pop_target:-
    retract('$building'(_)).

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
    				(	findall(step(S,W),
    						(	estimate_hook(TargetTerm,StepTerm,W),
    							target_key(StepTerm,S)
    						),
    						Steps
    					)
    				->	thread_send_message(build_arbiter,msg(Target,estimate(Steps)))
    				;	true
    				),
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
    			thread_send_message(build_arbiter,msg(Target,error(E))),
    			throw(E)
    		)
    	)    		
    ->	thread_send_message(build_arbiter,msg(Target,mark_clean))
    ;	thread_send_message(build_arbiter,msg(Target,fail))
    ).



%%
% pdt_invalidate_target(+Target)
% invalidate a target.
% 
% Marks the information associated with Target as obsolete.
pdt_invalidate_target(Target):-
    ensure_builder_is_running,
    target_key(Target,Key),
	thread_send_message(build_arbiter,msg(Key,mark_dirty)).



current_target_state(Target,State):-    
    thread_self(Me),
	(	Me \== build_arbiter
	->	throw(only_arbiter_should_query_state(Me,Target))
	;	'$target_state'(Target,S)
    *->	State=S
    ;	State=state(idle,outdated,[],[],[])
    ).

update_target_state(Target,NewState):-
	thread_self(Me),
	(	Me \== build_arbiter
	->	throw(only_arbiter_should_modify_state(Me,Target,NewState))
	;	NewState==state(idle,outdated,[],[],[])
    ->  retractall('$target_state'(Target,_))
    ;   retractall('$target_state'(Target,_)),
    	assert('$target_state'(Target,NewState))
    ),
    atom_number(Atom,Target), % keys >= 2^24 seem to be a problem.
    recorda(Atom,NewState).

   

stop_arbiter:-
    current_thread(build_arbiter,Status),
    !,
    (	Status==running	
    ->  thread_send_message(build_arbiter,msg(all,stop))
    ;	true
    ),
    thread_join(build_arbiter,ExitStatus),
    my_debug(builder(info),"build_arbiter stopped with status ~w~n",[ExitStatus]).
stop_arbiter.    
    

ensure_builder_is_running:-
    current_thread(build_arbiter,Status),
    !,
    (	Status==running
    ->	true
    ;	stop_arbiter, 
    	start_arbiter
    ).
ensure_builder_is_running:-
	start_arbiter.    

start_arbiter:-
    current_thread(build_arbiter,running),
    !.
start_arbiter:-    
    thread_create(run_arbiter,_,[alias(build_arbiter)]).

pdt_restart_arbiter:-
    stop_arbiter,
    start_arbiter.



open_log(LogDir,Alias,Stream):-
    concat_atom([LogDir,'/',Alias,'.log'],'',Path),
    %my_debug(builder,"trying to open log file for writing: ~w~n",[Path]),
	%flush,
    open(Path,append,Stream),
    %format(Stream,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),
    %my_debug(builder,"successfully opened log file for writing: ~w~n",[Path]),
	flush.

:- module_transparent redirect/2.

redirect(File,Goal):-
    open(File,write,Out),
    open_null_stream(In),
    stream_property(OldIn,alias(current_input)),
    stream_property(OldOut,alias(current_output)),
    set_prolog_IO(In,Out,Out),
    call_cleanup(Goal,
    	(  	set_prolog_IO(OldIn,OldOut,OldOut),
    		close(In),
    		close(Out)
    	)
    ).



/* the arbiter uses the "fast lane" to send messages to itself.
   they are processed before any other pending messages. 
 */
:- dynamic '$fast_lane'/2.


% used to mark threads that hold obsoleted locks.
:- dynamic '$obsolete'/2.

next_message(msg(Target,Event)):-	
	retract('$fast_lane'(Target,Event)),
	!.    
next_message(msg(Target,Event)):-    
    thread_get_message(msg(Target,Event)).

run_arbiter:-   
%    spy(pdt_builder:spyme),
%    trace,
    (	'$log_dir'(LogDir)
	->	thread_self(Me),
		open_null_stream(NullStream),
		open_log(LogDir,Me,LogStream),
		thread_at_exit(
			(	close(NullStream),
				close(LogStream)
			)
		),
		set_prolog_IO(NullStream,LogStream,LogStream)
	;	true
	),
    
	repeat,
		next_message(msg(Target,Event)),
		(	ground(Target)
	    	->	true
	    	;	throw(non_ground_target(Target,Event))
	    ),
	    (	ground(Event)
	    	->	true
	    	;	throw(non_ground_event(Target,Event))
	    ),		
		process_message(Target,Event),			
		Event==stop,
	!,
	report_error(arbiter_quits).
	
report_error(Error):-
    forall(
    	current_target_state(_,state(TargetActivity,_,_,Threads)),
    	(	TargetActivity=building(Thread)
    	->	report_error([Thread|Threads],Error)
    	;	report_error(Threads,Error)
    	)
    ).

report_error([],_Error).
report_error([Thread|Threads],Error):-
    (	Thread=target(Target)
    ->	throw(cannot_report_to_target(Error,Target))
    ;	thread_send_message(Thread,builder_msg(arbiter_error(Error)))
    ),
    report_error(Threads,Error).
    
process_message(all,stop):-!.
process_message(profile(Target),Event):-
    !,
    profile(process_message(Target,Event)).
process_message(meta,check_available(Thread,Targets)):-
	!,
	(	check_available(Targets)
	->	thread_send_message(Thread,builder_msg(yes))
	;	thread_send_message(Thread,builder_msg(no))
	).
    	

process_message(meta,run(Goal)):-
    !,
    once(Goal).

process_message(Target,Event):-
    
    atom_number(Atom,Target), %prob with keys>2^24
    recorda(Atom,Event),
    
    current_target_state(Target,State),
    (	ground(State)
    	->	true
    	;	throw(non_ground_state(Target,State,Event))
    ),    
    
    (	target_transition(State,Event,Action,NewState,Target)
    ->  (	ground(NewState)
    	->	true
    	;	throw(transition_to_non_ground_state(State,Event,Action,NewState,Target))
    	),
    	(	ground(Action)
    	->	true
    	;	throw(transition_with_non_ground_action(State,Event,Action,NewState,Target))
    	),
    	my_debug(builder(transition(Target)),"Target: ~w,~n~t Transition: ~w, ~w ---> ~w,~w~n",[Target,State,Event,Action,NewState]),
    	update_target_state(Target,NewState),
	    (	execute_action(Action,Target)
	    ->	true
	    ;	my_debug(builder(transition(Target)),"action failed ~w (target: ~w)~n",[Action,Target]),
	    	throw(error(action_failed(Target,State,Event,Action)))
	    )
	;	my_debug(builder(transition(Target)),"no transition for state ~w, event ~w (target: ~w)~n",[State,Event,Target]),
		throw(error(no_transition(Target,State,Event)))
	).
 
debugme:-
	my_debug(builder(debug),"ouch~n",[]).

execute_action([],_).
execute_action([Action|Actions],Target):-
    execute_action(Action,Target),
    execute_action(Actions,Target).
execute_action(grant([]),Target):-
    progress_report_worked(Target).
execute_action(grant([Thread|Threads]),Target):-
    (	functor(Thread,target,1)
    ->	true
    ;	thread_send_message(Thread,builder_msg(grant(Target)))
    ),
    execute_action(grant(Threads),Target).
execute_action(report_failure([]),_Target).
execute_action(report_failure([Thread|Threads]),Target):-
    thread_send_message(Thread,builder_msg(fail(Target))),
    execute_action(report_failure(Threads),Target).
execute_action(report_error([],_E),_Target).
execute_action(report_error([Thread|Threads],E),Target):-
    thread_send_message(Thread,builder_msg(error(Target,E))),
    execute_action(report_error(Threads,E),Target).
execute_action(invalidate,Target):-
	my_debug(builder(debug),"invalidating target: ~w~n",[Target]),
	target_key(TargetName,Target), %FIXME: should not be used by the arbiter!!
    pif_notify(builder(TargetName),invalid),     %FIXME   
    forall(target_depends(Dependent,Target),
    	assert('$fast_lane'(Dependent,mark_dirty))
    ).
    
execute_action(obsolete([]),_).    
execute_action(obsolete([L|Ls]),Target):-
	(	functor(L,target,1)
	->	true
	;	'$obsolete'(L,Target)
	->	true
	;	assert('$obsolete'(L,Target))
	),
	execute_action(obsolete(Ls),Target).
execute_action(clear_obsolete(Thread),Target):-
	(	functor(Thread,target,1)
	->	true
	;	retract('$obsolete'(Thread,Target))
	).
execute_action(rebuild(Thread),Target):-
	my_debug(builder(debug),"rebuilding target: ~w~n",[Target]),
	target_key(TargetName,Target), %FIXME: should not be used by the arbiter!!
	pif_notify(builder(TargetName),start(Thread)), %FIXME
	clear_dependencies(Target),	
	thread_send_message(Thread,builder_msg(rebuild(Target))).
execute_action(progress_prepare(Ts),Target):-
	progress_report_prepare(Target,Ts).	
execute_action(report_cycle(Thread),Target):-
	thread_send_message(Thread,	builder_msg(cycle(Target))).
execute_action(notify_done,Target):-
	my_debug(builder(debug),"target done: ~w~n",[Target]),
	target_key(TargetName,Target), %FIXME: should not be used by the arbiter!!
    pif_notify(builder(TargetName),done),
    progress_report_cleanup(Target).
execute_action(ackn_remove(W),Target):-
    my_debug(builder(debug),"sending message to ~w:~w~n",[W,builder_msg(removed(Target))]),
	thread_send_message(W,builder_msg(removed(Target))).    
execute_action(lock_deps([]),_Target).
execute_action(lock_deps([T|Ts]),Target):-  
	assert('$fast_lane'(T,request(target(Target)))),        
	execute_action(lock_deps(Ts),Target).
execute_action(unlock_deps([]),_Target).
execute_action(unlock_deps([T|Ts]),Target):-    		
	assert('$fast_lane'(T,release(target(Target)))),        
	execute_action(unlock_deps(Ts),Target).	
execute_action(unlock_deps([T|Ts]),_Target):-
    writeln([T|Ts]),
    spyme.
execute_action(add_dependency(Dep),Target):-
    add_dependency(Target,Dep).

% state(Activity, Status, Locks,SleepLocks,Waits)
% Activity: idle - there are no locks 
%		or reading - there is at least one read lock 
%	    or building(T) - thread T is currently rebuilding the target. 
%
% Status: available - the target is up to date, Timestamp is the build time
%		  outdated - the target is outdated
%		  pending(Thread) - the target is in the process of becoming available
%
% Locks: A list of threads/targets that were granted a read lock on this target
%
% SleepLocks: (not used right now) A list of targets that were granted a read lock, but that are currently asleep. 
%
% Waits: A list of threads that are waiting for the target to become available.


/* this will only add an edge if it is not redundant
   in addition it will look for and erase any other edge that may become
   redundant by adding this one.
   This is rather expensive, I yet have to find out
   whether it is worth the effort.
 */
add_dependency(Target,Dep):-    
    (	net_dependency(Target,Dep)
    ->	true
    ;  	erase_redundant_edge(Target,Dep),
    	assert('$target_depends'(Target,Dep)),
		assert('$target_depends_inv'(Dep,Target))
    ).
  
    
	


target_depends(T1,T2):-
    (	nonvar(T1)
    ->	'$target_depends'(T1,T2)   	
	;	'$target_depends_inv'(T2,T1)
	).


clear_dependencies(Target):-
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

target_key(T,K):-
    (	clause('$target'(T),_,K)
    ->	true
    ;	ground(T)
    ->	assert('$target'(T),K),
    	(	mutable_X(T)
    	->	assert('$mutable'(K))
    	;	true
    	)
    ;	throw(doof(T,K))	
    ).
    

check_available(Targets):-
    forall(member(Target,Targets),current_target_state(Target,state(_,available,_,_,_))).

    

target_transition(state(A, outdated, Ls,SLs,Ws), 			request(T), 	report_cycle(T),		state(A, outdated, Ls, SLs, Ws),			Target):-
    closes_cycle(T,Target).
target_transition(state(building(T1), A, Ls,SLs,Ws),		request(T2), 	report_cycle(T2),		state(building(T1), A, Ls, SLs,Ws) ,		Target):-
    closes_cycle(T2,Target).

%enter reading
target_transition(state(idle, available,[], SLs,Ws), 		request(T), 	[lock_deps(Deps),
																			grant([T])], 			state(reading, available,[T],SLs,Ws) ,		Target):-
	dependencies(Target,Deps).	
target_transition(state(reading, available, Ls, SLs,Ws),	request(T), 	grant([T]), 			state(reading, available, [T|Ls],SLs,Ws) ,	_Target).
target_transition(state(idle, outdated, [],SLs,[]), 		request(T), 	rebuild(T),				state(building(T), pending(T) , [],SLs, []),Target):-
	(	functor(T,target,1)
	->	throw(target_requests_outdated_dep(T,Target))
	;	true
	). 
target_transition(state(reading, outdated, Ls,SLs,Ws), 		request(T), 	[],						state(reading, outdated , Ls, SLs,[T|Ws]),	Target):-
	(	functor(T,target,1)
	->	throw(target_requests_outdated_dep(T,Target))
	;	true
	).
target_transition(state(building(P), A , [],SLs, Ws),		request(W), 	[],		 				state(building(P), A , [], SLs,[W|Ws]),		Target):-
	(	functor(W,target,1)
	->	throw(target_requests_dep_beeing_build(W,Target))
	;	true
	).

target_transition(state(building(_), _ , Ls,SLs,Ws),		fail,		 	report_failure(Ws),		state(idle, outdated, Ls,SLs,[]),			_Target).
target_transition(state(building(_), _ , Ls,SLs,Ws), 		error(E),	 	report_error(Ws,E),		state(idle, outdated, Ls,SLs,[]),			_Target).
target_transition(state(building(P), A , Ls,SLs,Ws),		estimate(Ts),	progress_prepare(Ts),	state(building(P), A , Ls,SLs,Ws),			_Target).	


target_transition(state(building(P),pending(P),Ls,SLs,Ws),	depend(Dep),	add_dependency(Dep), 	state(building(P), Status , Ls,SLs,Ws),		_Target):-
    (	available(Dep)
    ->	Status=pending(P)
    ;	Status=outdated
    ).


target_transition(state(building(P), pending(P), Ls,SLs,Ws),mark_dirty,	 	[invalidate,obsolete(Ls)],state(building(P), outdated , Ls,SLs,Ws),	_Target).
target_transition(state(A, available, Ls,SLs,[]), 			mark_dirty, 	[invalidate,obsolete(Ls)],state(A, outdated , Ls,SLs,[]),			_Target).

target_transition(state(A, outdated, Ls,SLs,Ws),			mark_dirty, 	[], 					state(A, outdated , Ls,SLs,Ws),				_Target).

target_transition(state(idle, available , [],SLs,[]),		mark_clean, 	[],						state(idle, available, [],SLs,[]),			_Target).
target_transition(state(building(P), pending(P),[],SLs,[]),	mark_clean, 	[notify_done],			state(idle, available, [],SLs,[]),			_Target).
target_transition(state(building(_), outdated,[],SLs,Ws),	mark_clean, 	[],						state(idle, outdated, [],SLs,Ws),			_Target).

%enter reading
target_transition(state(building(_), pending(_),[],SLs,Ts),	mark_clean, 	[lock_deps(Deps),
																			notify_done,
																			grant(Ts)],				state(reading, available, Ts,SLs,[]),		Target):-
	net_dependencies(Target,Deps).    																			
/*target_transition(state(building(_), pending(_),[],SLs,Ts),	mark_clean, 	report_error(Ts,not_all_deps),	
																									state(idle, outdated, [],SLs,[]),	_Target).*/
% exit reading
target_transition(state(reading, available, Ls,SLs,Ws),		release(T), 	Do, 					state(Act, available, Ls2,SLs,Ws),			Target):-
    /*(	functor(T,target,1)
	->	Deps=[]    																			
	;	net_dependencies(Target,Deps)
	),*/
	dependencies(Target,Deps),
    select(T,Ls,Ls2), 
    (	Ls2 == []
    ->	Act=idle,Do=[unlock_deps(Deps)]
    ;	Act=reading,Do=[]
    ).
% exit reading
target_transition(state(reading, outdated, [T],SLs,[W|Ws]),	release(T),		[clear_obsolete(T),
																			unlock_deps(Deps),
																			rebuild(W)],			state(building(W), pending(W) , [],SLs, Ws),Target):-
    /*(	functor(T,target,1)
	->	Deps=[]    																			
	;	net_dependencies(Target,Deps)
	).*/
	dependencies(Target,Deps).
target_transition(state(reading, outdated, Ls,SLs,Ws),		release(T),	 	[clear_obsolete(T)],	state(reading, outdated , Ls2,SLs,Ws),		_Target):-    
    select(T,Ls,Ls2), Ls2 \== [].
% exit reading
target_transition(state(reading, outdated, [L],SLs,[]),		release(L),	 	[clear_obsolete(L),
																			unlock_deps(Deps)],		state(idle, outdated , [],SLs,[]),			Target):-
    /*(	functor(L,target,1)
	->	Deps=[]    																			
	;	net_dependencies(Target,Deps)
	).*/
	dependencies(Target,Deps).
target_transition(state(Act, St, Ls,SLs,Ws),				remove(W),	 	[ackn_remove(W)],		state(Act, St , Ls,SLs,Ws2),				_Target):-
    (	select(W,Ws,Ws2)
    ;	Ws2=Ws
    ).



/*

Cycle checkking:
A thread depends on a target if it waits for it, or if it requests it.
A target depends on a thread if it is pending, and if the thread is working on providing the target.

Invariant: the graph induced by the above relations is always acyclic.
requesting a target constitutes adding an edge. If that edge would close a cylce, an error is reported to the requesting thread.
*/

target_depends_thread(Target,Thread):-
    current_target_state(Target,state(building(Thread2),_,_,_,_)),
    thread_depends_thread(Thread2,Thread).    
    
thread_depends_target(Thread,Target):-
    current_target_state(Target2,state(_,_,_,_,Waiting)),
    member(Thread,Waiting),
    target_depends_target(Target2,Target).


target_depends_target(Target,Target).
target_depends_target(Target1,Target2):-
    target_depends_thread(Target1,Thread),
    thread_depends_target(Thread,Target2).

thread_depends_thread(Thread,Thread).
thread_depends_thread(Thread1,Thread2):-
    thread_depends_target(Thread1,Target),
    target_depends_thread(Target,Thread2).

    
closes_cycle(Thread,Target):-  
    target_depends_thread(Target,Thread),
    !.


%pdt_thread_activity(Thread,status(A)):-
%    current_thread(Thread,A).
pdt_builder_info(Thread,build,Target):-
	current_target_state(Target,state(building(Thread),_,_,_,_)).    
pdt_builder_info(Thread,lock,Target):-
	current_target_state(Target,state(_,_,Ls,_,_)),
	memberchk(Thread,Ls).
pdt_builder_info(Thread,wait,Target):-
	current_target_state(Target,state(_,_,_,_,Ws)),
	memberchk(Thread,Ws).
pdt_builder_info(A,depend,B):-
	target_depends(A,B).
pdt_builder_info(Target,Activity+Status):-
	current_target_state(Target,state(Activity, Status, _, _, _)).
	
pdt_print_builder_info(slice(T),File):-
    pdt_print_builder_info(FT,
    	(	T=FT
    	;	net_dependency(T,FT)
    	;	net_dependency(FT,T)	
    	),
    	File
    ).
pdt_print_builder_info(FT,F,File):-
    thread_self(Me),
    (	Me==build_arbiter
    ->	print_builder_info(FT,F,File)
    ;	thread_send_message(build_arbiter,msg(meta,run(print_builder_info(FT,F,File))))
    ).

print_builder_info(FT,F,File):-   
	tell(File),
	call_cleanup(print_builder_info(FT,F),told).

print_builder_info(FT,F):-   	 
    format("digraph G {~nnode [shape = \"record\"]~n",[]),
    forall(
    	current_thread(Thread,Status),
    	format("\"~w\" [label=\"{~w|~w}\"]~n",[Thread,Thread,Status])
    ),
    forall(
    	(	pdt_builder_info(Target,Activity+Status), 
    		mutable(Target),
    		\+ \+ (Target=FT,once(F))
    	),
    	format("\"~w\" [label=\"{~w|~w|~w}\"]~n",[Target,Target,Activity,Status])
    ),
    forall(
    	(	pdt_builder_info(Thread,Edge,Node1),
    		(	Thread=target(Node0)
    		->	\+ \+ (Node0=FT,once(F))
    		;	Node0=Thread
    		),
    		\+ \+ (Node1=FT,once(F))
    	),
    	(	(	Edge==depend
    		->	Style=dashed,Color=black
    		;	Edge==lock
    		->	Style=solid,Color=green
    		;	Edge==build
    		->	Style=solid,Color=blue
    		;	Edge==wait
    		->	Style=solid,Color=red
    		;	Style=solid,Color=black
    		),
    		format("\"~w\" -> \"~w\" [label=\"~w\",style=~w,color=~w]~n",[Node0,Node1,Edge,Style,Color])
    	)
    ),
	format("}~n",[]).
available(Target):-  
    current_target_state(Target,state(_, available, _, _, _)).
    
progress_report_prepare(Target,Steps):-    
    progress_report_prepare_X(Steps,Target,0,Sum),
    assert('$progress_total'(Target,Sum)),
    target_key(TargetName,Target), %FIXME: should not be used by the arbiter!!
    pif_notify(builder(TargetName),estimate(Sum)).
    
progress_report_prepare_X([],_Target,Sum,Sum).
progress_report_prepare_X([step(S,W)|Steps],Target,Sum0,Sum):-
    Sum1 is Sum0 + W,
    assert('$progress_subproblem'(S,Target,W),Ref),
    assert('$progress_subproblem_inv'(Target,Ref)),
    progress_report_prepare_X(Steps,Target,Sum1,Sum).



progress_report_worked(SubTarget):-
	forall(
		'$progress_subproblem'(SubTarget,Target,W),
		
		(	spyme,
			target_key(TargetName,Target), %FIXME: should not be used by the arbiter!!
			pif_notify(builder(TargetName),worked(W))
		)
	).
	
	

progress_report_cleanup(Target):-
	forall(
		clause('$progress_subproblem_inv'(Target,Ref),_,InvRef),
		(	erase(Ref),
			erase(InvRef)
		)
	),
	retractall('$progress_total'(Target,_)).
	


:-thread_local '$profile'/2,'$profile_contains'/2,'$profile_starts'/2,'$profile_ends'/2,'$profile_current'/1.
:-dynamic '$profile'/2,'$profile_contains'/2,'$profile_starts'/2,'$profile_ends'/2,'$profile_current'/1.
	
profile_push(Target):-
    get_time(Now),
    pef_reserve_id('$profile',T),
    assert('$profile'(Target,T)),
    (	'$profile_current'(CurrentT)
    ->	assert('$profile_contains'(CurrentT,T))
    ;	true
    ),
    assert('$profile_start'(T,Now)),    
    asserta('$profile_current'(T)).
    
profile_pop:-
	get_time(Now),
	retract('$profile_current'(T)),
	assert('$profile_end'(T,Now)).

profile_clear:-
	retractall('$profile'(_,_)),
	retractall('$profile_current'(_)),
	retractall('$profile_contains'(_,_)),
	retractall('$profile_start'(_,_)),
	retractall('$profile_end'(_,_)).
	
target_profile(Target,Time,Netto):-
    '$profile'(Target,T),
    '$profile_start'(T,Start),
    '$profile_end'(T,End),
    Time is End - Start,
    
    findall(CTime,
    	(	'$profile_contains'(T,C),
    		'$profile_start'(C,CStart),
    		'$profile_end'(C,CEnd),
    		CTime is CEnd - CStart
    	),
    	CTimes
    ),
    sum(CTimes,TimeChildren),
    Netto is Time - TimeChildren.

target_profile(Target,Count,STime,SNetto):-
    findall(Time,target_profile(Target,Time,_),Times),
    findall(Netto,target_profile(Target,_,Netto),Nettos),
    length(Times,Count),
    sum(Times,STime),
    sum(Nettos,SNetto).
    
sum(Times,Brutto):-
	sum(Times,0,Brutto).

sum([],Sum,Sum).
sum([Time|Times],Sum0,Sum):-
    Sum1 is Time + Sum0,
    sum(Times,Sum1,Sum).
