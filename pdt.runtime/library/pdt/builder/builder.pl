:- module(pdt_builder,
	[	pdt_request_target/1, 
		pdt_request_targets/1, 
		pdt_invalidate_target/1,
		pdt_with_targets/1,
		pdt_with_targets/2,		
		pdt_restart_arbiter/0,
		pdt_thread_activity/2,
		debugme/0,
		redirect/2,
		my_debug/3
	]
).     
x(A).
:- use_module(library(pif_observe2)).
:- use_module(library('pef/pef_base')).
 
/* hooks */
:- dynamic 
	build_hook/1,
	invalidate_hook/1,
	estimate_hook/3,
	report_progress/1.

/* only used for debugging */
:- dynamic touched/1.
 

:- multifile
	build_hook/1,
	invalidate_hook/1, %FIXME: not used anymore.
	estimate_hook/3,
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
:- dynamic target_state/2, '$target_depends'/2,'$target_depends_inv'/2.
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

    

    
request_target(Target):-
    (	\+ ground(Target)
    ->	spyme,throw(error(target_must_be_ground))
    ;	true
    ), 
    '$has_lock'(Target),
    !,
    (	building_target(Building)
    ->	thread_send_message(build_arbiter,msg(meta,propagate_dependencies(Target,Building)))	
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
	  
	
	%thread_get_message(builder_msg(Msg)),
	(	Target=profile(Target2)
	->	true
	;	Target2=Target
	),
	
	my_debug(builder(debug),"Thread ~w received message ~w.~n",[Me,Msg]),
    (	Msg==grant(Target2)
    ->	asserta('$has_lock'(Target2),Ref),
	    my_debug(builder(debug),"added lock for target ~w, ref=~w~n",[Target2,Ref]), 	
	    (	building_target(Building)
	    ->	thread_send_message(build_arbiter,msg(meta,propagate_dependencies(Target,Building)))	    	
    	;	true
    	)
    ;	Msg==rebuild(Target2)
    ->  build_target(Target2),
    	pdt_request_target(Target2)
    ;	Msg=error(Target2,E)
    ->	throw(error(target_error(Target2,E)))
    ;	Msg=obsolete(Target2,Targets)
    ->	throw(error(target_obsolete(Target2,Targets)))
    ;	Msg=fail(Target2)
    ->	throw(error(target_failed(Target2)))
    ;	Msg=cycle(Target2)
    ->	throw(error(cycle(Target2)))    
    ;	throw(error(unexpected_message(Msg,wait_for_read_lock(Target2))))
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
    	request_target(T)
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
	profile_push(request(T)),
	call_cleanup( request_target(T), profile_pop ),
	request_targets(Ts).


%%  
% pdt_check_locks.
% Verify that all targets locked by the current thread are up to date.
% If the current thread holds a lock for a target that is outdated,
% this predicate will throw the exception "obsolete".
pdt_check_locks(WaitTarget):-
    my_debug(builder(obsolete),"checking locks while waiting for target ~w ~n",[WaitTarget]),      
    (	check_locks
    ->	my_debug(builder(obsolete),"All locks are up to date. (waiting for ~w)~n",[WaitTarget])
    ;	my_debug(builder(obsolete),"found obsolete locks while waiting for target ~w, ~n",[WaitTarget]),
    	handle_obsolete_locks(WaitTarget)
    ).
    
/*
check_locks:-
    	findall(Target,'$has_lock'(Target),Targets),
    	thread_self(Me),    	
    	thread_send_message(build_arbiter,msg(meta,check_available(Me,Targets))),
    	thread_get_message(builder_msg(Msg)),
    	!,
    	(	Msg==yes
    	->	true
    	;	spyme
    	).
*/
check_locks:-
    forall(
    	(	'$has_lock'(Target),
    		Target \== '$mark'
    	),
    	(	current_target_state(Target,State), %FIXME: this should only be run on the arbiter thread!
    		my_debug(builder(obsolete),"checking lock ~w: state is ~w ~n",[Target,State]),
    		(	State=state(_Activity, available, _Locks,_SleepLocks,_Waits)
    		->	my_debug(builder(obsolete),"ok~n",[])
    		;	my_debug(builder(obsolete),"obsolete~n",[]),
    			fail
    		)
    	)    	
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
    pef_clear_record(Target),
    pef_start_recording(Target),
    push_target(Target),
    profile_push(build(Target)),    
    (	catch(
    		call_cleanup(
    			(	debug(builder(build(Target)),"Building target ~w~n.",[Target]),
    				(	setof(step(S,W),estimate_hook(Target,S,W),Steps)
    				->	thread_send_message(build_arbiter,msg(Target,estimate(Steps)))
    				;	true
    				),
    				forall(build_hook(Target),true),
    				debug(builder(build(Target)),"Done with target ~w~n.",[Target])
    			),
    			(	profile_pop,
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
	thread_send_message(build_arbiter,msg(Target,mark_dirty)).



current_target_state(Target,State):-    
    target_state(Target,State),
    !.
current_target_state(_Target,state(idle,outdated,[],[],[])).

update_target_state(Target,state(idle,outdated,[],[],[])):-
    !,
    retractall(target_state(Target,_)).
update_target_state(Target,NewState):-
    retractall(target_state(Target,_)),
    assert(target_state(Target,NewState)).

   

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

run_arbiter:-   
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
		thread_get_message(msg(Target,Event)),
		
		
		catch(
			process_message(Target,Event),
			Error,
			(	%trace,
				report_error(Error),
				retractall(target_state(_,_)),
				throw(Error)
			)
		),		
		Event==stop,
	!,
	report_error(arbiter_quits),
	retractall(target_state(_,_)).
	
report_error(Error):-
    forall(
    	target_state(_,state(_,TargetStatus,_,Threads)),
    	(	TargetStatus=pending(Thread)
    	->	report_error([Thread|Threads],Error)
    	;	report_error(Threads,Error)
    	)
    ).

report_error([],_Error).
report_error([Thread|Threads],Error):-
    thread_send_message(Thread,builder_msg(arbiter_error(Error))),
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
    	
process_message(meta,propagate_dependencies(From,To)):-
	!,
	add_dependency(To,From). %FIXME: misleading variable names...
	%forall(net_dependency(From,Dep),add_dependency(To,Dep)).    	


process_message(Target,Event):-
    current_target_state(Target,State),    
    (	target_transition(State,Event,Action,NewState,Target)
    ->  my_debug(builder(transition(Target)),"Target: ~w,~n~t Transition: ~w, ~w ---> ~w,~w~n",[Target,State,Event,Action,NewState]),
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
    thread_send_message(Thread,builder_msg(grant(Target))),
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
    pif_notify(builder(Target),invalid),    
    %forall(invalidate_hook(Target),true),
    forall(net_dependency(Dependent,Target),
    	process_message(Dependent,mark_dirty) %FIXME: hm... hope this works.
    ).
execute_action(rebuild(Thread),Target):-
	my_debug(builder(debug),"rebuilding target: ~w~n",[Target]),
	pif_notify(builder(Target),start(Thread)),
	clear_dependencies(Target),	
	thread_send_message(Thread,builder_msg(rebuild(Target))).
execute_action(progress_prepare(Ts),Target):-
	progress_report_prepare(Target,Ts).	
execute_action(report_cycle(Thread),Target):-
	thread_send_message(Thread,	builder_msg(cycle(Target))).
execute_action(notify_done,Target):-
	my_debug(builder(debug),"target done: ~w~n",[Target]),
    pif_notify(builder(Target),done),
    progress_report_cleanup(Target).
execute_action(ackn_remove(W),Target):-
    my_debug(builder(debug),"sending message to ~w:~w~n",[W,builder_msg(removed(Target))]),
	thread_send_message(W,builder_msg(removed(Target))).    
execute_action(lock_deps([]),_Target).
execute_action(lock_deps([T|Ts]),Target):-
    
    current_target_state(T,State),
    Event=request(target(Target)),
    target_transition(State,Event,_,NewState,T),
    !,
	my_debug(builder(transition(T)),"Target: ~w,~n~tSilent Transition: ~w, ~w ---> ~w~n",[T,State,Event,NewState]),    
    update_target_state(T,NewState),
	execute_action(lock_deps(Ts),Target).
execute_action(unlock_deps([]),_Target).

execute_action(unlock_deps([T|Ts]),Target):-    
	
	
	current_target_state(T,State),
	Event=release(target(Target)),
    (	target_transition(State,Event,_,NewState,T)	
	->	my_debug(builder(transition(T)),"Target: ~w,~n~tSilent Transition: ~w, ~w ---> ~w~n",[T,State,Event,NewState])
	;	spyme
	),    
    !,
    update_target_state(T,NewState),
	execute_action(unlock_deps(Ts),Target).
execute_action(unlock_deps([T|Ts]),_Target):-
    writeln([T|Ts]),
    spyme.


% state(Activity, Status, Locks,SleepLocks,Waits)
% Activity: idle - there are no locks 
%		or reading - there is at least one read lock 
%
% Status: available(TimeStamp) - the target is up to date, Timestamp is the build time
%		  outdated - the target is outdated
%		  pending(Thread) - the target is beeing rebuild by Thread
%
% Locks: A list of threads that were granted a read lock on this target
%
% SleepLocks: (not used right now) A list of targets that were granted a read lock, but that are currently asleep. 
%
% Waits: A list of threads that are waiting for the target to become available.

add_dependency(Target,Dep):-
    target_key(Target,TargetKey),
    target_key(Dep,DepKey),
    (	'$target_depends'(Target,Dep)
    ->	true
    ;   assert('$target_depends'(TargetKey,DepKey)),
		assert('$target_depends_inv'(DepKey,TargetKey))
    ).
    
	


target_depends(T1,T2):-
    (	nonvar(T1)
    ->	target_key(T1,K1),
    	'$target_depends'(K1,K2),
    	target_key(T2,K2)
	;	target_key(T2,K2),
		'$target_depends_inv'(T2,T1),
		target_key(T1,K1)
	).


clear_dependencies(Target):-
    forall(
    	clause('$target_depends'(Target,Dep),_,Ref),
    	(	retract('$target_depends_inv'(Dep,Target)),
    		erase(Ref)
    	)
    ).



:- thread_local '$visited'/1.
:- dynamic '$visited'/1.	
net_dependency(T1,T2):-
    (	nonvar(T1)
	->	target_key(T1,K1),
		call_cleanup(net_dependency_X(K1,K2),retractall('$visited'(_))),
		target_key(T2,K2)
	;	target_key(T2,K2),
		call_cleanup(net_dependency_X_inv(K2,K1),retractall('$visited'(_))),
		target_key(T1,K1)
	).
net_dependency_X(T1,T3):-
	'$target_depends'(T1,T2),
	\+ '$visited'(T2),
	assert('$visited'(T2)),
	(	T3=T2
	;	net_dependency_X(T2,T3)
	).

net_dependency_X_inv(T1,T3):-
	'$target_depends_inv'(T1,T2),
	\+ '$visited'(T2),
	assert('$visited'(T2)),
	(	T3=T2
	;	net_dependency_X_inv(T2,T3)
	).


net_dependencies(L,R):-
    (	nonvar(L)
    ->	setof(M,net_dependency(L,M),R)
    ;	setof(M,net_dependency(M,R),L)
    ),
    !.
net_dependencies(L,R):-
    (	nonvar(L)
    ->	R=[]
    ;	L=[]
    ).


:- dynamic '$target'/1.

target_key(T,K):-
    (	clause('$target'(T),_,K)
    ->	true
    ;	assert('$target'(T),K)
    ).
    

check_available(Targets):-
    forall(member(Target,Targets),current_target_state(Target,state(_,available,_,_,_))).

    

target_transition(state(A, outdated, Ls,SLs,Ws), 			request(T), 	report_cycle(T),				state(A, outdated, Ls, SLs, Ws),			Target):-
    closes_cycle(T,Target).
target_transition(state(A, pending(T1), Ls,SLs,Ws),			request(T2), 	report_cycle(T2),				state(A, pending(T1), Ls, SLs,Ws) ,		Target):-
    closes_cycle(T2,Target).

%idle --> reading
%target_transition(state(idle, available,[], SLs,Ws), 		request(T), 	grant([T]), 					state(reading, available,[T],SLs,Ws) ,		_Target).
target_transition(state(idle, available,[], SLs,Ws), 		request(T), 	[lock_deps(Deps),grant([T])], 	state(reading, available,[T],SLs,Ws) ,		Target):-
	net_dependencies(Target,Deps),
    check_available(Deps).
target_transition(state(idle, available,[], SLs,Ws), 		request(T), 	report_error([T|Ws],not_all_deps), 	state(idle, outdated,[],SLs,[]) ,		_Target).    

target_transition(state(reading, available, Ls, SLs,Ws),	request(T), 	grant([T]), 			state(reading, available, [T|Ls],SLs,Ws) ,	_Target).
target_transition(state(idle, outdated, [],SLs,[]), 		request(T), 	rebuild(T),				state(idle, pending(T) , [],SLs, []),		_Target). 
target_transition(state(reading, outdated, Ls,SLs,Ws), 		request(T), 	[],						state(reading, outdated , Ls, SLs,[T|Ws]),	_Target).
target_transition(state(idle, pending(P) , [],SLs, Ws),		request(W), 	[],		 				state(idle, pending(P) , [], SLs,[W|Ws]),	_Target).

target_transition(state(idle, pending(_) , Ls,SLs,Ws),		fail,		 	report_failure(Ws),		state(idle, outdated, Ls,SLs,[]),			_Target).
target_transition(state(idle, pending(_) , Ls,SLs,Ws), 		error(E),	 	report_error(Ws,E),		state(idle, outdated, Ls,SLs,[]),			_Target).
target_transition(state(idle, pending(P) , Ls,SLs,Ws),		estimate(Ts),	progress_prepare(Ts),	state(idle, pending(P) , Ls,SLs,Ws),		_Target).	

%FIXME: do we need to react some way when invalidation is requested during pending?
target_transition(state(idle, pending(P) , Ls,SLs,Ws), 		mark_dirty,	 	[],						state(idle, pending(P) , Ls,SLs,Ws),		_Target).
target_transition(state(A, available, Ls,SLs,[]), 			mark_dirty, 	invalidate,				state(A, outdated , Ls,SLs,[]),				_Target).

target_transition(state(A, outdated, Ls,SLs,Ws),			mark_dirty, 	[], 					state(A, outdated , Ls,SLs,Ws),				_Target).

target_transition(state(idle, available , [],SLs,[]),		mark_clean, 	[],						state(idle, available, [],SLs,[]),			_Target).
target_transition(state(idle, _ , [],SLs,[]), 				mark_clean, 	[notify_done],			state(idle, available, [],SLs,[]),			_Target).

%idle --> reading
target_transition(state(idle, _ , [],SLs,Ts), 				mark_clean, 	[lock_deps(Deps),notify_done,grant(Ts)],	state(reading, available, Ts,SLs,[]),		Target):-
	net_dependencies(Target,Deps),
    check_available(Deps).
target_transition(state(idle, _ , [],SLs,Ts), 				mark_clean, 	report_error(Ts,not_all_deps),	state(idle, outdated, [],SLs,[]),		_Target).


% reading --> idle
target_transition(state(reading, available, Ls,SLs,Ws),		release(T), 	Do, 					state(Act, available, Ls2,SLs,Ws),			Target):-
    net_dependencies(Target,Deps),
    select(T,Ls,Ls2), 
    (	Ls2 == []
    ->	Act=idle,Do=[unlock_deps(Deps)]
    ;	Act=reading,Do=[]
    ).

% reading --> idle
target_transition(state(reading, outdated, [T],SLs,[W|Ws]),	release(T),		[unlock_deps(Deps),rebuild(W)],				state(idle, pending(W) , [],SLs, Ws),		Target):-
    net_dependencies(Target,Deps).
target_transition(state(reading, outdated, Ls,SLs,Ws),		release(T),	 	[],						state(reading, outdated , Ls2,SLs,Ws),		_Target):-    
    select(T,Ls,Ls2), Ls2 \== [].

% reading --> idle
target_transition(state(reading, outdated, [L],SLs,[]),		release(L),	 	[unlock_deps(Deps)],					state(idle, outdated , [],SLs,[]),			Target):-
    net_dependencies(Target,Deps).

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
    target_state(Target,state(_,pending(Thread2),_,_,_)),
    thread_depends_thread(Thread2,Thread).    
    
thread_depends_target(Thread,Target):-
    target_state(Target2,state(_,_,_,_,Waiting)),
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
pdt_thread_activity(Thread,build(Target)):-
	target_state(Target,state(_,pending(Thread),_,_,_)).    
pdt_thread_activity(Thread,lock(Target)):-
	target_state(Target,state(_,_,Ls,_,_)),
	memberchk(Thread,Ls).
pdt_thread_activity(Thread,wait(Target)):-
	target_state(Target,state(_,_,_,_,Ws)),
	memberchk(Thread,Ws).


available(Target):-
    current_target_state(Target,state(_, available, _, _, _)).
    
progress_report_prepare(Target,Steps):-    
    progress_report_prepare_X(Steps,Target,0,Sum),
    assert('$progress_total'(Target,Sum)),
    pif_notify(builder(Target),estimate(Sum)).
    
progress_report_prepare_X([],_Target,Sum,Sum).
progress_report_prepare_X([step(S,W)|Steps],Target,Sum0,Sum):-
    Sum1 is Sum0 + W,
    assert('$progress_subproblem'(S,Target,W),Ref),
    assert('$progress_subproblem_inv'(Target,Ref)),
    progress_report_prepare_X(Steps,Target,Sum1,Sum).

progress_report_worked(SubTarget):-
	forall(
		'$progress_subproblem'(SubTarget,Target,W),
		pif_notify(builder(Target),worked(W))
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