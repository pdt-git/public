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


 
:- use_module(library(pif_observe2)).
:- use_module(library('pef/pef_base')).

/* hooks */
:- dynamic 
	build_hook/1,
	invalidate_hook/1,
	delete_hook/1,
	target_group/2.


 

:- multifile
	build_hook/1,
	invalidate_hook/1,
	delete_hook/1,
	target_group/2.

:- dynamic '$has_lock'/1.
:- thread_local '$has_lock'/1.

/* associate state with targets */
:- dynamic target_state/2.

 
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

%%
% pdt_request_target(+Target)
% request a target.
% May only be called within a pdt_with_targets/1 or pdt_with_targets/2 call.
%
%  Make sure the information associated with Target is present and up to date.
% If necessary, the calling thread will wait until the information is built.
%
% The calling thread obtains a "read lock" on the specified target.
% As long as a target is locked, it can not be built.
% The targets are released once the surrounding pdt_with_targets/1 or pdt_with_targets/2 call exits.
%

pdt_request_target(T):-
    (	'$has_lock'('$mark')
    ->	ensure_builder_is_running,
    	request_target(T)
    ;	throw(error(not_within_pdt_with_targets))
    ).
request_target(Group):-
    setof(Target,target_group(Target,Group),Targets),
    !,
    pdt_request_targets(Targets).
request_target(Target):-
    '$has_lock'(Target),
    !,
    my_debug(builder(debug),"target already granted: ~w ~n",[Target]).
request_target(Target):-
    thread_self(Me),
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
	
	
	my_debug(builder(debug),"Thread ~w received message ~w.~n",[Me,Msg]),
    (	Msg==grant(Target)
    ->	asserta('$has_lock'(Target),Ref),
	    my_debug(builder(debug),"added lock for target ~w, ref=~w~n",[Target,Ref]) 	
    ;	Msg==rebuild(Target)
    ->  build_target(Target),
    	pdt_request_target(Target)
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

pdt_request_targets(Ts):-
	(	'$has_lock'('$mark')
    ->	ensure_builder_is_running,
    	request_targets(Ts)
    ;	throw(error(not_within_pdt_with_targets))
    ).
request_targets([]).
request_targets([T|Ts]):-
	request_target(T),
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
    



check_locks:-
    forall(
    	(	'$has_lock'(Target),
    		Target \== '$mark'
    	),
    	(	current_target_state(Target,State),
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






build_target(Target):-
    pef_clear_record(Target),
    pef_start_recording(Target),
    (	catch(
    		call_cleanup(
    			forall(build_hook(Target),true),
    			pef_stop_recording
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
pdt_invalidate_target(Group):-
	target_group(_Target,Group),
	!,
	throw(cannot_invalidate_group(Group)).
pdt_invalidate_target(Target):-
    ensure_builder_is_running,
	thread_send_message(build_arbiter,msg(Target,mark_dirty)).



current_target_state(Target,State):-
    target_state(Target,_),
    !,
    target_state(Target,State).
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
process_message(Target,Event):-
    current_target_state(Target,State),    
    (	target_transition(State,Event,Action,NewState,Target)
    ->  my_debug(builder(transition(Target)),"Target: ~w, Transition: ~w, ~w ---> ~w,~w~n",[Target,State,Event,Action,NewState]),
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
execute_action(grant([]),_Target).
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
/*
 FIXME: 
 Probably it would be best to make sure that all
 cascading invalidation messages get processed before any
 other messages, (requests in particular).
 
 This is not that difficult. 
 when starting to invalidate, perform a state change into some
 state invalidating. within this state, all incoming messages are frozen.
  After calling the invalidate_hook, enqueue some "invalidation_done" marker.
  When this marker is read, the waiting messages are unfrozen.
  
  Problem is, that this cuts through all the target states.
  We have to add a global state component. So
  I decided to ignore the "problem", as long as possible.
  
  Possible workaround: when invalidation is requested, check if the 
  request comes in from the arbiter thread. IN this case, process it right away, 
  i.e. without using the message loop. This way we make sure that cascading 
  invalidation requests are processed before any other requests.
  I don't know how to model this in our state chart diagram...
*/
execute_action(invalidate,Target):-
	my_debug(builder(debug),"invalidating target: ~w~n",[Target]),
    pif_notify(builder(Target),invalid),    
    forall(invalidate_hook(Target),true),
    forall(
    	(	target_group(Target,Group),
    		pif_notify(builder(Group),invalid),
    		invalidate_hook(Group)
    	),
    	true
    ).
execute_action(rebuild(Thread),Target):-
	my_debug(builder(debug),"rebuilding target: ~w~n",[Target]),
	pif_notify(builder(Target),start(Thread)),
	thread_send_message(Thread,builder_msg(rebuild(Target))).
execute_action(report_cycle(Thread),Target):-
	thread_send_message(Thread,	builder_msg(cycle(Target))).
execute_action(notify_done,Target):-
	my_debug(builder(debug),"target done: ~w~n",[Target]),
    pif_notify(builder(Target),done).
execute_action(ackn_remove(W),Target):-
    my_debug(builder(debug),"sending message to ~w:~w~n",[W,builder_msg(removed(Target))]),
	thread_send_message(W,builder_msg(removed(Target))).    



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

target_transition(state(A, outdated, Ls,SLs,Ws), 				request(T), 	report_cycle(T),		state(A, outdated, Ls, SLs, Ws) ,Target):-
    closes_cycle(T,Target).
target_transition(state(A, pending(T1), Ls,SLs,Ws),				request(T2), 	report_cycle(T2),		state(A, pending(T1), Ls, SLs ,Ws) ,Target):-
    closes_cycle(T2,Target).
target_transition(state(idle, available,[], SLs,Ws), 			request(T), 	grant([T]), 				state(reading, available,[T],SLs,Ws) ,_Target).
target_transition(state(reading, available, Ls, SLs,Ws), 		request(T), 	grant([T]), 			state(reading, available, [T|Ls],SLs,Ws) ,_Target).
target_transition(state(idle, outdated, [],SLs,[]), 			request(T), 	rebuild(T),				state(idle, pending(T) , [],SLs, []),_Target). 
target_transition(state(reading, outdated, Ls,SLs,Ws), 			request(T), 	[],						state(reading, outdated , Ls, SLs,[T|Ws]),_Target).
target_transition(state(idle, pending(P) , [],SLs, Ws),			request(W), 	[],		 				state(idle, pending(P) , [], SLs,[W|Ws]),_Target).

target_transition(state(idle, pending(_) , Ls,SLs,Ws),			fail,		 	report_failure(Ws),		state(idle, outdated, Ls,SLs,[]),_Target).
target_transition(state(idle, pending(_) , Ls,SLs,Ws), 			error(E),	 	report_error(Ws,E),		state(idle, outdated, Ls,SLs,[]),_Target).

%FIXME: do we need to react some way when invalidation is requested during pending?
target_transition(state(idle, pending(P) , Ls,SLs,Ws), 			mark_dirty,	 	[],						state(idle, pending(P) , Ls,SLs,Ws),_Target).
target_transition(state(A, available, Ls,SLs,[]), 				mark_dirty, 	invalidate,				state(A, outdated , Ls,SLs,[]),_Target).
target_transition(state(A, outdated, Ls,SLs,Ws),				mark_dirty, 	[], 					state(A, outdated , Ls,SLs,Ws),_Target).

target_transition(state(idle, available , [],SLs,[]),			mark_clean, 	[],						state(idle, available, [],SLs,[]),_Target).
target_transition(state(idle, _ , [],SLs,[]), 					mark_clean, 	[notify_done],			state(idle, available, [],SLs,[]),_Target).
target_transition(state(idle, _ , [],SLs,Ts), 					mark_clean, 	[notify_done,grant(Ts)],state(reading, available, Ts,SLs,[]),_Target).

target_transition(state(reading, available, Ls,SLs,Ws),			release(T), 	[], 					state(Act, available, Ls2,SLs,Ws),_Target):-
    select(T,Ls,Ls2), 
    (	Ls2 == []
    ->	Act=idle
    ;	Act=reading
    ).
target_transition(state(reading, outdated, [T],SLs,[W|Ws]),		release(T),		rebuild(W),				state(idle, pending(W) , [],SLs, Ws),_Target).
target_transition(state(reading, outdated, Ls,SLs,Ws),			release(T),	 	[],						state(reading, outdated , Ls2,SLs,Ws),_Target):-
    select(T,Ls,Ls2), Ls2 \== [].
target_transition(state(reading, outdated, [L],SLs,[]),			release(L),	 	[],						state(idle, outdated , [],SLs,[]),_Target).

target_transition(state(Act, St, Ls,SLs,Ws),					remove(W),	 	[ackn_remove(W)],		state(Act, St , Ls,SLs,Ws2),_Target):-
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


    
%:-tdebug.
%myspy.
%:-tspy(myspy).
%user:prolog_exception_hook(error(target_error(_, _)),
%                               _, _, _) :-
%            writeln(arsch),trace, fail.

%user:prolog_exception_hook(E, _, _, _) :- 
%            writeln(arsch),
%            var(E),
%            myspy,
%            trace, fail.

%user:prolog_exception_hook(E, _, _, _) :- 
%            writeln(arsch),
%            attvar(E),
%            myspy,
%            trace, fail.

            
%:-debug(builder),debug(builder(_)).             


