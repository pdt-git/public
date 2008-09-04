:- module(builder__arbiter,
	[	ensure_arbiter_is_running/0,
		stop_arbiter/0
	]
).

:- use_module(builder__transitions).
:- use_module(builder__messages).

% runs the arbiter's message dispatch loop.
run_arbiter:-       
	repeat,
		next_target_message(Target,_,Event),
		(	ground(Target)
	    	->	true
	    	;	throw(non_ground_target(Target,Event))
	    ),
	    (	ground(Event)
	    	->	true
	    	;	functor(Event,error,_)
	    	->	true
	    	;	throw(non_ground_event(Target,Event))
	    ),		
		process_message(Target,Event),			
		Event==stop,
	!,
	clear_graph,
	clear_target_states,
	report_error(arbiter_quits).


process_message(meta,_):-!. 
process_message(Target,Event):-        
    current_target_state(Target,State),
    (	ground(State)
    	->	true
    	;	throw(non_ground_state(Target,State,Event))
    ),        
    (	transition(Event,Target,State,NewState)
    ->  (	ground(NewState)
    	->	update_target_state(Target,State)
    	;	throw(transition_to_non_ground_state(State,Event,NewState,Target))
    	)
    	
	;	my_debug(builder(transition(Target)),"no transition for state ~w, event ~w (target: ~w)~n",[State,Event,Target]),
		throw(error(no_transition(Target,State,Event)))
	).



         
    

ensure_arbiter_is_running:-
    current_thread(build_arbiter,Status),
    !,
    (	Status==running
    ->	true
    ;	throw(builder_not_running(Status))
    ).
ensure_arbiter_is_running:-
	start_arbiter.    

start_arbiter:-
    current_thread(build_arbiter,running),
    !.
start_arbiter:-    
    thread_create(run_arbiter,_,[alias(build_arbiter)]).

	
	
/*
stopping the arbiter will reset the entire
arbiter state. 
In particular this will make all locks obsolete.
There is no feasible way of informing clients about this.
This predicate is only usefull in situations where we have 
complete control over all clients.
I use it for tearing down test fixtures. 
*/	
stop_arbiter:-       
    (	current_thread(build_arbiter,running)
    ->	send_message_client_target(meta,stop),
    	thread_join(build_arbiter,_)
    ;	current_thread(build_arbiter,_)
    ->	thread_join(build_arbiter,_)
    ;	true
    ).
    
	

