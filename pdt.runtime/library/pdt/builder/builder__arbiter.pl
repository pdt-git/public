:- module(builder__arbiter,[]).
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
	report_error(arbiter_quits).


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


         
	


