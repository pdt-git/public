:- module(pifcom_server,[pifcom_start_server/1]).

:- use_module(library(socket)).
:- use_module(library(record)).
:- use_module(library(memfile)).

:- use_module(pifcom_codec).

%:- debug(pifcom_server).
%:- guitracer.


pifcom_start_server(Address):-
    thread_create(pifcom_run_server(Address),_,[alias(pifcom_accept_loop)]).

pifcom_run_server(Address):-
    thread_at_exit(thread_signal(main,halt)),
    tcp_socket(ServerSocket),    
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, TCPPort),
	tcp_listen(ServerSocket, 5),
	udp_socket(USocket),
	tcp_bind(USocket,UDPPort),
	new_memory_file(MF),
	open_memory_file(MF,write,MFStream,[encoding(octet)]),
	debug(pifcom_server,"master ports: TCP=~w, UDP=~w~n",[TCPPort,UDPPort]),
	pifcom_write_integer_bytes(MFStream,2,TCPPort),
	pifcom_write_integer_bytes(MFStream,2,UDPPort),
	close(MFStream),
	memory_file_to_codes(MF,Codes),	
	udp_send(USocket, Codes, Address, []),
	free_memory_file(MF),
	accept_loop(ServerSocket,USocket).
	
accept_loop(TCPSocket,UDPSocket):-
    tcp_open_socket(TCPSocket,TCPIn,_),
    tcp_open_socket(UDPSocket,UDPIn,_),
	repeat,
		wait_for_input([TCPIn,UDPIn],ReadyList,5),
	    (	memberchk(UDPIn,ReadyList)
	    ->	udp_receive(UDPSocket,Data,From,[]),
	    	debug(pifcom_server,"Received Datagram \"~s\" from  ~w.~n",[Data,From])
	    ;	memberchk(TCPIn,ReadyList)
	    ->	tcp_accept(TCPSocket, Slave, Peer),
			unused_thread_name(handle_client,Alias),
			debug(pifcom_server,"accepted connection from ~w. Starting new thread ~w.~n",[Peer,Alias]),			
			catch(
				(	
					thread_create(handle_client(Slave), ThreadId , [alias(Alias),detached(true)]),						
					assert('$handler_thread'(Alias))
				),	
				E,
				(	tcp_close_socket(Slave),
					debug(pifcom_server,"Could not create handler thread: ~w.~n",[E])
				)
			),

			
			(	nonvar(ThreadId),
				thread_property(ThreadId,status(Status)),
				Status==running
			->	true
			;	tcp_close_socket(Slave),
				debug(pifcom_server,"Handler at ~w could not be created.~n",[Alias])
			),
			fail
		;	fail
		),
	!,
	debug(pifcom_server,"Exiting accept loop~n",[]),
	debug(pifcom_server,"Asking handler threads to exit.~n",[]),
	forall(
		(	current_thread(Alias,running),
			'$handler_thread'(Alias)
		),
		thread_send_message(Alias,pifcom_internal_event(exit))
		
	),
	retractall('$handler_thread'(_)),
	debug(pifcom_server,"Closing master sockets.~n",[]),
	close(UDPIn),	
	close(TCPIn),	
	debug(pifcom_server,"done.~n",[]).


:- dynamic '$handler_thread'/1.


:- thread_local '$udp_io'/4,                 
                '$tcp_io'/3,                
                '$state'/1,
                '$aborting_batch'/1,
                '$aborting_control'/1.
:- dynamic '$udp_io'/4,            
           '$tcp_io'/3,           
           '$state'/1,
           '$aborting_batch'/1,
           '$aborting_control'/1.


unused_thread_name(Base,Alias):-
	flag(pifcom_client_num,N,N+1),
	atom_concat(Base,N,Alias).

	
handle_client(SlaveSocket):-    
    debug(pifcom_server,"Starting client handler.~n",[]),
    tcp_open_socket(SlaveSocket, In, Out),
    udp_socket(UDPSocket),
	tcp_bind(UDPSocket,UDPPort),
	tcp_open_socket(UDPSocket,UDPIn,UDPOut),
	
	assert('$tcp_io'(SlaveSocket,In,Out)),	
	assert('$udp_io'(UDPSocket,UDPPort,UDPIn,UDPOut)),
	assert('$state'(idle)),		   	
   	set_stream(In,encoding(octet)),
    set_stream(In,tty(false)),
    set_stream(Out,encoding(octet)),
    set_stream(Out,tty(false)),        
    pifcom_write_integer_bytes(Out,2,UDPPort),
    thread_self(Alias),
    pifcom_encode_and_write_message(Out,name,0,name(Alias)),
    flush_output(Out),
    debug(pifcom_server,"Bound UDP socket to port ~w~n",[UDPPort]),
   	call_cleanup(
   		handle_client2,
   		(	debug(pifcom_server,"Closing sockets ~n",[]),
   			close(In),
   			close(Out),
   			close(UDPIn),
   			close(UDPOut)   			
   		)
   	),
   	debug(pifcom_server,"done. ~n",[]).
		
handle_client2:-    		
	dispatch.
	

dispatch:-
	repeat,
		dispatch_block,
	!.

dispatch_block:-	
	clause('$state'(State),_,Ref),
	debug(pifcom_server,"in state ~w.~n",[State]),
	next_event(State,Event),
	debug(pifcom_server,"received event ~w.~n",[Event]),
	(	transition(State,Event,Actions,NextState)
	->	true
	;	throw(no_transition(State,Event))
	),					
	execute_actions(Actions),
	(	NextState==State
	->	fail
	;	!, %before changing to the new state, cut all choices 
		   %created by the current one.
		debug(pifcom_server,"transition to ~w.~n",[NextState]),
		erase(Ref),
		assert('$state'(NextState)),
		NextState==stop
	).


next_event(idle,Event):-
	(	check_internal_queue(Event)    
	;	'$tcp_io'(_,TCPIn,_),
		'$udp_io'(_,_,UDPIn,_),
		wait_for_input([UDPIn,TCPIn],Ready,5),
		(	memberchk(UDPIn,Ready)
		->	next_control_event(Event)
		;	memberchk(TCPIn,Ready)
		->	next_batch_event(Event)
		;	Event=timeout
		)
	).
next_event(skipping,Event):-
	next_event(idle,Event).

next_event(proving(_,G),Event):-    
    prove(G,Result), 
    % prove/2 has found a solution. 
    % before processing it, we check for any message
    % on the control queue.          
	(	check_internal_queue(Event)
	;	check_control_queue(Event)
	;	Event=Result
	).

/*
Successively produces all pending events on the control queue. 
*/
check_control_queue(Event):-
    '$udp_io'(_,_,UDPIn,_),
    repeat,
    	(	wait_for_input([UDPIn],[UDPIn],0.000000000000000000000000000000000000000000000000000000000000000000000000000000001)	
		->	next_control_event(Event)
		;	!,
			fail
		).

/*
Successively produces all pending events on the internal queue. 
*/
check_internal_queue(Event):-
    repeat,
    	(	thread_peek_message(pifcom_internal_event(_))	
		->	thread_get_message(pifcom_internal_event(Event))
		;	!,
			fail
		).

	
prove('$goal'(VarNames,Goal),Result):-
	(	catch(
			(	Goal,
				Result=success(VarNames)
			),
			Error,
			Result=error(Error)
		)		
	;	Result=failure
	).
	
	
next_batch_event(Event):-
	'$tcp_io'(_,In,_),	
	(	at_end_of_stream(In)
	->	Event=end_of_stream
	;	catch(
			(	pifcom_read_and_decode_message(In,OpCode,Ticket,EventData),
				message_event(OpCode,Ticket,EventData,batch,Event)
			),
			% this is the kind of decode error that *may* be non-fatal.
			% example: syntax error in query. 
			% we turn non-fatal errors into events so the state machiene can 
			% decide how to deal with them.
			% All other uncought errors will cause the handler to terminate the session.
			% see dispatch/0 and friends. 
			error(pifcom_error(E,decode_body(OpCode,Ticket))),		
			Event=pifcom_error(E,decode_body(OpCode,Ticket))
		)
	).

next_control_event(Event):-	   
	'$udp_io'(UDPSocket,_,_,_),
	udp_receive(UDPSocket,Data,_From,[]),
	new_memory_file(MF),
	open_memory_file(MF,write,Out,[encoding(octet)]),
	format(Out,"~s",[Data]),
	close(Out),
	open_memory_file(MF,read,In,[encoding(octet)]),	
	
	catch(
		(	pifcom_read_and_decode_message(In,OpCode,Ticket,EventData),
			message_event(OpCode,Ticket,EventData,control,Event)
		),
		% this is the kind of decode error that *may* be non-fatal.
		% example: syntax error in query. 
		% we turn non-fatal errors into events so the state machiene can 
		% decide how to deal with them.
		% All other uncought errors will cause the handler to terminate the session.
		% see dispatch/0 and friends. 
		error(pifcom_error(E,decode_body(OpCode,Ticket))),		
		Event=pifcom_error(E,decode_body(OpCode,Ticket))
	).
	
	
message_event(query,Ticket,cterm(VarNames,Goal),_,query(Ticket,'$goal'(VarNames,Goal))).
message_event(abort,Ticket,[],Queue,Event):-
    (	Queue==control
    ->	Event=abort_control(Ticket)
    ;	Queue==batch,
    	Event=abort_batch(Ticket)
    ).
message_event(mark,Ticket,[],_,mark(Ticket)).
message_event(bye,Ticket,[],_,bye(Ticket)).    

    
transition(idle,query(T,'$goal'(VarNames,Goal)),[report_names(T,VarNames)],proving(T,'$goal'(VarNames,Goal))).
transition(idle,timeout,[],idle).
transition(idle,abort_batch(T),[aborting_batch_add(T),report_abort_complete(T)],idle).
transition(idle,abort_control(T),Action,NextState):-
    (	'$aborting_batch'(T)
    ->	Action=[aborting_batch_remove(T)], NextState=idle
    ;	Action=[aborting_control_add(T)],NextState=skipping
    ).   
transition(skipping,query(T,_),[report_skip(T)],skipping).
transition(skipping,timeout,[],skipping).
transition(skipping,abort_batch(T),Action,NextState):-
    (	'$aborting_control'(T)
    ->	Action=[aborting_control_remove(T),report_abort_complete(T)],
    	(	predicate_property('$aborting_control'(_),number_of_clauses(1))
		->	NextState=idle
    	;	NextState=skipping
    	)
    ;	Action=[aborting_batch_add(T),report_abort_complete(T)],
    	NextState=skipping
    ).
transition(skipping,abort_control(T),Action,NextState):-
    (	'$aborting_batch'(T)
    ->	Action=[aborting_batch_remove(T)], NextState=skipping
    ;	Action=[aborting_control_add(T)],NextState=skipping
    ).

transition(proving(T,G),abort_control(AT),Action,NextState):-
    (	'$aborting_batch'(AT)
    ->	Action=[aborting_batch_remove(AT)], NextState=proving(T,G)
    ;	Action=[aborting_control_add(AT),report_cut(T)],NextState=skipping
    ).
transition(proving(T,G),success(Bindings),[report_solution(T,Bindings)],proving(T,G)).
transition(proving(T,_),failure,[report_failure(T)],idle).
transition(proving(T,_),error(E),[report_error(T,E)],idle).


transition(State,mark(T),[report_mark(T)],State).
transition(_,bye(T),[report_bye(T)],stop).
transition(_,end_of_stream,[],stop).
transition(_,exit,[],stop).
transition(_,pifcom_error(E,decode_body(_,Ticket)),[report_protocol_error(Ticket,E)],idle).


execute_actions([]).
execute_actions([Action|Actions]):-
    debug(pifcom_server,"executing action ~w.~n",[Action]),
    (	execute_action(Action)
    ->	true
    ;	throw(failed(execute_action(Action)))
    ),
    execute_actions(Actions).

execute_action(aborting_batch_add(T)):-
    assert('$aborting_batch'(T)).
execute_action(aborting_batch_remove(T)):-
    retract('$aborting_batch'(T)).    
execute_action(aborting_control_add(T)):-
    assert('$aborting_control'(T)).
execute_action(aborting_control_remove(T)):-
    retract('$aborting_control'(T)).    
execute_action(report_abort_complete(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,complete,T,[]),
    flush_output(Out).
execute_action(report_mark(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,mark,T,[]),
    flush_output(Out).    
execute_action(report_bye(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,bye,T,[]),
    flush_output(Out).    
execute_action(report_cut(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,cut,T,[]).    
execute_action(report_skip(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,skip,T,[]).
execute_action(report_solution(T,Bindings)):-
    '$tcp_io'(_,_,Out),   
    %length(Bindings,Len),
    %pifcom_encode_and_write_message(Out,begin_solution,T,uint(Len)),
    (	Bindings==[]
    ->	pifcom_encode_and_write_message(Out,empty,T,[])
    ;	write_bindings(Bindings,Out,T)
    ),
    flush_output(Out).
execute_action(report_names(T,Bindings)):-
    '$tcp_io'(_,_,Out),   
    length(Bindings,Len),
    pifcom_encode_and_write_message(Out,begin_solution,T,uint(Len)),
    write_names(Bindings,Out,T).    
execute_action(report_failure(T)):-
    '$tcp_io'(_,_,Out),    
    pifcom_encode_and_write_message(Out,fail,T,[]),
    flush_output(Out).
execute_action(report_error(T,Error)):-
    '$tcp_io'(_,_,Out),    
    pifcom_encode_and_write_message(Out,error,T,cterm([],Error)),
    flush_output(Out).
execute_action(report_protocol_error(T,Error)):-
    '$tcp_io'(_,_,Out),    
    pifcom_encode_and_write_message(Out,protocol_error,T,cterm([],Error)),
    flush_output(Out).

	



write_bindings(Bindings,Out,T):-
    bindings_unbound(Bindings,Unbound),    
    write_bindings(Bindings,Unbound,Out,T).

bindings_unbound([], []).
bindings_unbound([Name=Value|Bindings], Out):-
    (	var(Value)
    ->	Out=[Name=Value|Unbound]
    ;	Out=Unbound
    ),
    bindings_unbound(Bindings,Unbound).
    
write_bindings([],_,_,_).
write_bindings([_Name=Value|Bindings],Unbound,Out,T):-    
    pifcom_encode_and_write_message(Out,binding,T,cterm(Unbound,Value)),
    write_bindings(Bindings,Unbound,Out,T).

write_names([],_,_).
write_names([Name=_Value|Bindings],Out,T):-    
    pifcom_encode_and_write_message(Out,name,T,name(Name)),
    write_names(Bindings,Out,T).    
