:- module(pifcom_server,[pifcom_run_server/1]).

:- use_module(library(socket)).
:- use_module(library(record)).
:- use_module(library(memfile)).

:- use_module(pifcom_codec).

:- debug(pifcom_server).

pifcom_run_server(Address):-
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
	    ->	true
	    ;	memberchk(TCPIn,ReadyList)
	    ->	tcp_accept(TCPSocket, Slave, Peer),
			unused_thread_name(handle_client,Alias),
			debug(pifcom_server,"accepted connection from ~w. Starting new thread ~w.~n",[Peer,Alias]),
			thread_create(handle_client(Slave), _ , [alias(Alias),detached(true)]),									
			fail
		;	fail
		),
	!,
	close(UDPIn),
	close(TCPIn),
	tcp_close_socket(TCPSocket),
	tcp_close_socket(UDPSocket).


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
	flag(spike_client_num,N,N+1),
	atom_concat(Base,N,Alias).

	
handle_client(SlaveSocket):-    
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
    flush_output(Out),
    debug(pifcom_server,"Bound UDP socket to port ~w~n",[UDPPort]),
   	call_cleanup(
   		handle_client2,
   		(	close(In),
   			close(Out),
   			halt
   		)
   	).
		
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
spyme.

next_event(idle,Event):-
    
	'$tcp_io'(_,TCPIn,_),
	'$udp_io'(_,_,UDPIn,_),
	wait_for_input([UDPIn,TCPIn],Ready,5),
	(	memberchk(UDPIn,Ready)
	->	next_control_event(Event)
	;	memberchk(TCPIn,Ready)
	->	next_batch_event(Event)
	;	Event=timeout
	).
next_event(skipping,Event):-
	next_event(idle,Event).

next_event(proving(_,G),Event):-    
    prove(G,Result), 
    % prove/2 has found a solution. 
    % before processing it, we check for any message
    % on the control queue.          
	(	check_control_queue(Event)
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
	
prove('$goal'(VarNames,Goal),Result):-
	(	catch(Goal,Error,Result=error(Error)),
		Result=success(VarNames)
	;	Result=failure
	).
	
	
next_batch_event(Event):-
	'$tcp_io'(_,In,_),
	spyme,
	pifcom_read_and_decode_message(In,OpCode,Ticket,Data),
	message_event(OpCode,Ticket,Data,batch,Event).

next_control_event(Event):-
	'$udp_io'(UDPSocket,_,_,_),
	udp_receive(UDPSocket,Data,_From,[]),
	new_memory_file(MF),
	open_memory_file(MF,write,Out,[encoding(octet)]),
	format(Out,"~s",[Data]),
	close(Out),
	open_memory_file(MF,read,In,[encoding(octet)]),
	pifcom_read_and_decode_message(In,OpCode,Ticket,Data),
	message_event(OpCode,Ticket,Data,control,Event).
	
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

transition(proving(T,G),abort_control(T),Action,NextState):-
    (	'$aborting_batch'(T)
    ->	Action=[aborting_batch_remove(T)], NextState=proving(T,G)
    ;	Action=[aborting_control_add(T),report_cut(T)],NextState=skipping
    ).
transition(proving(T,G),success(Instance),[report_solution(T,G,Instance)],proving(T,G)).
transition(proving(T,_),failure,[report_failure(T)],idle).
transition(proving(T,_),error(E),[report_error(T,E)],idle).


transition(State,mark(T),[report_mark(T)],State).



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
execute_action(report_cut(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,cut,T,[]).    
execute_action(report_skip(T)):-
    '$tcp_io'(_,_,Out),
    pifcom_encode_and_write_message(Out,skip,T,[]).
execute_action(report_solution(T,Bindings)):-
    '$tcp_io'(_,_,Out),   
    length(Bindings,Len),
    pifcom_encode_and_write_message(Out,begin_bindings,T,uint(Len)),
    write_bindings(Bindings,Out,T),
    flush_output(Out).
execute_action(report_names(T,Bindings)):-
    '$tcp_io'(_,_,Out),   
    length(Bindings,Len),
    pifcom_encode_and_write_message(Out,begin_names,T,uint(Len)),
    write_names(Bindings,Out,T).    
execute_action(report_failure(T)):-
    '$tcp_io'(_,_,Out),    
    pifcom_encode_and_write_message(Out,fail,T,[]),
    flush_output(Out).
execute_action(report_error(T,Error)):-
    '$tcp_io'(_,_,Out),    
    pifcom_encode_and_write_message(Out,error,T,cterm(Error)),
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