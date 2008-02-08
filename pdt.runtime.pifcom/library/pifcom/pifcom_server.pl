:- module(pifcom_server,[]).

:- use_module(library(socket)).
:- use_module(library(record)).
:- use_module(library(memfile)).

:- use_module(pifcom_codec).

run_server(Address):-
    tcp_socket(ServerSocket),    
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, TCPPort),
	tcp_listen(ServerSocket, 5),
	udp_socket(USocket),
	tcp_bind(USocket,UDPPort),
	new_memory_file(MF),
	open_memory_file(MF,write,MFStream,[encoding(octet)]),
	pifcom_write_integer_bytes(MFStream,2,TCPPort),
	pifcom_write_integer_bytes(MFStream,2,UDPPort),
	close(MFStream),
	memory_file_to_codes(MF,Codes),	
	udp_send(USocket, Codes, Address, []),
	free_memory_file(MF),
	accept_loop(ServerSocket,USocket).
	
accept_loop(TCPSocket,UDPSocket):-
	repeat,
		wait_for_input([TCPSocket,UDPSocket],ReadyList,5),
	    (	memberchk(UDPSocket,ReadyList)
	    ->	true
	    ;	memberchk(TCPSocket,ReadyList)
	    ->	tcp_accept(ServerSocket, Slave, _Peer),
			unused_thread_name(handle_client,Alias),
			thread_create(handle_client(Slave), _ , [alias(Alias),detached(true)]),									
			fail
		;	fail
		),
	!,
	tcp_close_socket(TCPSocket),
	tcp_close_socket(UDPSocket).


:- thread_local '$udp_io'/4,                 
                '$tcp_io'/2,                
                '$state'/1,
                '$aborting_batch'/1,
                '$aborting_control'/1.
:- dynamic '$udp_io'/4,            
           '$tcp_io'/1,           
           '$state'/1,
           '$aborting_batch'/1,
           '$aborting_control'/1.



	
handle_client(SlaveSocket):-
    tcp_open_socket(SlaveSocket, In, Out),
    udp_socket(UDPSocket),
	tcp_bind(UDPSocket,UDPPort),
	tcp_open_soclet(UDPSocket,UDPIn,UDPOut),
	assert('$tcp_io'(SlaveSocket,In,Out)),	
	assert('$udp_io'(UDPSocket,UDPPort,UDPIn,UDPOut)),		
   	set_stream(In,encoding(octet)),
    set_stream(In,tty(false)),
    set_stream(Out,encoding(octet)),
    set_stream(Out,tty(false)),    
   	call_cleanup(
   		handle_client(In,Out),
   		(	close(In),
   			close(Out),
   			halt
   		)
   	).
		
	

	
unused_thread_name(Base,Alias):-
	flag(spike_client_num,N,N+1),
	atom_concat(Base,N,Alias).
	
enter(idle,[dispatch]).
enter(spike,[dispatch]).
enter(proving,[dispatch_control]).	
transition(idle,query(T,G),[prove(T,G)],proving(T,G)).
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
    	NexxtState=skipping
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
transition(proving(T,G),success(Instance),[report_solution(T,G,Instance),redo],proving(T,G)).
transition(proving(T,G),failure,[report_failure(T)],idle).
transition(proving(T,G),error(E),[report_error(T,E)],idle).


dispatch:-
	clause('$state'(State),_,Ref),
	next_event(State,Event),
	transition(State,Event,Actions,NextState),
	!,
	erase(Ref),
	assert('$state'(NextState)),
	execute_actions(Actions).

execute_actions([]):-
    dispatch.
execute_actions([prove(T,G)|Actions]):-
	block(
		T,
		(	prove(G),
			execute_actions(Actions)
		),
		_
	).
	
next_event(idle,Event):-
	'$input'(Batch),
	'$udp_socket'(Control),
	wait_for_input([Control,Batch],Ready,5),
	(	memberchk(Control,Ready)
	->	next_control_event(Event)
	;	memberchk(Batch,Ready)
	->	next_batch_event(Event)
	;	Event=timeout
	).
next_event(skipping,Event):-
	'$input'(Batch),
	'$udp_socket'(Control),
	wait_for_input([Control,Batch],Ready,5),
	(	memberchk(Control,Ready)
	->	next_control_event(Event)
	;	memberchk(Batch,Ready)
	->	next_batch_event(Event)
	;	Event=timeout
	).
next_event(proving(T,G),Event):-
	'$input'(Batch),
	'$udp_socket'(Control),
	wait_for_input([Control,Batch],Ready,5),
	(	memberchk(Control,Ready)
	->	next_control_event(Event)
	;	memberchk(Batch,Ready)
	->	next_batch_event(Event)
	;	Event=timeout
	).