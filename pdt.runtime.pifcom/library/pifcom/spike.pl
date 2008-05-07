:- use_module(library(socket)).
:- use_module(library(record)).

:- use_module(library(memfile)).
:- debug(spike).

:- record(cx(state,in,out,ctrl,goal,ticket)).

server(Port):-
	tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, In, _),
    assert(my_server(Socket,In)),
    thread_create(accept_loop(Socket,In),_,[alias(spike_accept_loop),detached(true)]).

stop_accept_loop:-
    my_server(_,In),
    close(In),
    retractall(my_server(_,In)).

accept_loop(ServerSocket,In):-
    repeat,
	    (	is_stream(In)
    	->	wait_for_input([In],[In],5),    	
			tcp_accept(ServerSocket, Slave, _Peer),
			tcp_open_socket(Slave, InStream, OutStream),
			unused_thread_name(spike_client,Alias),
			thread_create(handle_client(InStream, OutStream), _ , [alias(Alias),detached(true)]),
			fail
		;	true
		),
	!.
	

	
unused_thread_name(Base,Alias):-
	flag(spike_client_num,N,N+1),
	atom_concat(Base,N,Alias).
	
handle_client(InStream, OutStream):-
    %set_stream(InStream,buffer(false)),
    repeat,
	    (	read_opc(InStream,Command)
	    ->	debug(spike,"got command head: ~w~n",[Command]),	    
    		read_args(Command,InStream),
	    	debug(spike,"got args: ~w~n",[Command]),	        	
	    	process_command(Command,OutStream,Next),
	    	flush_output(OutStream)
	    ;	Next=stop
	    ),
    Next==stop,!,
    close(InStream),
    close(OutStream).


    
read_opc(InStream,Command):-
	    get_byte(InStream,OPC),
	    debug(spike,"got opc: ~w~n",[OPC]),
	    opc_head(OPC,Command).
	    
read_args(Command,InStream,Vars):-
    functor(Command,_Name,Arity),
    empty_assoc(Vars0),
    read_args(1,Arity,InStream,Command,Vars0,Vars).

read_args(I,N,_InStream,_Command,Vars,Vars):-
    I>N,
    !.
read_args(I,N,InStream,Command,Vars0,Vars):-
    read_arg(I,InStream,Command,VarNames),
    merge_vars(VarNames,Vars0,Vars1),
    J is I + 1,
    read_args(J,N,InStream,Command,Vars1,Vars).
    
read_arg(I,InStream,Command,VarNames):-
    read_bytes_to_integer(4,InStream,0,Num),
    debug(spike,"got length: ~w~n",[Num]),	        	    
    new_memory_file(MemFile),
    call_cleanup(read_arg_X(MemFile,InStream,Num,Arg,VarNames),free_memory_file(MemFile)),
    debug(spike,"got arg: ~w~n",[Arg]),	        	    
    arg(I,Command,Arg).

read_arg_X(MemFile,InStream,Num,Arg,VarNames):-    
    open_memory_file(MemFile,write,MemOut),
    call_cleanup(
    	(	copy_stream_data(InStream,MemOut,Num),
    		put_char(MemOut,'.')
    	),
    	close(MemOut)
    ),
    open_memory_file(MemFile,read,MemIn),
    call_cleanup(
    	read_term(MemIn,Arg,[variable_names(VarNames)]),
    	close(MemIn)
    ).

read_bytes_to_integer(0,_Stream,Sum,Sum):-
    !.
read_bytes_to_integer(I,Stream,Old,Sum):-
	get_byte(Stream,Byte),
    J is I - 1,
	New is Old + (Byte << (J * 8)),
	read_bytes_to_integer(J,Stream,New,Sum).


merge_vars([Name=Var|VarNames],Vars0,Vars):-
    (	get_assoc(Name,Vars0,Var)
    ->	Vars1=Vars0
    ;	put_assoc(Name,Vars0,Var,Vars1)
    ),
    merge_vars(VarNames,Vars1,Vars).
    

opc_head(0,test(_,_)).
opc_head(1,query_once(_Goal)).	
opc_head(1,query_all(_Goal)).	
process_command(Command,OutStream,continue):-
    write(OutStream,Command),
    nl(OutStream).
    
    

next_event(both,TimeOut,Cx,Event):-
	cx_up(Cx,Up),
	cx_ctl(Cx,Ctrl),
	wait_for_input([Ctrl,Up],Avail,TimeOut),
	(	memberchk(Ctrl,Avail)
	->	read_ctrl(Ctrl,Event)
	;	memberchk(Up,Avail)
	->	read_cmd(Up,Event)
	;	Event=timeout
	).
next_event(cmd,TimeOut,Cx,Event):-
	cx_up(Cx,Up),
	(	wait_for_input([Up],[Up],TimeOut)
	->	read_cmd(Up,Event)
	;	Event=timeout
	).
next_event(ctrl,TimeOut,Cx,Event):-
	cx_ctrl(Cx,Ctrl),
	(	wait_for_input([Ctrl],[Ctrl],TimeOut)
	->	read_ctrl(Ctrl,Event)
	;	Event=timeout
	).
next_event(prove,_,Cx,goal(Event)):-
    cx_goal(Cx,Goal),
    catch(
    	(	user:call(Goal)
    	;	Event=failure
    	),
    	Error,
    	true
    ),
    (	nonvar(Event)
    ->	true
    ;	nonvar(Error)
    ->	Event=error(Error)
    ;	Event=success
    ).

read_ctrl(Stream,ctrl(T,Event)):-
	read(Stream,ctrl(T,Event)).
	
read_cmd(Stream,cmd(T,Event,Vars)):-
	read_opc(Stream,Event),
	read_args(Event,Stream).


dispatch:-
    default_cx(Cx0),
    nb_setval('$handle_client_cx',Cx0),
    repeat,    
    	nb_getval('$handle_client_cx',Cx),
	    cx_state(Cx,State),
	    state_filter(State,Filter),
	    state_timeout(State,TimeOut),
	    block(event,
	    	(	next_event(Filter,TimeOut,Cx,Event),
			    transition(State,Event,Action,NextState),
			    perform_actions(Action,Cx,Cx1)
			),
			_
		),    
	    set_state_of_cx(NextState,Cx1,NextCx),
	    nb_setval('$handle_client_cx',NextCx),
	    client_should_stop(NextCx),
	!.
 

transition(state(skipping,Ts),		ctrl(T,abort),		[],									state(skipping,[T|Ts])).
transition(state(skipping,[T|Ts]),	cmd(T,abort,[]),	[report_abort_complete([T|Ts])],	state(idle,[])).
transition(state(skipping,Ts),		cmd(T,_C,_VNs),		[report_skipping(T)],				state(skipping,Ts)).    
transition(state(proving2,[T]),		ctrl(T2,abort),		[cut,report_cut(T)],				state(skipping,[T2])).    
transition(state(proving2,[_T]),	timeout,			[fail],								[]).
transition(state(proving,[T]),		goal(success),		[report_solution(T)],				state(proving2,[T])).
transition(state(proving,[T]),		goal(fail),			[report_failure(T)],				state(idle,[])).
transition(state(proving,[T]),		goal(error(E)),		[report_error(T,E)],				state(idle,[])).
transition(state(waiting,[T]),		timeout,			[report_abort_timeout(T)],			state(idle,[])).
transition(state(waiting,[T]),		cmd(T,abort,[]),	[report_abort_complete([T])],		state(idle,[])).
transition(state(waiting,[T]),		cmd(T2,_C,_VNs),	[report_abort_unexpected_cmd(T,T2)],state(idle,[])).
transition(state(idle,[]),			ctrl(T,abort,[]),	[],									state(skipping,[T])).
transition(state(idle,[]),			cmd(T,mark,[]),		[report_mark(T)],					state(idle,[])).
transition(state(idle,[]),			cmd(T,query(G),VNs),[prepare(T,G,VNs)],					state(proving,[T])).
transition(state(idle,[]),			cmd(T,abort,[]),	[],									state(waiting,[T])).

perform_actions([],Cx,Cx).
perform_actions([A|As],Cx,Cx3):-
    perform_action(A,Cx,Cx2),
    perform_actions(As,Cx2,Cx3).

perform_action(cut,Cx,Cx):-
	!(event).
perform_action(fail,Cx,Cx):-
	!,
	fail.
perform_action(report_skipping(T),Cx,Cx):-
    !,
    cx_out(Cx,Out),    
    reply_code(skipping,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out).
perform_action(report_cut(T),Cx,Cx):-
    !,
    cx_out(Cx,Out),    
    reply_code(cut,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out).    
perform_action(report_solution(T),Cx,Cx):-
	!,
    cx_out(Cx,Out),
	reply_code(var_names,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out),
    cx_var_names(Cx,VNames),
    cx_var_names(Cx,VNums),    
    var_bindings(VNames,VNums,Bindings),
    length(Bindings,Len),    
    put_byte(Out,Len),        
    encode_bingings(Bindings,Out).
perform_action(report_failure(T),Cx,Cx):-
    !,
    cx_out(Cx,Out),    
    reply_code(failure,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out).    
perform_action(report_error(T,E),Cx,Cx):-
    !,
    cx_out(Cx,Out),    
    reply_code(error,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out),
	encode_term(E,Out).
perform_action(report_abort_timeout(T),Cx,Cx):-
    !,
    cx_out(Cx,Out),    
    reply_code(abort_timeout,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out).    
perform_action(report_abort_complete([Ts]),Cx,Cx):-
	!,
    cx_out(Cx,Out),	
	reply_code(abort_complete,Byte),
    put_byte(Out,Byte),
	length(Ts,Len),
    put_byte(Out,Len),            
    encode_tickets(Ts,Out).        
perform_action(report_abort_unexpected_cmd(T,T2),Cx,Cx):-
    !,
    cx_out(Cx,Out),    
    reply_code(abort_unexpected_cmd,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out),
    encode_ticket(T2,Out).
perform_action(report_mark(T),Cx,Cx):-
	!,
    cx_out(Cx,Out),	
	reply_code(mark,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out).        
perform_action(prepare(T,G,VNames),Cx,Cx2):-
	!,
    cx_out(Cx,Out),	
	reply_code(var_names,Byte),
    put_byte(Out,Byte),
    encode_ticket(T,Out),
    var_names_numbers(VNames,VNums),
    length(VNums,Len),
    put_byte(Out,Len),        
    encode_var_nums(VNums,Out),
    set_cx_fields([goal(G),var_names(VNames),var_nums(VNums)],Cx,Cx2).
perform_action(A,Cx,Cx):-
    throw(dunno_howto_perform_action(A)).
    
var_names_numbers(VNames,VNums):-
    copy_term(VNames,VNums),
    numbervars(VNums,0,_).

var_bindings([],[],[]).
var_bindings([Name=Var|VNames],[Name='$var'(Num)|VNums],Bindings):-
    (	nonvar(Var)
    ->	Bindings= [(Num=Var)|Bindings2]
    ;	Bindings=Bindings2
    ),
	var_bindings(VNames,VNums,Bindings2).
    

encode_bindings([],_Out).
encode_bindings([(Num=Val)|Bs],Out):-
    put_byte(Out,Num),
    encode_term(Val,Out),
    encode_bindings(Bs,Out).

	    
encode_var_nums([],_Out).
encode_var_nums([Name='$var'(Num)|VNs],Out):-
    put_byte(Out,Num),
    encode_name(Name,Out),
    encode_var_nums(VNs,Out).

encode_tickets([],_Out).
encode_tickets([T|Ts],Out):-
    encode_ticket(T,Out),
    encode_tickets(Ts,Out).
    

encode_ticket(T,Out):-
    put_integer_bytes(T,4,Out).
    
put_integer_bytes(_T,0,_Out):-
    !.
put_integer_bytes(T,D,Out):-
    E is D - 1,
    Byte is T >> (E * 8),
    put_byte(Out,Byte),
    Remain is T - (Byte <<(E * 8)),
    put_integer_bytes(Remain,E,Out).
encode_name(Name,Out):-
    atom_to_memory_file(Name,MF),
    call_cleanup(
	    (	size_memory_file(MF,Len),
	    	put_byte(Out,Len),
	    	open_memory_file(MF,read,In),
	    	copy_stream_data(In,Out,Len)
	    ),
	    free_memory_file(MF)
	).
encode_term(Val,Out):-
	type_code(term_image,Byte),
	put_byte(Out,Byte),
    new_memory_file(MF),
    call_cleanup(
	    (	open_memory_file(MF,write,MOut),
			write(MOut,Val),
			close(MOut),
			size_memory_file(MF,Len),
	    	put_integer_bytes(Len,4,Out),
	    	open_memory_file(MF,read,In),
	    	copy_stream_data(In,Out,Len)
	    ),
	    free_memory_file(MF)
	).

test_pipe(A):-
	pipe(In,Out),
	thread_create(test_pipe_x(Out),_,[detached(true)]),
	repeat,
		writeln(waiting),
		wait_for_input([In],[In],infinite),
	!,
	read(In,A),
	close(In).
	
test_pipe_x(Out):-
	sleep(5),
	format(Out,"~w.~n",[foo(bar)]),
	flush_output(Out),
	writeln(written),
	close(Out).
	
	
test_integer(A,Len,B):-
	new_memory_file(MF),
	open_memory_file(MF,write,Out),
	put_integer_bytes(A,Len,Out),
	close(Out),
	open_memory_file(MF,read,In),
	read_bytes_to_integer(Len,In,0,B),
	close(In),
	free_memory_file(MF).	
	
	/*
demo_nb_linkval :-
            T = nice(N),
            (   N = world,
                nb_linkval(myvar, T),
                fail
            ;   nb_getval(myvar, V),
                writeln(V)
            ).	
            */