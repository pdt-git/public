module(consult_server,[
	consult_server/1,
	consulted_symbol/1	
]).
% Author: Lukas
% Date: 23.10.2004
:- use_module(library(socket)).

:- dynamic zombie_symbol/1.

consulted_symbol(Symbol) :-
	source_file(Symbol),
	\+zombie_symbol(Symbol).

delete_symbol(Symbol) :-
	assert(zombie_symbol(Symbol)).	

undelete_symbol(Symbol) :-
	retractall(zombie_symbol(Symbol)).
	
consult_server(Port):-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	concat_atom([consult_server,'@',Port],Alias),
	thread_create(accept_loop(ServerSocket), _,[alias(Alias)]).
    
accept_loop(ServerSocket) :-
	tcp_accept(ServerSocket, Slave, Peer),
	tcp_open_socket(Slave, InStream, OutStream),
	tcp_host_to_address(Host,Peer),
	thread_self(Self),
	atom_concat(Self,'_handle_client_',Prefix),	
	count_thread(Prefix,Next),
	concat_atom([Self,'_handle_client_',Next,'@',Host],Alias),	
	thread_create(handle_client(InStream, OutStream), _ , [alias(Alias)]),
	accept_loop(ServerSocket).
	
handle_client(InStream, OutStream):-    
	catch(
		handle_client_impl(InStream,OutStream),
		Error,
		( handle_exception(InStream,OutStream,Error)
		;	true
		)			
  ),
	thread_self(Me),
	thread_detach(Me),
	thread_exit(0).    

handle_client_impl(InStream, OutStream):-
	repeat,
	request_line(InStream,OutStream,'GIVE_COMMAND',Command),
	( handle_command(InStream,OutStream,Command)
	->report_ok(OutStream)
	;	report_error(OutStream, 'failed, sorry.')
	),
	fail.	
	
handle_command(_,_,'').

handle_command(_,OutStream,'PING'):-
	my_format(OutStream,"PONG~n",[]).
	
handle_command(InStream,OutStream,'CONSULT'):-
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	undelete_symbol(Symbol),
	my_format(OutStream,"USING_SYMBOL: ~a~n",[Symbol]),	
	read_pending_input(InStream, Trash, []),
	format("XXX ~a~n",Trash),
	my_format(OutStream,"GO_AHEAD~n",[]),
	load_stream(Symbol,InStream).
	
handle_command(InStream,OutStream,'UNCONSULT'):-
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	consulted_symbol(Symbol),
	new_memory_file(MemFile),
	open_memory_file(MemFile,read,NullStream),
	load_stream(Symbol,NullStream),
	close(NullStream),
	free_memory_file(MemFile),	
	delete_symbol(Symbol).
	
handle_command(InStream,OutStream,'LIST_CONSULTED'):-
	request_line(InStream,OutStream,'GIVE_PREFIX',Prefix),
	forall(
		(
			consulted_symbol(Symbol),
			atom_concat(Prefix,_,Symbol)
		),
		my_format(OutStream,"~a~n",Symbol)
	).
	
handle_command(InStream,OutStream,'IS_CONSULTED'):-
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	( consulted_symbol(Symbol) 
	->my_format(OutStream,"YES~n",[])	
	; my_format(OutStream,"NO~n",[])
	).

handle_command(InStream,OutStream,'QUERY'):-
	my_format(OutStream,"GIVE_TERM~n",[]),	
	read_term(InStream,Term,[variable_names(Vars),double_quotes(string)]),
	write('<<< '),write(Term),nl,
	catch(
		iterate_solutions(InStream,OutStream,Term,Vars),
		query_aborted,
		true
	).

handle_command(InStream,OutStream,'QUERY_ALL'):-
	my_format(OutStream,"GIVE_TERM~n",[]),
	read_term(InStream,Term,[variable_names(Vars),double_quotes(string)]),	
	write('<<< '),write(Term),nl,
	(
		all_solutions(OutStream,Term,Vars)
	;
		true
	).


	
handle_command(_,_,'SHUTDOWN'):-	
	throw(shut_down).

handle_command(_,_,'BYE'):-	
	throw(peer_quit).
	

all_solutions(OutStream,Term,Vars):-
	forall(
		Term,
		(
			print_solution(OutStream,Vars)						
		)		
	).

	
iterate_solutions(InStream,OutStream,Term,Vars):-
	( forall(
			Term,
			(
				print_solution(OutStream,Vars),
				request_line(InStream,OutStream,'MORE?','YES')											
			)
		)
	->my_format(OutStream,"NO~n",[])
	; my_format(OutStream,"YES~n",[])
	).
	

	
	
print_solution(OutStream,Vars):-
	forall(
		member(Elm,Vars),
		(
			write_term(OutStream,Elm,[quoted(true)]),
			nl(OutStream),
			write('>>> '),
			write_term(Elm,[quoted(true)]),
			nl
		)
	),
	my_format(OutStream,"END_OF_SOLUTION~n",[]).

	
	
handle_exception(InStream,OutStream,peer_quit):-
	catch(	
		byebye(InStream,OutStream),
		_,
		shut_down(InStream,OutStream)
	).
	
handle_exception(InStream,OutStream,shut_down):-
	catch(	
		byebye(InStream,OutStream),
		_,
		shut_down(InStream,OutStream)
	),
	thread_signal(main,halt).

handle_exception(InStream,OutStream,peer_reset):-
	catch(
		(
			my_format(OutStream,"RESET~n",[]),
			report_ok(OutStream)
		),							
		_,(
			shut_down(InStream,OutStream),
			fail
			)
	),
	handle_client(InStream,OutStream).
	
handle_exception(InStream,OutStream,Error):-
	catch(		
		report_error(OutStream,Error),					
		_,(
			shut_down(InStream,OutStream),
			fail
			)
	),
	handle_client(InStream,OutStream).

	

	
report_ok(OutStream):-
	my_format(OutStream,"OK~n",[]).	
	
report_error(OutStream, Error):-
	my_format(OutStream,"ERROR: ~w~n",[Error]).			
	
		
byebye(InStream,OutStream):-
	my_format(OutStream,"BYE~n",[]),
	close(InStream),
	close(OutStream).
	
shut_down(InStream,OutStream):-
	catch(
		close(InStream, [force(true)]),
		_,
		true),
	catch(
		close(OutStream, [force(true)]),
		_,
		true).
		

request_line(InStream, OutStream, Prompt, Line):-
	my_format(OutStream,"~a~n",[Prompt]),
	read_line_to_codes(InStream,LineCodes),
	codes_or_eof_to_atom(LineCodes,Line),
	check_eof(Line),
	format("<<< ~a~n",Line).

check_eof(end_of_file):-
	!,
	throw(peer_reset).
	
check_eof('end_of_file'):-
	!,
	throw(peer_reset).

check_eof('end_of_file.'):-
	!,
	throw(peer_reset).
	
check_eof(A):-
	atom_concat(_,end_of_file,A),
	!,
	throw(peer_reset).

check_eof(A):-
	atom_concat(_,'end_of_file',A),
	!,
	throw(peer_reset).

check_eof(A):-
	atom_concat(_,'end_of_file.',A),
	!,
	throw(peer_reset).	
check_eof(_):-
	true.
request_data(InStream, OutStream, Symbol):-	
	my_format(OutStream,"USING_SYMBOL: ~a~n",[Symbol]),	
	read_pending_input(InStream, Trash, []),
	format("XXXX ~a~n",Trash),
	my_format(OutStream,"GO_AHEAD~n",[]).
	
	
codes_or_eof_to_atom(end_of_file,_):-
	throw(peer_reset).
	
codes_or_eof_to_atom(Codes,Atom):-
	atom_codes(Atom,Codes).
	
load_stream(Symbol,Stream):-
	load_files(Symbol,[stream(Stream)]),!,true
	; 
	throw(error(pipi,kaka)).
	
count_thread(Prefix,Count):-
	findall(X,(
		current_thread(A,_),
			atom_concat(Prefix,X,A)
		),
		Bag
	),
	length(Bag,Count).
	
	
my_format(OutStream,Format,Args):-
	format(OutStream,Format,Args),
	write('>>> '),
	format(current_output,Format,Args),
	flush_output(OutStream),
	flush_output(current_output).