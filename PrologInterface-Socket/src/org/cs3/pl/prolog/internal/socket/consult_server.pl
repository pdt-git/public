:- module(consult_server,[
	consult_server/1,
	consulted_symbol/1,
	starts_at/2
	]). 
% Author: Lukas
% Date: 23.10.2004
:- use_module(library(socket)).

:- dynamic zombie_symbol/1.
:- dynamic offset/2.

starts_at(Symbol,Line):-	
		(	offset(Symbol,Line)
		->	true
		;	Line is 0
		).
	
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
	%accept_loop(ServerSocket).
	thread_create(accept_loop(ServerSocket), _,[alias(Alias)]).
	
accept_loop(ServerSocket):-
	catch(
		accept_loop_impl(ServerSocket),
		Error,
		(
			write('AAAaaaAAAaaaAAAaaaAAA:'),write(Error),nl,thread_signal(main,halt)
		)
	),
	thread_self(Me),
	thread_detach(Me),
	thread_exit(0).
	
accept_loop_impl(ServerSocket) :-
	write(0),nl,
	tcp_accept(ServerSocket, Slave, Peer),
	write(1),nl,
	tcp_open_socket(Slave, InStream, OutStream),
	write(2),nl,
	tcp_host_to_address(Host,Peer),
	write(3),nl,
	thread_self(Self),
	write(4),nl,
	atom_concat(Self,'_handle_client_',Prefix),
	atom_concat('@',Host,Suffix),
	write(5),nl,	
	unused_thread_name(Prefix,Suffix,Alias),
	write(7),nl,	
	%handle_client(InStream, OutStream),
	thread_create(handle_client(InStream, OutStream), _ , [alias(Alias)]),
	write(8),nl,
	accept_loop_impl(ServerSocket).
	
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
	my_format(OutStream,"GO_AHEAD~n",[]),
	load_stream(Symbol,InStream).
	
handle_command(InStream,OutStream,'UNCONSULT'):-
	(
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	consulted_symbol(Symbol),
	new_memory_file(MemFile),
	open_memory_file(MemFile,read,NullStream),
	load_stream(Symbol,NullStream),
	close(NullStream),
	free_memory_file(MemFile),	
	delete_symbol(Symbol)
	)
	;true.
	
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
	my_read_term(InStream,Term,[variable_names(Vars),double_quotes(string)]),
	catch(
		iterate_solutions(InStream,OutStream,Term,Vars),
		query_aborted,
		true
	).
	
handle_command(InStream,OutStream,'QUERY_ALL'):-
	my_format(OutStream,"GIVE_TERM~n",[]),
	my_read_term(InStream,Term,[variable_names(Vars),double_quotes(string)]),		
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
	user:forall(
		Term,
		(
			consult_server:print_solution(OutStream,Vars)						
		)		
	).
	
	
iterate_solutions(InStream,OutStream,Term,Vars):-
	( user:forall(
			Term,
			(
				consult_server:print_solution(OutStream,Vars),
				consult_server:request_line(InStream,OutStream,'MORE?','YES')											
			)
		)
	->my_format(OutStream,"NO~n",[])
	; my_format(OutStream,"YES~n",[])
	).
	
	
	
	
print_solution(OutStream,Vars):-
	forall(
		member(Key=Val,Vars),
	%%			my_write_term(OutStream,Elm,[quoted(true)])		
		print_binding(OutStream,Key,Val)
	),
	my_format(OutStream,"END_OF_SOLUTION~n",[]).
	
print_binding(Out,Key,Val):-
		write(Out,'<'),
		(write_escaped(Out,Key);true),
		write(Out, '>'),		
		(print_value(Out,Val);true),		
		nl(Out).

print_values(_,[]):-
	!. 
print_values(Out,[Head|Tail]):-
	!,
	print_value(Out,Head),		
	print_values(Out,Tail).
	
print_value(Out,Val):-    	
	( 	is_list(Val)
 	->	write(Out,'{'),
		(print_values(Out,Val);true),
		write(Out, '}')		
	;	write(Out,'<'),
		(write_escaped(Out,Val);true),
		write(Out, '>')
	).
	
	
	
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
	(	var(Error)
	->	my_format(OutStream,"ERROR: unbound error term~n",[])
	;	my_format(OutStream,"ERROR: ~w~n",[Error])
	).			
	
		
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
	
	
	
codes_or_eof_to_atom(end_of_file,_):-
	throw(peer_reset).
	
codes_or_eof_to_atom(Codes,Atom):-
	atom_codes(Atom,Codes).
	
load_stream(Symbol,Stream):-    
		(	
			retractall(offset(Symbol,_)),
			line_count(Stream,Line),
			assert(offset(Symbol,Line)),
			user:load_files(Symbol,[stream(Stream)])
			%new_memory_file(Handle),
			%open_memory_file(Handle, write, WriteStream),
			%copy_stream_data(Stream,WriteStream),
			%close(WriteStream),
			%open_memory_file(Handle, read, ReadStream),
			%user:load_files(Symbol,[stream(ReadStream)]),
			%close(ReadStream),
			%free_memory_file(Handle)
	->	true
	; 	throw(error(pipi,kaka))
	).
	
count_thread(Prefix,Count):-
	findall(A,(
			current_thread(A,_),
			atom_concat(Prefix,_,A),
			format("There is e.g. a thread named ~a~n",[A])
		),
		Bag
	),	 
	length(Bag,Count).
	
unused_thread_name(Prefix,Suffix,Name):-
	unused_thread_name(Prefix,Suffix,0,Name).	
	
unused_thread_name(Prefix,Suffix,Try,Name):-
	concat_atom([Prefix,Try,Suffix],A),
	format("trying ~a~n",[A]),
	(	current_thread(A,_)
	->plus(Try,1,Next),
		unused_thread_name(Prefix,Suffix,Next,Name)
	; format("unused: ~a~n",[A]),
		Name=A			
	).
	
	
	
	
request_line(InStream, OutStream, Prompt, Line):-
	my_format(OutStream,"~a~n",[Prompt]),
	read_line_to_codes(InStream,LineCodes),
	codes_or_eof_to_atom(LineCodes,Line),
	check_eof(Line),
	thread_self(Self),
	format("~a: <<< ~a~n",[Self,Line]).
	
	
my_read_term(InStream,Term,Options):-
	read_term(InStream,Term,Options),
	thread_self(Self),
	write(Self),write(': <<< '),write(Term),nl.
	
my_write_term(OutStream,Elm,Options):-
	write_term(OutStream,Elm,Options),
	nl(OutStream),
	thread_self(Self),
	write(Self),
	write(': >>> '),
	write_term(Elm,Options),
	nl.
	
my_format(OutStream,Format,Args):-
	format(OutStream,Format,Args),
	thread_self(Self),
	write(Self),write(': >>> '),
	format(current_output,Format,Args),
	flush_output(OutStream),
	flush_output(current_output).
	
write_escaped(Out,Atom):-
		nonvar(Atom),
		( 	atom(Atom)
		->  atom_chars(Atom,Chars),
		write_escaped_l(Out,Chars)
	;	term_to_atom(Atom,AAtom),
		write_escaped(Out,AAtom)
	).

	
		
write_escaped_l(_,[]).
write_escaped_l(Out,['<'|ITail]):-
	write(Out,'&lt;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,['>'|ITail]):-
	write(Out,'&gt;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,['{'|ITail]):-
	write(Out,'&cbo;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,['}'|ITail]):-
	write(Out,'&cbc;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,['&'|ITail]):-
	write(Out,'&amp;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,['"'|ITail]):-
	write(Out,'&quot;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,['\''|ITail]):-
	write(Out,'&apos;'),
		write_escaped_l(Out,ITail).
write_escaped_l(Out,[C|ITail]):-
		put_char(Out,C),
		write_escaped_l(Out,ITail).	