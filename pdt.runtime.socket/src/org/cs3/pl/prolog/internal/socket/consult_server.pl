%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(consult_server,[
	consult_server/1,
		consult_server/2,
	consulted_symbol/1,
	starts_at/2
	]). 
% Author: Lukas
% Date: 23.10.2004
:- use_module(library(socket)).

:- dynamic zombie_symbol/1.
:- dynamic offset/2.

create_lock_file(Filename):-
	(	exists_file(Filename)
	->	write('Found existing lockfile, exiting...'),write(Filename),nl,thread_signal(main,halt)
	;	open(Filename, write, Stream),
		write(Stream,Filename),
		nl(Stream),
		close(Stream)
	).
	



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

consult_server(Port,Lockfile):-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	concat_atom([consult_server,'@',Port],Alias),
	%accept_loop(ServerSocket).
	thread_create(accept_loop(ServerSocket), _,[alias(Alias)]),
	create_lock_file(Lockfile).

	
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
    thread_self(Alias),
	my_format(OutStream,"PONG ~a~n",[Alias]).
	
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

handle_command(InStream,OutStream,'ENTER_BATCH'):-
	my_format(OutStream,"GO_AHEAD~n",[]),
	repeat,
		handle_batch_messages,
		my_read_command(InStream,Term),
		handle_batch_command(Term,InStream,OutStream),
		Term=end_of_batch,!.

my_read_command(InStream,Term):-
    my_read_term(InStream,Term,[/*double_quotes(string)*/]).
my_read_goal(InStream,Term,Vars):-
    read_line_to_codes(InStream,Codes),
    atom_codes(Atom,Codes),
    thread_self(Self),
	format("~a: <<< (goal) >~a~n<",[Self,Atom]),
    atom_to_term(Atom,Term,Vars).

		
handle_batch_messages:-
    repeat,    
    (	thread_peek_message(batch_message(Message)),
    	writeln(found_batch_message_in_queue(Message)),
    	handle_batch_message(Message)
    ->	thread_get_message(batch_message(Message)), fail
    ;	writeln(no_more_messages_in_queue),true
    ),!.
handle_batch_message(abort(Id)):-
	recordz(pif_batch_abort,Id).

handle_batch_command(abort(Id),_,OutStream):-
	(	recorded(pif_batch_abort,Id,Ref)
	->	erase(Ref),
		my_format(OutStream, "ABORT_COMPLETE: ~a~n",[Id])
	;	my_format(OutStream, "WARNING: ~a unexpected abort marker~n",[Id])
	).
handle_batch_command(join(Id),_,OutStream):-
	my_format(OutStream, "JOIN_COMPLETE: ~a~n",[Id]).
handle_batch_command(end_of_batch,InStream,OutStream):-
	forall(recorded(pif_batch_abort,Id),handle_batch_command(abort(Id),InStream,OutStream)),
	my_format(OutStream, "END_OF_BATCH_COMPLETE~n",[]).
handle_batch_command(query_once(Id,_),InStream,OutStream):-
    recorded(pif_batch_abort,_),
    !,
    my_read_goal(InStream,_,_),    
    my_format(OutStream, "SKIPPING_QUERY: ~a~n",[Id]).
handle_batch_command(query_all(Id,_),InStream,OutStream):-
    recorded(pif_batch_abort,_),
    !,
    my_read_goal(InStream,_,_),
    my_format(OutStream, "SKIPPING_QUERY: ~a~n",[Id]).
    
handle_batch_command(query_once(Id,Mode),InStream,OutStream):-
    my_format(OutStream, "RESULTS_FOR_QUERY: ~a~n",[Id]),
    call_save(OutStream,(
    		my_read_goal(InStream,Goal,Vars),
    		one_solution(OutStream,Goal,Vars,Mode)
    		)
    	).
handle_batch_command(query_all(Id,Mode),InStream,OutStream):-
    my_format(OutStream, "RESULTS_FOR_QUERY: ~a~n",[Id]),
    call_save(OutStream,(
    		my_read_goal(InStream,Goal,Vars),
    		solutions_weak_until_cut(OutStream,Goal,Vars,Mode)
    		)
    	).

call_save(OutStream, Goal):-
    catch(Goal,
	    Error,
    		report_error(OutStream,Error)
    	).

solutions_weak_until_cut(OutStream,Term,Vars,Mode):-
	(	solutions_until_cut(OutStream,Term,Vars,Mode)
	->	my_format(OutStream, "CUT~n",[])
	;	solutions_yes_or_no(OutStream)
	).


solutions_until_cut(OutStream,Term,Vars,Mode):-
	user:Term,
	nb_setval(hasSolutions,1),
	print_solution(OutStream,Vars,Mode),
	goal_was_cut,!.	

goal_was_cut:-
	handle_batch_messages,
	recorded(pif_batch_abort,_).
	
solutions_yes_or_no(OutStream):-
	(	nb_current(hasSolutions,1)
	->	my_format(OutStream,"YES~n",[]),
		nb_delete(hasSolutions)
	; 	my_format(OutStream,"NO~n",[])
	).

	
	
handle_command(InStream,OutStream,'QUERY'):-
	my_format(OutStream,"GIVE_TERM~n",[]),	
	my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/]),
	( iterate_solutions(InStream,OutStream,Term,Vars,default)
	; true
	).
	
handle_command(InStream,OutStream,'QUERY_ALL'):-
	my_format(OutStream,"GIVE_TERM~n",[]),
	my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/]),		
	(
		all_solutions(OutStream,Term,Vars,default)
	;
		true
	).
	
handle_command(InStream,OutStream,'QUERY_CANONICAL'):-
	my_format(OutStream,"GIVE_TERM~n",[]),	
	my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/]),
	( iterate_solutions(InStream,OutStream,Term,Vars,canonical)
	; true
	).
	
handle_command(InStream,OutStream,'QUERY_ALL_CANONICAL'):-
	my_format(OutStream,"GIVE_TERM~n",[]),
	my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/]),		
	(
		all_solutions(OutStream,Term,Vars,canonical)
	;
		true
	).
	
	
handle_command(_,_,'SHUTDOWN'):-	
	throw(shut_down).
	
handle_command(_,_,'BYE'):-	
	throw(peer_quit).

one_solution(OutStream,Term,Vars,Mode):-
	( 	user:Term
	->	consult_server:print_solution(OutStream,Vars,Mode),
		my_format(OutStream,"YES~n",[])
	; 	my_format(OutStream,"NO~n",[])
	).
	
	
all_solutions(OutStream,Term,Vars,Mode):-
	user:forall(
		Term,
		(
			consult_server:print_solution(OutStream,Vars,Mode),
			nb_setval(hasSolutions,1)
		)		
	),
	(	nb_current(hasSolutions,1)
	->	my_format(OutStream,"YES~n",[]),
		nb_delete(hasSolutions)
	;	my_format(OutStream,"NO~n",[])
	).
	
	
iterate_solutions(InStream,OutStream,Term,Vars,Mode):-
	( user:forall(
			Term,
			(
				consult_server:print_solution(OutStream,Vars,Mode),
				consult_server:request_line(InStream,OutStream,'MORE?','YES')											
			)
		)
	->my_format(OutStream,"NO~n",[])
	; my_format(OutStream,"YES~n",[])
	).
	
	
	
	
print_solution(OutStream,Vars,Mode):-
	forall(
		member(Key=Val,Vars),
	%%			my_write_term(OutStream,Elm,[quoted(true)])		
		print_binding(OutStream,Key,Val,Mode)
	),
	my_format(OutStream,"END_OF_SOLUTION~n",[]).
	
print_binding(Out,Key,Val,Mode):-
		write(Out,'<'),
		(write_escaped(Out,Key,Mode);true),
		write(Out, '>'),		
		(print_value(Out,Val,Mode);true),		
		nl(Out).

print_values(_,[],_):-
	!. 
print_values(Out,[Head|Tail],Mode):-
	!,
	print_value(Out,Head,Mode),		
	print_values(Out,Tail,Mode).
	
print_value(Out,Val,default):-    	
	( 	is_list(Val)
 	->	write(Out,'{'),
		(print_values(Out,Val,default);true),
		write(Out, '}')		
	;	write(Out,'<'),
		(write_escaped(Out,Val,default);true),
		write(Out, '>')
	).

print_value(Out,Val,canonical):-    	
		write(Out,'<'),
		(write_escaped(Out,Val,canonical);true),
		write(Out, '>').

	
handle_exception(InStream,OutStream,Error):-
	var(Error),
	handle_exception(InStream,OutStream,unbound_error_term).	
	
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
	threads,
	thread_signal(main,halt),
	threads.
	
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
	write_escaped(Out,Atom,canonical).
	
write_escaped(Out,Atom,canonical):-
		nonvar(Atom),
		( 	atom(Atom)
		->  atom_chars(Atom,Chars),
		write_escaped_l(Out,Chars)
	;	term_to_canonical_atom(Atom,AAtom),
		write_escaped(Out,AAtom)
	).
write_escaped(Out,Atom,default):-
		nonvar(Atom),
		( 	atom(Atom)
		->  atom_chars(Atom,Chars),
		write_escaped_l(Out,Chars)
	;	term_to_atom(Atom,AAtom),
		write_escaped(Out,AAtom)
	).


term_to_canonical_atom(Term,AAtom):-
	new_memory_file(Memfile),
	open_memory_file(Memfile,write,Stream),
%	write_term(Stream,Term,[ignore_ops(true),quoted(true)]),
	write_canonical(Stream,Term),
	close(Stream),
	memory_file_to_atom(Memfile,AAtom),
	free_memory_file(Memfile).
	
    
		
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