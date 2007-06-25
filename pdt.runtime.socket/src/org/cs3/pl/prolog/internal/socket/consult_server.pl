%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test
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

% Author: Lukas
% Date: 23.10.2004

:- module(consult_server,[
	consult_server/1,
	consult_server/2
	]). 


:- use_module(library(socket)).
:- use_module(library(memfile)).

option_default(interprete_lists,true).
option_default(canonical,false).



:- dynamic option_value/2.
:- thread_local option_value/2.
option(Name,Value):-
    option_value(Name,AValue),
    !,
    Value=AValue.
option(Name,Value):-
	option_default(Name,Value).
	
set_option(Name,Value):-
	retractall(option_value(Name,_)),
	assert(option_value(Name,Value)).

unset_option(Name):-	
	retractall(option_value(Name,_)).
	
clear_options:-
	retractall(option_value(_,_)).	
		    


create_lock_file(Filename):-
	(	exists_file(Filename)
	->	debug(consult_server,"Found existing lock file ~w.~n Shutting down...~n",[Filename]),		
		thread_signal(main,halt)
	;	open(Filename, write, Stream),
		call_cleanup(
			(	write(Stream,Filename),
				nl(Stream)
			),
			close(Stream)
		)
	).
	
delete_lock_file(Filename):-
	(	exists_file(Filename)
	->	delete_file(Filename)
	;	true
	).



	
consult_server(Port):- 
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	concat_atom([consult_server,'@',Port],Alias),
	%accept_loop(ServerSocket).
	recordz(pif_flag,port(Port)),
	thread_create(accept_loop(ServerSocket), _,[alias(Alias)]).

consult_server(Port,Lockfile):-
%    open_null_stream(NullStream),
    (	'$log_dir'(LogDir)
	->	open_log(LogDir,main,LogStream),
		thread_at_exit(
			(
				close(LogStream)
			)
		),
		set_prolog_IO(user_input,LogStream,LogStream)
	;	true
	),
	
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	concat_atom([consult_server,'@',Port],Alias),
	%accept_loop(ServerSocket).
	recordz(pif_flag,port(Port)),
	thread_create(accept_loop(ServerSocket), _,[alias(Alias)]),
	create_lock_file(Lockfile).




	
accept_loop(ServerSocket):-    
    (	'$log_dir'(LogDir)
	->	open_null_stream(NullStream),
		open_log(LogDir,accept_loop,LogStream),
		thread_at_exit(
			(	close(NullStream),
				close(LogStream)
			)
		),
		set_prolog_IO(NullStream,LogStream,LogStream)
	;	true
	),
	
	catch(
		accept_loop_impl(ServerSocket),
		Error,
		(
			debug(consult_server,"accept loop encountered an error:~w~n. Shutting down...~n",[Error]),
			thread_signal(main,halt)
		)
	),
	debug(consult_server,"signaling main to shutdown... ~n",[]),
	thread_signal(main,do_shutdown),
	debug(consult_server,"shutdown signal send, exit current thread. ~n",[]),
	thread_exit(0).


:- multifile pif_shutdown_hook/0.
:- dynamic pif_shutdown_hook/0.
:- multifile user:'$log_dir'/1.
:- dynamic user:'$log_dir'/1.

pif_shutdown_hook.

call_shutdown_hook:-
    forall(pif_shutdown_hook,true).
    
do_shutdown:-
   	debug(consult_server,"begin shutdown ~n",[]),
   	call_shutdown_hook,
    %join any thread that is not main.
    (	current_thread(Id,_),
	    do_shutdown_X(Id),
       	fail
	;	debug(consult_server,"shutdown complete~n",[]),
		threads,
		halt
	).
do_shutdown_X(Id):-
    Id\==main,
    debug(consult_server,"joining ~w~n",[Id]),
    thread_join(Id,Status),
    debug(consult_server,"successfully joined ~w, status: ~w ~n",[Id,Status]).
    
	
accept_loop_impl(ServerSocket) :-
	tcp_accept(ServerSocket, Slave, Peer),
	accept_loop_impl_X(ServerSocket,Slave,Peer).

accept_loop_impl_X(ServerSocket,Slave,_):-
    recorded(pif_flag,shutdown),
    !,
    % the accepted connection is just a "wakeup call" we can savely discard it.
    tcp_close_socket(Slave),
    % that's it, we are closing down business.
    tcp_close_socket(ServerSocket).

accept_loop_impl_X(ServerSocket,Slave,Peer):-
	tcp_open_socket(Slave, InStream, OutStream),
	term_to_atom(Peer, Host),
	debug(consult_server,"connect from host ~w~n",[Host]),
	unused_thread_name(handle_client,'',Alias),
	debug(consult_server,"handler thread alias: ~w~n",[Alias]),	
	flush,
	thread_create(
		(	(	'$log_dir'(LogDir)
			->	open_log(LogDir,Alias,LogStream),
				open_null_stream(NullStream),
				set_prolog_IO(NullStream,LogStream,LogStream)
			;	true
			),
			
			thread_at_exit(
				(	debug(consult_server,"Thread is exiting. Trying to close the connection...~n",[]),
					catch(
						(	byebye(InStream,OutStream),
							debug(consult_server,"Connection closed successfully. ~n",[])
						),
						E,
						debug(consult_server,"Error encountered while closing the connection: ~w.~n",[E])
					),
					(	nonvar(LogStream)
					->	close(LogStream),
						close(NullStream)
					;	true
					)

				)
			),
			handle_client(InStream, OutStream)
		), _ , [alias(Alias),detached(true)]
	),
	debug(consult_server,"successfully created thread ~w.~n",[Alias]),
	flush,	
	accept_loop_impl(ServerSocket).

open_log(LogDir,Alias,Stream):-
    concat_atom([LogDir,'/',Alias,'.log'],'',Path),
    debug(consult_server,"trying to open log file for writing: ~w~n",[Path]),
	flush,
    open(Path,append,Stream),
    format(Stream,"~n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),
    debug(consult_server,"successfully opened log file for writing: ~w~n",[Path]),
	flush.
    
	
handle_client(InStream, OutStream):-    
	
	repeat,
	debug(consult_server,"start hanlde_client~n",[]),
	catch(
		handle_client_impl(InStream,OutStream),
		Error,
		(handle_exception(InStream,OutStream,Error,Action),!,Action==stop)
					
	),
	!,
		
	debug(consult_server,"Thread exiting...~n",[]),
	thread_exit(0).    
	
handle_client_impl(InStream, OutStream):-
    set_stream(InStream,encoding(utf8)),
    set_stream(OutStream,encoding(utf8)),
    repeat,
		request_line(InStream,OutStream,'GIVE_COMMAND',Command),
		( handle_command(InStream,OutStream,Command,Next)
		->report_ok(OutStream)
		;	report_error(OutStream, 'failed, sorry.'),
			Next=continue
		),
	Next==stop,
	!.
		

		
handle_command(_,_,'BYE',stop).	
handle_command(_,_,'SHUTDOWN',stop):-	
	% stop accept loop:
	% we set the shutdown flag (which is read by the accept loop)
	% then we have to kick the accept loop out of the tcp_accept/3 call.
	% we do this by simply opening a connection to the listen port.

	recordz(pif_flag,shutdown),
	recorded(pif_flag,port(Port)),
	tcp_socket(Socket),
	tcp_connect(Socket,localhost:Port),
	tcp_close_socket(Socket).
handle_command(_,_,'',continue):-
	clear_options.
handle_command(_,OutStream,'PING',continue):-
		current_prolog_flag(pid,Pid),
    thread_self(Alias),
	my_format(OutStream,"PONG ~w:~w~n",[Pid,Alias]).
handle_command(InStream,OutStream,'ENTER_BATCH',continue):-
	my_format(OutStream,"GO_AHEAD~n",[]),
	repeat,
		handle_batch_messages(OutStream),
		my_read_command(InStream,Term),
		handle_batch_command(Term,InStream,OutStream),
		Term=end_of_batch,!.
handle_command(InStream,OutStream,'QUERY',continue):-
	my_format(OutStream,"GIVE_TERM~n",[]),	
	call_save(OutStream,my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/])),
	( iterate_solutions(InStream,OutStream,Term,Vars)
	; true
	).
handle_command(InStream,OutStream,'QUERY_ALL',continue):-
	my_format(OutStream,"GIVE_TERM~n",[]),
	call_save(OutStream,my_read_term(InStream,Term,[variable_names(Vars)/*,double_quotes(string)*/])),		
	(
		all_solutions(OutStream,Term,Vars)
	;
		true
	).
handle_command(InStream,OutStream,'SET_OPTION',continue):-
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	request_line(InStream,OutStream,'GIVE_TERM',Term),
	call_save(OutStream,set_option(Symbol,Term)).
handle_command(InStream,OutStream,'GET_OPTION',continue):-
	request_line(InStream,OutStream,'GIVE_SYMBOL',Symbol),
	call_save(OutStream,
		(	option(Symbol,Term),
			my_format(OutStream,"~w~n",[Term])
		)
	).


my_read_command(InStream,Term):-
    my_read_term(InStream,Term,[]).

my_read_goal(InStream,Term,Vars):-
    my_read_term(InStream,Term,[variable_names(Vars)]).

		
handle_batch_messages(OutStream):-
    repeat,    
    (	thread_peek_message(batch_message(Message)),
    	debug(consult_server,"recieved message: ~w~n",[Message]),
    	handle_batch_message(Message, OutStream)
    ->	thread_get_message(batch_message(Message)), 
		debug(consult_server,"dequeued message: ~w~n",[Message]),
    	fail
    ;	(	var(Message)
    	->	true
    	;	debug(consult_server,"could not handle message: ~w~n",[Message])
	    )
    ),!.
    
%note on aborts: an abort request is complete if BOTH the async abort message aswell as the
%sync abort marker have been recieved. I originally assumed that the async message would always 
%preceed the marker, but it seems to be more tricky. So i will now handle this symetrically.

record_abort_request(Type,Id):-
    thread_self(Thread),
    (	recorded(pif_batch_abort,request(Thread,Type,Id))
    ->	true
    ;	recordz(pif_batch_abort,request(Thread,Type,Id))
    ).
    

erase_abort_request(Type,Id):-
    thread_self(Thread),
    (	recorded(pif_batch_abort,request(Thread,Type,Id),Ref)
    ->	erase(Ref)
    ;	true
    ).
    
abort_requested(Type,Id):-
	thread_self(Thread),
	recorded(pif_batch_abort,request(Thread,Type,Id)).    

aborting:-
    abort_requested(async,_).

send_abort_complete(Id,OutStream):-    
   	my_format(OutStream, "ABORT_COMPLETE: ~w~n",[Id]).
    
handle_batch_message(abort(Id),OutStream):-    
	debug(consult_server,"recieved abort (async, id=~w)~n",[Id]),
	(	abort_requested(sync,Id)
	->	erase_abort_request(sync,Id),
		send_abort_complete(Id,OutStream)
	;	record_abort_request(async,Id),
		debug(consult_server,"recorded abort (async, id=~w)~n",[Id])
	).
handle_batch_command(abort(Id),_,OutStream):-
    debug(consult_server,"recieved abort (sync, id=~w)~n",[Id]),
	(	abort_requested(async,Id)
	->	erase_abort_request(async,Id),
		send_abort_complete(Id,OutStream)
	;	record_abort_request(sync,Id),
	    debug(consult_server,"recorded abort (sync, id=~w)~n",[Id])
	).
handle_batch_command(join(Id),_,OutStream):-
	my_format(OutStream, "JOIN_COMPLETE: ~w~n",[Id]).
handle_batch_command(end_of_batch,InStream,OutStream):-
	forall(abort_requested(async,Id),handle_batch_command(abort(Id),InStream,OutStream)),
	forall(abort_requested(sync,Id),handle_batch_message(abort(Id),OutStream)),
	my_format(OutStream, "END_OF_BATCH_COMPLETE~n",[]).
handle_batch_command(query_once(Id),InStream,OutStream):-
	aborting,
    !,
    my_read_goal(InStream,_,_),    
    my_format(OutStream, "SKIPPING_QUERY: ~w~n",[Id]).
handle_batch_command(query_all(Id),InStream,OutStream):-
    aborting,
    !,
    my_read_goal(InStream,_,_),
    my_format(OutStream, "SKIPPING_QUERY: ~w~n",[Id]).
    
handle_batch_command(query_once(Id),InStream,OutStream):-
    my_format(OutStream, "RESULTS_FOR_QUERY: ~w~n",[Id]),
    call_save(OutStream,(
    		my_read_goal(InStream,Goal,Vars),
    		one_solution(OutStream,Goal,Vars)
    		)
    	).
handle_batch_command(query_all(Id),InStream,OutStream):-
    my_format(OutStream, "RESULTS_FOR_QUERY: ~w~n",[Id]),
    call_save(OutStream,(
    		my_read_goal(InStream,Goal,Vars),
    		solutions_weak_until_cut(OutStream,Goal,Vars)
    		)
    	).

call_save(OutStream, Goal):-
    catch(Goal,
	    Error,
    		report_error(OutStream,Error)
    	).

solutions_weak_until_cut(OutStream,Term,Vars):-
	(	solutions_until_cut(OutStream,Term,Vars)
	->	my_format(OutStream, "CUT~n",[])
	;	solutions_yes_or_no(OutStream)
	).


solutions_until_cut(OutStream,Term,Vars):-
	user:Term,
	nb_setval(hasSolutions,1),
	print_solution(OutStream,Vars),
	goal_was_cut(OutStream),!.	

goal_was_cut(OutStream):-
	handle_batch_messages(OutStream),
	aborting.
	
solutions_yes_or_no(OutStream):-
	(	nb_current(hasSolutions,1)
	->	my_format(OutStream,"YES~n",[]),
		nb_delete(hasSolutions)
	; 	my_format(OutStream,"NO~n",[])
	).

	
	


one_solution(OutStream,Term,Vars):-
	( 	user:Term
	->	consult_server:print_solution(OutStream,Vars),
		my_format(OutStream,"YES~n",[])
	; 	my_format(OutStream,"NO~n",[])
	).
	
	
all_solutions(OutStream,Term,Vars):-
	user:forall(
		catch(Term,E,throw(wrapped(E))),
		(
			consult_server:print_solution(OutStream,Vars),
			nb_setval(hasSolutions,1)
		)		
	),
	(	nb_current(hasSolutions,1)
	->	my_format(OutStream,"YES~n",[]),
		nb_delete(hasSolutions)
	;	my_format(OutStream,"NO~n",[])
	).
	
	
iterate_solutions(InStream,OutStream,Term,Vars):-
	( user:forall(
			catch(Term,E,throw(wrapped(E))),
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
		print_binding(OutStream,Key,Val)
	),
	my_format(OutStream,"END_OF_SOLUTION~n",[]).
	
print_binding(Out,Key,Val):-
		my_write(Out,'<'),
		write(Out,Key),
		my_write(Out, '>'),		
		print_value(Out,Val),		
		nl(Out).

print_values([],_). 
print_values([Head|Tail],Out):-
	print_value(Out,Head),		
	print_values(Tail,Out).
	

print_value(Out,Val):-    	
	option(canonical,true),
	!,
	my_write(Out,'<'),
	(write_escaped(Out,Val);true),
	my_write(Out, '>').
print_value(Out,Val):-    	
	( 	is_list(Val), option(interprete_lists,true)
 	->	my_write(Out,'{'),
		print_values(Val,Out),
		my_write(Out, '}')		
	;	my_write(Out,'<'),
		write_escaped(Out,Val),
		my_write(Out, '>')
	).



handle_exception(InStream,OutStream,Error,Action):-
    debug(consult_server, "handle_excpetion (pre): Up:~w, Down:~w, Error:~w~n",[InStream,OutStream,Error]),
    handle_exception_X(InStream,OutStream,Error,Action),
    debug(consult_server, "handle_excpetion (post): Action:~w~n",[Action]).	
    
handle_exception_X(InStream,OutStream,Error,Action):-
	var(Error),
	!,
	handle_exception(InStream,OutStream,unbound_error_term,Action).	
	

	
handle_exception_X(_InStream,OutStream,peer_reset,continue):-
	catch(
		(
			my_format(OutStream,"RESET~n",[]),
			report_ok(OutStream)
		),							
		_,(
		%	shut_down(InStream,OutStream),
			fail
			)
	),
	!.
	
handle_exception_X(_InStream,OutStream,wrapped(Error),continue):-
	catch(		
		report_error(OutStream,Error),					
		_,(
%			shut_down(InStream,OutStream),
			fail
			)
	),
	!.
	
handle_exception_X(_InStream,_OutStream,Error,stop):-
	debug(consult_server,"Unhandled Exception :~w~n Trying to shut down...~n",[Error]).
	
	
	
report_ok(OutStream):-
	my_format(OutStream,"OK~n",[]).	
	
report_error(OutStream, Error):-
	(	var(Error)
	->	my_format(OutStream,"ERROR: unbound error term~n",[])
	;	my_format(OutStream,"ERROR: ~w~n",[Error])
	).			
	
		
byebye(InStream,OutStream):-
	debug(consult_server,"byebye called~n",[]),
	(	is_stream(OutStream)
	->	my_format("Downstream is a stream: ~w~n",[OutStream]),
		my_format("sending BYE downstream: ~w~n",[OutStream]),
		my_format(OutStream,"BYE~n",[]),
		my_format("closing downstream: ~w~n",[OutStream]),		
		catch(close(OutStream),E,
			my_format("Problem closing downstream: ~w~n",[E])
		)
	;	my_format("Downstream is no stream: ~w~n",[OutStream])
	),
	(	is_stream(InStream)	
	->	my_format("Upstream is a stream: ~w~n",[InStream]),
		my_format("closing upstream: ~w~n",[InStream]),		
		catch(close(InStream),E,
			my_format("Problem closing upstream: ~w~n",[E])
		)
	;	my_format("Upstream is no stream: ~w~n",[InStream])
	).
	
	

	
	
	
codes_or_eof_to_atom(end_of_file,_):-
	throw(end_of_file).
	
codes_or_eof_to_atom(Codes,Atom):-
	atom_codes(Atom,Codes).
	
	
count_thread(Prefix,Count):-
	findall(A,
		(	current_thread(A,_),
			atom_concat(Prefix,_,A),
			my_format("There is e.g. a thread named ~w~n",[A])
		),
		Bag
	),	 
	length(Bag,Count).
	
unused_thread_name(Prefix,Suffix,Name):-
	unused_thread_name(Prefix,Suffix,0,Name).	
	
unused_thread_name(Prefix,Suffix,Try,Name):-
	concat_atom([Prefix,Try,Suffix],A),
	(	current_thread(A,_)
	->	plus(Try,1,Next),
		unused_thread_name(Prefix,Suffix,Next,Name)
	;	Name=A			
	).
	
	
	
	
request_line(InStream, OutStream, Prompt, Line):-
	my_format(OutStream,"~w~n",[Prompt]),
	read_line_to_codes(InStream,LineCodes),
	codes_or_eof_to_atom(LineCodes,Line),
	debug(consult_server,"(Up:~w, read_line_to_codes)<<< ~w~n",[InStream,Line]).
	
	
	
my_read_term(InStream,Term,Options):-
	read_term(InStream,Term,Options),
	debug(consult_server,"(Up:~w read_term) <<<~w~n",[InStream,Term]).
	
my_write_term(OutStream,Elm,Options):-
	debug(consult_server,"(Down:~w write_term) >>>~w~n",[OutStream,Elm]),  
	write_term(OutStream,Elm,Options),
	nl(OutStream).
my_write(OutStream,Term):-
	debug(consult_server,"(Down:~w, write)>>>~w~n",[OutStream,Term]),
	write(OutStream,Term).	
	
my_format(OutStream,Format,Args):-
	append("(Down:~w, format) >>>",Format,Format2),
	append(Format2,"~n",Format3),
	debug(consult_server,Format3,[OutStream|Args]),
	format(OutStream,Format,Args),
	flush_output(OutStream).

my_format(Format,Args):-
    debug(consult_server,Format,Args).
	

write_escaped(Out,Term):-
    write_term_to_memfile(Term,Memfile),
    new_memory_file(TmpFile),
    open_memory_file(Memfile,read,In),
    open_memory_file(TmpFile,write,Tmp),
    escape_stream(In,Tmp),    
    close(In),
    close(Tmp),
    memory_file_to_atom(TmpFile,Atom),
    free_memory_file(TmpFile),
    free_memory_file(Memfile),
    my_write(Out,Atom).

write_term_to_memfile(Term,Memfile):-
	new_memory_file(Memfile),
	open_memory_file(Memfile,write,Stream),
	call_cleanup(
		(	option(canonical,true)
		->	write_canonical(Stream,Term)
		;	write(Stream,Term)
		),
		close(Stream)
	).

escape_stream(In,Out):-
    repeat,	    
    (	at_end_of_stream(In)
    ->	!
    ;   get_char(In,Char),    	
	    write_escaped_char(Char,Out),
	    fail
	).
	

write_escaped_char('<',Out):-
	write(Out,'&lt;').
write_escaped_char('>',Out):-
	write(Out,'&gt;').
write_escaped_char('{',Out):-
	write(Out,'&cbo;').
write_escaped_char('}',Out):-
	write(Out,'&cbc;').
write_escaped_char('&',Out):-
	write(Out,'&amp;').
write_escaped_char('"',Out):-
	write(Out,'&quot;').
write_escaped_char('\'',Out):-
	write(Out,'&apos;').
write_escaped_char(C,Out):-
	put_char(Out,C).	
		


		
