:- module(prolog_interface,[
	prolog_interface/1,	
	handle_client/2,
	notify/2
]).
% Author: Lukas
% Date: 23.10.2004
:- use_module(library(socket)).
:- dynamic client/1.
:- dynamic observer/2.
:- dynamic file_provider/2.
:- dynamic offset/2.


living_observer(Id,Subject):-
    client(Id),
    observer(Id,Subject).

living_file_provider(Id,Prefix):-
    client(Id),
    file_provider(Id,Prefix).

notify(Subject,Event):-
    forall(
    	living_observer(Id,Subject),
    	thread_send_message(Id,notify(Subject,Event))
    ).
user:prolog_load_file(Spec,Options):-
    format("user:prolog_load_file(~w,~w)~n",[Spec,Options]),
    \+ memberchk(wrapped,Options),
    (	consult_canidate(Spec,Module,Prefix,File)
    ->	living_file_provider(Id,Prefix),
    	!,
    	thread_self(T),
    	new_memory_file(MemFile),
    	call_cleanup(
    		(
		    	thread_send_message(Id,get_file(T,Prefix,File,MemFile)),    	
	    		thread_get_message(reply(get_file(T,Prefix,File,MemFile),Status)),
		    	!,
				Status==yes,
				open_memory_file(MemFile,read,Stream),
				load_stream(Module,pdt:Prefix:File,Stream,Options)	    ,
				close(Stream)
			),
			free_memory_file(MemFile)
		)
    ;	load_files(Spec,[wrapped|Options])
    ),    
    notify(consulted,Spec).
    
consult_canidate(Spec,Module,Prefix,File):-
    strip_module(Spec,Module,Plain),
    consult_canidate(Plain,Prefix,File).

consult_canidate(Plain,Prefix,File):-    
    atom(Plain),
    concat_atom(['pdt',Prefix,File],':',Plain).
    

prolog_interface(Port):-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	concat_atom([prolog_interface,'@',Port],Alias),
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
	tcp_accept(ServerSocket, Slave, _),
	tcp_open_socket(Slave, In, Out),
	thread_create(handle_client(In, Out), Id ,[]),
	assert(client(Id)),
	accept_loop_impl(ServerSocket).
	
handle_client(In, Out):-    
	thread_self(Me),
	catch(
		(
			write_header(Out),
			utter(In,Out,session,[],session(In,Out)),
			nl(Out),
			flush_output(Out)
		),
		E,
		format("on thread ~a:~w~n Thread will exit.~n",[Me,E])
	),
	catch(
		(
			close(In),
			close(Out)			
		),
		E,
		format("on thread ~a:~w~n Thread will exit.~n",[Me,E])
	),
	%thread_detach(Me),
	retractall(client(Me)),
	retractall(observer(Me,_)),
	retractall(file_provider(Me,_)),
	thread_exit(0).
	



session(In, Out):-
	repeat,
	write(Out,'<ok/>'),
	nl(Out),
    flush_output(Out),
	dispatch(In,Out,Codes),	
	handle_command(In,Out,Codes),
	byebye(Codes),
	!.

dispatch(In,Out,Codes):-
    repeat,
    ( 	thread_peek_message(A)
    ->	thread_get_message(A),
    	(	handle_message(In,Out,A)
    	->	nl(Out),
		    flush_output(Out)
		;	true
		)
    ;	true	
    ),
    wait_for_input([In],[In],0.1),
    !,
    read_line_to_codes(In,Codes).
    

handle_message(_,_,shutdown):-
    throw(shutdown).
handle_message(In,Out,get_file(T,Prefix,File)):-
    utter(In,Out,request,[type=file,subject=Prefix, name=File],
		(
		    nl(Out),flush_output(Out),
		    (	read_line_to_codes(In,['y','e','s']),
		    	stream_to_memfile(In,MemFile)
			->  thread_send_message(T,reply(get_file(T,Prefix,File),MemFile))
			;	thread_send_message(T,reply(get_file(T,Prefix,File),_))
			)
		)
	).
handle_message(In,Out,notify(Subject,Event)):-
    utter(In,Out,event,[subject=Subject],write_escaped(Out,Event)).

	
load_stream(Module,Symbol,Stream,Options):-    
    	retractall(offset(Symbol,_)),
    	line_count(Stream,Line),
    	assert(offset(Symbol,Line)),
    	Module:load_files(Symbol,[stream(Stream)|Options]).
    	    
byebye(bye).   
byebye(end_of_file):-
    fail.
byebye([]):-
    fail.
byebye([H|T]):-
    atom_codes(Atom,[H|T]),
    byebye(Atom).
    
handle_command(_,_,[]).
handle_command(_,_,end_of_file):-
    throw(peer_reset).
handle_command(In,Out,[H|T]):-
    atom_codes(Atom,[H|T]),
    utter(
		In,
		Out,
		'reply',
		[command=Atom],
		(			
			atom_to_term(Atom,Command,Vars),
			handle_command(In,Out,Command,Vars)
		)
	).
	
	
handle_command(_,_,'',[]).	
handle_command(_,_,'bye',[]).	
handle_command(_,_,register_file_provider(Prefix),[]):-
    thread_self(Me),
    assert(file_provider(Me,Prefix)).
handle_command(_,_,unregister_file_provider(Prefix),[]):-
    thread_self(Me),
    retractall(file_provider(Me,Prefix)).
handle_command(_,_,register_observer(Subject),[]):-
    thread_self(Me),
    assert(observer(Me,Subject)).
handle_command(_,_,unregister_observer(Subject),[]):-
    thread_self(Me),
    retractall(observer(Me,Subject)).

handle_command(_,_,halt(Timeout),[]):-
    thread_create((sleep(Timeout),thread_signal(main,halt)),_,[]),
    forall(
    	(current_thread(T,running),client(T)),
    	(	
    		thread_self(T)
    	->	true
    	;	thread_send_message(T,shutdown),
			thread_join(T,_)
    	)
    ),
    thread_at_exit(thread_signal(main,halt)),    
    throw(shutdown).    
handle_command(In,Out,query_all(Goal),Vars):-
    all_solutions(In,Out,Goal,Vars).
handle_command(In,Out,query_once(Goal),Vars):-
    one_solution(In,Out,Goal,Vars).
handle_command(In,Out,query(Goal),Vars):-
    iterate_solutions(In,Out,Goal,Vars).
handle_command(In,Out,consult(File),[]):-
    load_stream(user,File,In,[]),
		notify(consulted,Spec).		

    

    
all_solutions(In,Out,Term,Vars):-
	forall(
		Term,
		utter_solution(In,Out,Vars)
	).

one_solution(In,Out,Term,Vars):-
	Term,
	!,
	utter_solution(In,Out,Vars).
	
iterate_solutions(In,Out,Term,Vars):-
	Term,
	utter_solution(In,Out,Vars),
	nl(Out),
	flush_output(Out),
	get_char(In,'!'),
	!,
	true.

    

utter_solution(In,Out,Vars):-
    utter(In,Out,'tuple',[],
		forall(
			member(Elm,Vars),
				utter_binding(In,Out,Elm)		
		)
	).
	
utter_binding(In,Out,Name=Value):-
    utter(In,Out,'binding',[name=Name],
	    write_escaped(Out,Value)
	).
	

utter(In,Out,Tag,AttrList,Goal):-
	utter(In,Out,Tag,AttrList,Goal,no_flush).
	    
utter(In,Out,Tag,AttrList,Goal,no_flush):-
    write(Out,'<'),
	write(Out,Tag),
	write_attr_list(Out,AttrList),
	write(Out, '>'),
	call_cleanup(
		Goal,
		Catcher,
		(
			handle_catcher(In,Out,Catcher,Goal),
			write(Out,'</'),
			write(Out,Tag),
			write(Out, ' '),
			write(Out, '>')			
		)
	),
	!.

utter(In,Out,Tag,AttrList,Goal,inner_flush):-
    write(Out,'<'),
	write(Out,Tag),
	write_attr_list(Out,AttrList),
	write(Out, '>'),
	flush_output(Out),
	call_cleanup(
		Goal,
		Catcher,
		(
			handle_catcher(In,Out,Catcher,Goal),
			write(Out,'</'),
			write(Out,Tag),
			write(Out, ' '),
			write(Out, '>')			
		)
	),
	!.


write_attr_list(_,[]).	
write_attr_list(Out,[Name=Value|Tail]):-
	write(Out,' '),
	write(Out,Name),
	write(Out,'="'),
	write_escaped(Out,Value),
	write(Out, '"'),
	write_attr_list(Out,Tail).
	
handle_catcher(_,_,exit,_).
handle_catcher(_,_,!,_).
handle_catcher(_,Out,fail,Goal):-
    write(Out,'<failed>'),
    write_escaped(Out,Goal),
    write(Out,'</failed>').
handle_catcher(_,Out,Error,_):-
	write(Out,'<exception>'),
	write_escaped(Out,Error),
	write(Out,'</exception>').
	
	
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
    
write_header(Out):-
	write(Out,'<?xml version=\'1.0\' encoding=\"ISO-8859-1\"?>'),nl(Out),
	write(Out,'<!DOCTYPE session ['),nl(Out),
	write(Out,'	<!ELEMENT session ((reply|request|event|ok)*,exception?)>'),nl(Out),
	write(Out,'	<!ELEMENT ok EMPTY>'),nl(Out),	
	write(Out,'	<!ELEMENT reply (tuple*,(failed|exception)?)>'),nl(Out),
	write(Out,'	<!ATTLIST reply'),nl(Out),
	write(Out,'command CDATA #REQUIRED>'),nl(Out),
	write(Out,'	<!ELEMENT request (#PCDATA)>'),nl(Out),
	write(Out,'<!ATTLIST request'),nl(Out),
	write(Out,'type (file|line|char) #REQUIRED'),nl(Out),
	write(Out,'subject CDATA #REQUIRED>'),nl(Out),
	write(Out,'	<!ELEMENT event (#PCDATA)>'),nl(Out),
	write(Out,'<!ATTLIST event'),nl(Out),
	write(Out,'subject CDATA #REQUIRED>'),nl(Out),
	write(Out,'<!ELEMENT tuple (binding*,(failed|exception)?)>'),nl(Out),
	write(Out,'<!ELEMENT binding (#PCDATA)>'),nl(Out),
	write(Out,'<!ATTLIST binding'),nl(Out),
	write(Out,'name	 CDATA #REQUIRED>'),nl(Out),
	write(Out,'<!ELEMENT failed (#PCDATA)>'),nl(Out),
	write(Out,'<!ELEMENT exception (#PCDATA)>'),nl(Out),
	write(Out,']>'),nl(Out).
	
	
	
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
	
