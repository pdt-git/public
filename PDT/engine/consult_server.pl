% Author: Lukas
% Date: 23.10.2004
:- use_module(library(socket)).

:- dynamic user:zombie_symbol/1.

user:consulted_symbol(Symbol) :-
	source_file(Symbol),
	\+user:zombie_symbol(Symbol).

user:delete_symbol(Symbol) :-
	assert(zombie_symbol(Symbol)).	

user:undelete_symbol(Symbol) :-
	retractall(zombie_symbol(Symbol)).
	
user:consult_server(Port):-
    tcp_socket(ServerSocket),
    tcp_setopt(ServerSocket, reuseaddr),
    tcp_bind(ServerSocket, Port),
    tcp_listen(ServerSocket, 5),
    thread_create(accept_loop(ServerSocket), _,[alias(consult_server)]).
    
user:accept_loop(ServerSocket) :-
    tcp_accept(ServerSocket, Slave, Peer),
    tcp_open_socket(Slave, InStream, OutStream),
    tcp_host_to_address(Host,Peer),
    atom_concat('handle_client@', Host, Alias),
	thread_create(handle_client(InStream, OutStream, Slave), _ , [alias(Alias)]),
	accept_loop(ServerSocket).
	
user:handle_client(InStream, OutStream, Slave ):-
    format(OutStream,"GIVE_SYMBOL~n",[]),
    flush_output(OutStream),
	read_line_to_codes(InStream,Codes),
	( 	isEOF(Codes)
    ->	format(OutStream,"BYE~n",[]),
    	close(InStream),
    	close(OutStream)
	;	atom_codes(Symbol,Codes),
	    format(OutStream,"USING_SYMBOL ~a~nGO_AHEAD~n",[Symbol]),
	    flush_output(OutStream),
	    module(user), 
	    catch(load_files(Symbol,[stream(InStream)]	), E,
              ( print_message(error, E),
                format(OutStream,"ERROR~n",[]),
                handle_client(InStream,OutStream,Slave)
              )
          ),	    
	    format(OutStream,"OK~n",[]),
	    handle_client(InStream, OutStream, Slave)
    ).    

isEOF([_:_]):-
	!,
	fail.
isEOF(end_of_file):-
	true.
	
	