:- module(pdt_console_server,[
	pdt_current_console_server/2,
	pdt_start_console_server/2
]).
:- use_module(library(prolog_server)).
% TODO make this dependency explicit!
%:- use_module(library('org/cs3/pdt/runtime/consult_server')).

% server(-Port,-Lockfile)
%
% used internally to store information about running servers
:- dynamic server/2.

:- at_initialization(mutex_create(pdt_console_server_mux)).
:- at_halt(mutex_destroy(pdt_console_server_mux)).

% pdt_current_console_server(-Port, -LockFile).
% retrieve information about running servers
pdt_current_console_server(Port,LockFile):-
    with_mutex(pdt_console_server_mux,
	    server(Port,LockFile)
    ).
    

% pdt_start_console_server(+Port,+LockFile)
% starts a new console server at a given port,
% creating the specified lock file when done.
% fails if a server is already running.
pdt_start_console_server(Port,LockFile):-
    with_mutex(pdt_console_server_mux,
    	start_server(Port,LockFile)
    ).

start_server(Port,LockFile):-
    \+ current_thread(prolog_server,_),
    prolog_server(Port, []),
    assert(server(Port,LockFile)),
    consult_server:create_lock_file(LockFile).
	    