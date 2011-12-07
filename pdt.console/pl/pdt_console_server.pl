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

/* NOTE: This file contains third-party code!

   Most of this file was borrowed from the swi-prolog library 
   prolog_server. Many thanks to the original authors for making their 
   work available to the public. 
   
   I changed the following things:
   1) added a way to gracefully stop the console server accept loop
   2) changed the naming policy for thread alias names, so that more
      than one client may connect from the same host.
   
   The copyright header of the original file 
   follows.
   	--lu
*/

/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker & Steve Prior
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/
   
   

:- module(pdt_console_server,[
	pdt_current_console_server/1,
	pdt_start_console_server/1,
	pdt_stop_console_server/0
]).
:- use_module(library(socket)).
 

:- dynamic(console_thread_name/1).

prolog_server(Port,Options) :-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	thread_create(server_loop(ServerSocket, Options), _,
		      [ alias(pdt_console_server)
		      ]).
 
server_loop(ServerSocket, Options):-
    server_loop_impl(ServerSocket,Options),
    thread_exit(0).
server_loop_impl(ServerSocket, Options) :-
	tcp_accept(ServerSocket, Slave, Peer),
	server_loop_impl_X(ServerSocket,Options,Slave,Peer).

server_loop_impl_X(ServerSocket,_,Slave,_) :-
	recorded(pdt_console_server_flag,shutdown,Ref),
	!,
	erase(Ref),
	% the accepted connection is just a "wakeup call" we can savely discard it.
    tcp_close_socket(Slave),
    % that's it, we are closing down business.
    tcp_close_socket(ServerSocket).	
server_loop_impl_X(ServerSocket,Options,Slave,Peer):-	
	tcp_open_socket(Slave, InStream, OutStream),
	tcp_host_to_address(Host, Peer),
	flag(pdt_console_client_id,Id,Id+1),
	concat_atom(['pdt_console_client_',Id,'@',Host],Alias),
	thread_create(service_client(InStream, OutStream, Peer, Options),
		      ID,
		      [ alias(Alias)
		      ]),
	assert(console_thread_name(ID)),
	server_loop_impl(ServerSocket, Options).
 
service_client(InStream, OutStream, Peer, Options) :-
	allow(Peer, Options), !,
	thread_self(Id),
	set_prolog_IO(InStream, OutStream, OutStream),
	format(user_error,
	       'Welcome to the SWI-Prolog server on thread ~w~n~n',
	       [Id]),
	run_prolog,
	close(InStream),
	close(OutStream),
	thread_detach(Id).
service_client(InStream, OutStream, _, _):-
	thread_self(Id),
	format(OutStream, 'Go away!!~n', []),
	close(InStream),
	close(OutStream),
	thread_detach(Id).


run_prolog :-
	catch(prolog, E,
	      ( print_message(error, E),
%		E = error(_, _),
		run_prolog)).


allow(Peer, Options) :-
	(   member(allow(Allow), Options)
	*-> Peer = Allow,
	    !
	;   Peer = ip(127,0,0,1)
	).

% TODO make this dependency explicit!
%:- use_module(library('org/cs3/pdt/runtime/consult_server')).

% server(-Port)
%
% used internally to store information about running servers
:- dynamic(server/1).

%:- initialization(mutex_create(pdt_console_server_mux)).
%:- at_halt(mutex_destroy(pdt_console_server_mux)).

% pdt_current_console_server(-Port, -LockFile).
% retrieve information about running servers
pdt_current_console_server(Port) :-
    with_mutex(pdt_console_server_mux,
	    server(Port)
    ).
    

% pdt_start_console_server(?TCPPort)
% starts a new console server.
% UDPPort is used for sending back a sync when the server is up.
pdt_start_console_server(Port) :-
    with_mutex(pdt_console_server_mux,
    	start_server(Port)
    ).

% pdt_stop_console_server(+LockFile)
% stops the console server, removing the Lockfile.
pdt_stop_console_server:-
    with_mutex(pdt_console_server_mux,
    	stop_server
    ).

:- multifile(consult_server:pif_shutdown_hook/0).
consult_server:pif_shutdown_hook:-
    pdt_stop_console_server.

start_server(Port) :-
    \+ thread_property(_, alias(pdt_console_server)),
    prolog_server(Port, []),
    assertz(server(Port)).

stop_server :-
	server(Port),
	!,
	do_stop_server(Port,_LockFile).
stop_server.

do_stop_server(Port) :-
	recordz(pdt_console_server_flag, shutdown),
	tcp_socket(Socket),
	tcp_connect(Socket, localhost:Port),
	tcp_close_socket(Socket),
	retractall(server(Port)).