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
	    