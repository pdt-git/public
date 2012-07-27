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

/**
 * to activate debugging for this module, uncomment:
 */
 

:- module(pif_observe,[
	pif_subscribe/2,
	pif_subscribe/3,
	pif_unsubscribe/2,	
	pif_unsubscribe/1,
	pif_notify/2
]).

%:- debug(pif_observe).

:- use_module(library(socket)).

:- dynamic observing/3, socket/1.

%%
% pif_subscribe(+Address,+Subject).
%
% @param Address host:port, host is a name, or an ip/4 term.
% @param subject the subject term to listen for.
pif_subscribe(Address,Subject):-
    pif_subscribe(Address,Subject,_).


pif_subscribe(Address,Subject,Ticket):-
    term_hash((Address,Subject),Ticket),
    assert(observing(Address,Subject,Ticket)),
    (	socket(_)
    ->	true
    ;	udp_socket(USocket),
    	assert(socket(USocket))
    ).
    
%%
% pif_unsubscribe(+Address,+Subject).
%
pif_unsubscribe(Address,Subject):-
 	retractall(observing(Address,Subject,_)),
 	(	socket(S), \+ observing(_,_,_)
 	->	tcp_close_socket(S),
 		retractall(socket(_))
 	).

pif_unsubscribe(Ticket):-
    retract(observing(_,_,Ticket)),
 	(	socket(S), \+ observing(_,_,_)
 	->	tcp_close_socket(S),
 		retractall(socket(_))
 	).
 	


pif_notify(Subject,Event) :-
	debug(pif_observe,'~w~n',[pif_notify(Subject,Event)]),
	forall(
		observing(Address,Subject,Ref),
		notify_X(Address,Subject,Ref,Event)
	).   

notify_X(Address,Subject,Ref,Event):-	
	socket(S),
	with_output_to(codes(Data),write_canonical(notify(Ref,Subject,Event))),
	udp_send(S,Data,Address, []),
	debug(pif_observe,Data,[]).

 	