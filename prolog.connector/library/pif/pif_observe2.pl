/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

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

 	


