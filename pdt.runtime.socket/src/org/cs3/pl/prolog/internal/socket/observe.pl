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

:- module(observe,[
	observe/3,
	observe/2,	
	unobserve/2,
	notify/2,
	dispatch/3
]).



/**
 * backwards compatibility. 
 */
observe(Thread,Subject):-
   term_to_atom(Subject,Key),
   observe(Thread,Subject,Key). 
/**
 * observe(+Thread,+Subject)
 *
 * @arg Thread the Thread that is running dispatch/3
 * @arg Subject the subject to observe. 
 *		This term is unified with the subject given as second argument to notify/2.
 * @arg Key should be an atom. During notification, if the Subject terms was successfully unified,
 *		the key is also passed to the observer. The idea of this is to help observers calling from
 *		Java, or otherwise lacking the concept of unification, to recognize the Subject they subscribed 
 *		for. 
 */
observe(Thread,Subject,Key) :-
%  atom_to_term(AtomSubject, Subject,_),
	writeln(observer_1),
  recorded(observer,observation(Thread,OtherSubject,Key)),
  OtherSubject =@= Subject,
  	writeln(already_recorded(recorded(observer,observation(Thread,OtherSubject,Key)))),
  !.

observe(Thread,Subject,Key) :-
%  atom_to_term(AtomSubject, Subject,_),
	writeln(observer_2),
  sync:init_idb(Subject),
  writeln(recordz(observer,observation(Thread,Subject,Key))),
  recordz(observer,observation(Thread,Subject,Key)).


/**
 * unobserve(+Thread,+Subject) 
 *
 * @arg Thread associated thread
 */

unobserve(Thread,Subject) :-
%  atom_to_term(AtomSubject, Subject,_),
  recorded(observer,observation(Thread,OtherSubject,_),Ref),
  OtherSubject =@= Subject,
  erase(Ref),
  sync:unregister_observer(Subject). 

/**
 * notify(+Subject,+Event)
 *
 * Notify all active observers.
 * If observer's thread is stopped
 * it will be removed.
 */
notify(Subject,Event) :-
   writeln(notify(Subject,Event)),	
   forall(
    	( 
    	  recorded(observer,observation(Thread,ObservedSubject,_),Ref),
%    	  atom_to_term(ObservedSubject,Term,_),
		writeln(testing(ObservedSubject =@= Subject)),
    	  ObservedSubject =@= Subject
    	),
    	(	current_thread(Thread,running)
    	->	(    	      
    	      thread_send_message(Thread,notify(ObservedSubject,Event)),
   	      writeln(thread_send_message(Thread,notify(ObservedSubject,Event)))
    	   
    	);	erase(Ref)
    	)
    ).    

/*
 this predicate is intended to be called by the PrologEventDispatcher class.
 It produces solutions for every recieved event.
 If it recieves an event for the subject '$stop' it will cut and fail.
 */
 
 dispatch(Subject,Key,Event):-
     	repeat,
	     	thread_get_message(notify(Subject,Event)),
	     	(	Subject='$abort'
	     	->	!
	     	;	recorded(observer,observation(_,Subject,Key))
	     	).