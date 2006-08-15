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

:- module(pif_observe,[
	pif_observe/3,
	pif_observe/2,	
	pif_unobserve/2,
	pif_notify/2,
	pif_dispatch/3
]).

:-dynamic pif_observe_hook/3,pif_unobserve_hook/3.
:-multifile pif_observe_hook/3,pif_unobserve_hook/3.

% pif_observe_hook(Thread,Subject,Key),
% A hook predicate that will be called each time an observer registers to 
% a subject, if it is not already registered with this subject.
% Clients that provide observable subjects can add clauses to these predicates
% if they need to take additional actions on registration of an observer
% to a subject. If the call succeeds, it is assumes that these additional actions have 
% been taken.
% 
pif_observe_hook(_,_,_):-
	fail.	     	

% pif_unobserve_hook(Thread,Subject,Key),
% A hook predicate that will be called each time an observer unregisters to 
% a subject. see pif_observe_hook/3
% 
pif_unobserve_hook(_,_,_):-
	fail.	     	

call_observe_hook(Thread,Subject,Key):-
	catch(pif_observe_hook(Thread,Subject,Key),E,print_message(error,E)),
	!.  
call_observe_hook(_,_,_).
call_unobserve_hook(Thread,Subject,Key):-
	catch(pif_unobserve_hook(Thread,Subject,Key),E,print_message(error,E)),
	!.  		
call_unobserve_hook(_,_,_).	


/**
 * backwards compatibility. 
 */
pif_observe(Thread,Subject):-
   term_to_atom(Subject,Key),
   pif_observe(Thread,Subject,Key). 
%% pif_observe(+Thread,+Subject).
%  Add an observer to a subject.
% 
%  
% 
%  @param Thread the observer, i.e. a thread that is running dispatch/3
%  @param Subject the subject to observe. 
% 		This term is unified with the subject given as second argument to notify/2.
%  @param Key should be an atom. During notification, if the Subject terms was successfully unified,
% 		the key is also passed to the observer. The idea of this is to help observers calling from
% 		Java, or otherwise lacking the concept of unification, to recognize the Subject they subscribed 
% 		for. 
% 
pif_observe(Thread,Subject,Key) :-
  recorded(pif_observer,observation(Thread,OtherSubject,Key)),
  OtherSubject =@= Subject,
  !.

pif_observe(Thread,Subject,Key) :-
%  sync:init_idb(Subject),
  call_observe_hook(Thread,Subject,Key),
  recordz(pif_observer,observation(Thread,Subject,Key)).



%%  pif_unobserve(+Thread,+Subject) 
%  Remove an observer from a subject.
% 
%  @param Thread the observer thread to remove.
%  @param Subject the subject from which to remove the observer.
% 

pif_unobserve(Thread,Subject) :-
  recorded(pif_observer,observation(Thread,OtherSubject,Key),Ref),
  OtherSubject =@= Subject,
  erase(Ref),
  %sync:unregister_observer(Subject). 
  call_unobserve_hook(Thread,Subject,Key).

%% pif_notify(+Subject,+Event)
% Notify all active observers.
% If observer's thread is stopped
% it will be removed.
% 
pif_notify(Subject,Event) :-
   forall(
    	( 
    	  recorded(pif_observer,observation(Thread,Subject,_),Ref)
    	),
    	(	current_thread(Thread,running)
    	->	(    	      
    	      thread_send_message(Thread,notify(Subject,Event)),
   	      writeln(thread_send_message(Thread,notify(Subject,Event)))
    	   
    	);	erase(Ref)
    	)
    ).   

%% pif_dispatch(-Subject,-Key,-Event)
% Recieve events.
% This predicate is intended to be called by observer threads. 
% It produces solutions for every recieved event, i.e. every time
% pif_notify/2 is called on a subject the observer thread is subscribed for.
% If it recieves an event for the subject '$stop' it will cut and fail.

 
 pif_dispatch(Subject,Key,Event):-
     	thread_self(Me),
     	repeat,
	     	thread_get_message(notify(Subject,Event)),
	     	(	Subject='$abort'
	     	->	!
	     	;	recorded(pif_observer,observation(Me,Subject,Key))
	     	).
	     	
	     	
