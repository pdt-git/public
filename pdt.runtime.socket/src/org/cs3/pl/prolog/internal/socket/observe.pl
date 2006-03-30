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