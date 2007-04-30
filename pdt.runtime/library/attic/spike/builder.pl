:- module(pdt_builder,
	[	pdt_target_request/1, 
		pdt_target_invalidate/1
	]
).

/* hooks */
:- dynamic current_target_hook/1.
:- dynamic up_to_date_hook/1.
:- dynamic build_hook/1.
:- dynamic invalidate_hook/1.



pdt_target_request(T):-
    \+ current_target(T),
    !,
	throw(error(invalid_arguement,unknown_target(T))).
pdt_target_request(T):-
	copy_term(T,TT),
	up_to_date(TT),
	T==TT, %the up-to-date target is at least as general as T
	!.
pdt_target_request(T):-
    forall(
    	current_target(T),
	    target_request(T)
	).









pdt_target_request(T):-
    \+ current_target(T),
    !,
	throw(error(invalid_arguement,unknown_target(T))).
pdt_target_request(T):-
    forall(
    	current_target(T),
	    target_invalidate(T)
	).



target_request(T):-
    up_to_date(T),
    !.
target_request(T):-
	build(T).



target_invalidate(T):-
	up_to_date(T),    
	!,
	







build(T):-
    build_hook(T),
    !.
    
current_target(T):-
	current_target_hook(T).    
	
up_to_date(T):-
	up_to_date_hook(T).	