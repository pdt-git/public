:- module(pdt_rewrite,[]).

:- multifile pdt_rewrite_goal/2.

rt_expand(A,B):-
	do_goal_expansion(A,B).


do_goal_expansion(A,(rt_expand(A,C),C)):-
	functor(A,':',2),arg(1,A,M),
	var(M),!.
  
do_goal_expansion(A,(rt_expand(A,C),C)):-
	functor(A,F,_),var(F),!.


do_goal_expansion(A,B):-
    pdt_rewrite_goal(A,B),!.
do_goal_expansion(A,A).    




user:goal_expansion(A,B):-
	do_goal_expansion(A,B).



pdt_proving(Goal):-
    (	watching_goal(Goal)
    ->	true
    ;	watch_goal(Goal)
    ),
    prove_key(Key),
    recorded(Key,Goal).
    
    

/*
memo:
what to do to avoid messing up lco/tro ?

simple. If the expanded goal is the last subgoal within the parent goal, we push as usual
before descending, but we do not insert a pop after ascending. (this would keep the original 
last call from beeing one, hence byebye tro - which is  bad, bad, bad)
instead, we mark the expanded goal as a last call. 

To eventualy clean the stack on ascending, any non-last-call goal has the obligation to pop any remaining last-call goals from the 
stack before popping itself. 
My, my, i am so clever :-) Only problem: how do i know if the expanded goal is a last call?
sure, if i see the whole clause body it's simple. i could use term_expansion/2...

a term is called
 - if it is arg to :-/1
 - if it is the second arg of :-/2
 - if it is a goal arg to any known meta call predicate (including ,/2 and ;/2)
 - if it is bound to a variable that satisfies the above conditions.

*/