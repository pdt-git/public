/**
 * mgh(+Literal, ?MostGeneralLiteral)
 *
 * Arg2 is the most general form of the literal in Arg1
 */
mgh(Mod:Term, Mod:Term_mgh):-
    !,
    functor(Term, Functor, Arity),
    functor(Term_mgh, Functor, Arity).

mgh(Term, Term_mgh):-
    functor(Term, Functor, Arity),
    functor(Term_mgh, Functor, Arity).


/**
 * built_in(?Head)
 *
 * Check whether the specified term is the head of a built-in
 * predicate or enumerate all built-in predicates (if called 
 * with a free argument).
 */     
built_in(Head) :-
    predicate_property(Head, built_in).    
    
    
/**
 * repeat_n_times(+Goal,+N)
 */
repeat_n_times(Goal,N) :-             % initialize loop counter
  flag(repeat_counter,_,0),            
  repeat_n_times_loop(Goal,N).
  
repeat_n_times_loop(_,N) :-        % stop if counter = N
  flag(repeat_counter,N,N),
  !.
repeat_n_times_loop(Goal,N) :-        % loop
  once(Goal),                           % execute Goal once
  flag(repeat_counter,I,I+1),           % increment counter
  repeat_n_times_loop(Goal,N).          % repeat

   
:- module_transparent prolog_iteration_via_backtracking/1, all/1.

all(G) :- prolog_iteration_via_backtracking(G) .

prolog_iteration_via_backtracking(G) :- (call(G), fail) ; true .


/**
 * has_property(+Pred, ?Prop, ?HasProp) is det
 * 
 * Arg3 is 1 if the predicate referenced by Arg1 has the predicate of Arg2.
 * Else Arg3 is 0. 
 */
has_property(Pred,Prop,1) :- 
	predicate_property(Pred,Prop),
	!.
has_property(_Pred,_Prop,0).
