%
% try to find meta-calls, etc through
% "abstract simulation" (let's patent this term :-) 
%
% This is a mix of several approaches. 
% I picked the part that I understood from each and through it all together.
%
% Here is the idea:
% 1) Think of transition systems. 
/*
State Space = Herbrand Universe 
Transition = Resolution steps

easy. Only problem: there may be infinite, acyclic paths. Hmm.
Abstraction brings the answer. 
- Since we assume that our program is finite,
an infinit path (cyclic or not) can only be induced by (unterminated) recursion.
(or infinite failure, we will see about this later, forget it for now)
- A common property of unterminated recursions is that at some point in the 
  recursion, two terms are executed as (sub-)goals that are equivalent, i.e. differ only in the
  naming of variables. (Otherwise the program text could not be finite)
- We exploit this by doing the simplest of all thinkable abstractions: identify (sub-)goals 
  that only differ in the naming of variables, and then identify states which represent the same
  conjunction of sub goals. 
- This earns us the following: There may still be infinite paths in our abstract TS, but only 
  through cycles. The reachable sub-TS should in fact be finite, if only the set of initial 
  states is finite. Cool!
- There are a couple of other simplifications: we only consider transitions resulting from
  left-to-right resolution.
*/


effects(Goal,FX):-
    effects_known(Goal,FX),
    !.
effects(Goal,FX):-
    mark_as_processed(Goal),
	calculate_effects(Goal,FX),
	remember_effects(Goal,FX).
	
calculate_effects((Goal1,Goal2),FX):-
    !,
    effects(Goal1,FX1),
    apply(Goal2,FX1,Goal2X),
    effects(Goal2X,FX2),
    concat_effects(FX1,FX2,FX).
    
calculate_effects(Head,FX):-
    clause_exists(Head),
    !,
	use_clause(Head,Body,FX1),
	effects(Body,FX2),
	concat_effects(FX1,FX2,FX3),
	filter_effects(FX3,Head,FX).
calculate_effects(Goal,FX):-
	effects_new(FX0),
	effects_add(FX0,dunno(Goal),FX).



done(Goal):-
    build_term(Goal,G),
	edge(G,ELabels,_),
	labels_head(ELabels,Head),
	term_equiv(Goal,Head).
    
/*	
--------------
g2:-writeln(2).
goals([g1,g2]).	

r([]).
r([T|Ts]):-
	call(T),
	r(Ts).	    
-------------
r(A):-
	A=[].
r(A):-
	A=[T|Ts],
	call(T),
	r(Ts).	    



	
r(A)--> A=[].
r(A)-->	A=[T|Ts],q(T),r(Ts).

r([1,2]) --> q(1),r([2]) --> q(1),q(2),r([]).

goals(G),r(G) --> G=[g1,g2],r([g1,g2]).
r([g1,g2]) --> call(g1),r([g2]).
call(g1) --> dunno(g1).
r([g2]) --> call(g2), r([]).
call(g2) -->g2.
g2 --> writeln(2)


------


r(A) --> A=[T|Ts],unbound(T),r(Ts).

goals(G),r(G) --> G=[g1,g2],dunno(g1), writeln(2).
goals(G) --> G=[g1,g2]
r([g1,g2]) --> dunno(g1), writeln(2).
call(g1) --> dunno(g1).
r([g2]) --> writeln(2).
call(g2) -->call_arg([1], call),writeln(2).

  
  */