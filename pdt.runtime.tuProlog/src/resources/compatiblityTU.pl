plus(A,B,C) :-
  nonvar(A),
  nonvar(B),
  !,
  C is A + B.
  
plus(A,B,C) :-
	nonvar(A),
	nonvar(C),
	  !,
  B is C - A.

  
plus(A,B,C) :-
	nonvar(B),
	nonvar(C),
	  !,
  A is C - B.


/**
 * TuProlog does not support modules.
 * 
 * We ignore the module directive 
 * and store module name and exported predicates
 * in the dynamic fact consulted_modules.
 */
:- dynamic consulted_modules/2.

module(Name,ExportedPredicates):-
	consulted_modules(Name,ExportedPredicates).
	
/**
 * TuProlog does not support modules.
 * 
 * We forward use_module to consult.
 */
use_module(Name):-
    consult(Name).

