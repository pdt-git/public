:- module(mi_built_ins,[mi_call_built_in/1]).
:- use_module(meta_interpreter).
:- use_module(mi_meta_ops).

mi_call_built_in((','(A,B))):-
 	!,
    mi_call(A),
    mi_apply_subst(B,Bin),
    mi_call(Bin).

mi_call_built_in(('='(A,B))):-
 	!,
    mi_unify(A,B).

mi_call_built_in(true):-
    !.

mi_call_built_in(fail):-
    !,
	fail.
	
mi_call_built_in(InHead):-
	mi_call_undefined_built_in(InHead).
    