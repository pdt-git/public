:- module(karl,
[achmann/1,
 props/1,
 props/0,
 native/1,
 native/0]
).

:-module_transparent props/0.
:-module_transparent props/1.
:-module_transparent native/1.
:-module_transparent native/0.
achmann(o).
    
nase(aua).

props(_Pred):-
    writeln(_Pred),
    forall(
    	predicate_property(_Pred,_Prop),
    	format("-->~w~n",[_Prop])
    ),
    nl.

props:-
    forall(
    	current_predicate(_,_Pred),
    	props(_Pred)
    ).

native:-
    forall(
    	native(_Pred),
    	(
    		functor(_Pred,Name,Ar),
	    	format("~a/~a~n",[Name,Ar])
	    )
    ),
    nl.
    
native(Pred):-
    current_predicate(_,Pred),
    not(
    	predicate_property(Pred,imported_from(_))
    ).
