:- module(meta_interpreter,[
	mi_call/1,
	mi_call_undefined/1,
	mi_call_variable/1,
	mi_call_undefined_built_in/1
]).
:- use_module(mi_meta_ops).
:- use_module(mi_built_ins).


% sim_call(Head)
%
% called by the meta interpreter when a goal is encountered.
% Should succeed if InHead is provable.
% on Success, should unify OutHead with a *COPY* of InHead that represents the instantiation 
% state of InHead after successfully proving it.
% Should *NOT* alter the instantiation state of InHead.
mi_call(Head):-
    mi_apply_subst(Head,SubstHead),
    (	var(SubstHead)
    ->	mi_call_variable(Head)
    ;	%format("subgraph \"cluster~w\" {~n label=\"~w\"~n",[SubstHead,SubstHead]),
    	writeln(enter(Head)),
    	(   do_mi_call(Head),
    		writeln(proven(Head))
	    	%format("} ~n",[])
	    ;	writeln(failed(Head)),
	    	%format("} ~n",[]),
	    	fail
    	)
    ).
    

    

do_mi_call(Head):-
    predicate_property(Head,built_in),!,
    mi_call_built_in(Head).
do_mi_call(Head):-
    functor(Head,Fun,Ar),
    (	current_predicate(Fun/Ar)
    ->	do_mi_clause(Head)
    ;	mi_call_undefined(Head)        
    ).
    
do_mi_clause(Head):-
    copy_term(Head,DupHead),
	clause(DupHead,Body),
	mi_unify(Head,DupHead),
	mi_apply_subst(Body,SubstBody),
	mi_call(SubstBody).

% mi_call_undefined(+Head)
% called if a Goal is called that cannot be bound to any
% predicate. 
mi_call_undefined(Head):-
%	format("subgraph \"cluster~w\" {label=\"undefined(~w)\"}~n",[Head,Head]).
	writeln(undefined(Head)).

% mi_call_undefined_built_in(+Head)
% called if a Goal is a builtin, but no semantic is defined
% in the meta interpreter
mi_call_undefined_built_in(Head):-
%	format("subgraph \"cluster~w\" {label=\"undefined_built_in(~w)\"}~n",[Head,Head]).
	writeln(undefined_built_in(Head)).

    
% mi_call_variable(+Head)
% called if a Goal is called but the called term is an
% unbound variable.
mi_call_variable(Head):-
%	format("subgraph \"cluster~w\" {label=\"variable(~w)\"}~n",[Head,Head]).
	writeln(variable(Head)).



some_goal(goal).
some_goal(no_goal).
    
notest(no_goal,Text):-
	writeln(Text).
notest(goal,Text):-
	Text.		
	
notest(A):-
    some_goal(IsGoal),
    notest(IsGoal,A).