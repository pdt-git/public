:-module(runtime,
	[predicates_defined_in/2,
	 predicates_defined_in/5,
	 predicates_defined_in/4,
	 clauses_defined_in/2,
	 clause_ref/1,
	 clause_of/4,
	 clause_of/3,
	 term_type/2]).



predicates_defined_in(M, Head) :-
	current_predicate(_, M:Head),
	\+ predicate_property(M:Head, imported_from(_)).


predicates_defined_in(M,Head, Name, Arity) :-
	(	nonvar(Name),
		nonvar(Arity)
	-> 	functor(Head,Name,Arity),
	    predicates_defined_in(M,Head)
	;	predicates_defined_in(M,Head),
		functor(Head,Name,Arity)
	).

    
predicates_defined_in(M, Head, Name, Arity, Exported) :-
    predicates_defined_in(M,Head, Name,Arity),
	(	predicate_property(M:Head,exported)
	->	Exported=true
	;	Exported=false
	).


clause_ref(Ref):-
    (	nonvar(Ref)
    ->	catch(nth_clause(_,_,Ref),_,fail)
    ;	current_predicate(_,_:Pred),
		nth_clause(Pred,_,Ref)
	).

clause_of(Module,Name,Arity,Index):-
    (	ground(Name/Arity)
    ->	functor(Head,Name,Arity),
    	clause_of(Module,Head,Index)
    ;	clause_of(Module,Head,Index),
    	functor(Head,Name,Arity)
    ).

clause_of(Module,Head,Index):-
    predicates_defined_in(Module,Head),
    Module:nth_clause(Head,Index,_).
    
clauses_defined_in(File,ClauseRef):-
	current_predicate(_,_:Pred),
	nth_clause(Pred,_,ClauseRef),
	clause_property(ClauseRef,file(File)).	

term_type(Term, var):-
    var(Term),!.
	
term_type(Term, string):-
	string(Term),!.	

term_type(Term, integer):-
	integer(Term),!.    

term_type(Term, float):-
	float(Term),!.    

term_type(Term, number):-
	number(Term),!.    

term_type(Term, atom):-
    atom(Term),!.

term_type(Term, list):-
	is_list(Term),!.

term_type(Term, compound):-
    %\+ is_list(Term), eigentlich sind listen auch compounds. (ld)
    compound(Term),!.
    