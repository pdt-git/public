:-module(runtime,[runtime_node/1, runtime_node_property/2]).

model:do_node(Node):-
    runtime:runtime_node(Node).

model:do_node_property(Node,Prop):-
    runtime:runtime_node_property(Node,Prop).    


runtime_node(predicate_node(M:F/A)):-
	predicates_defined_in(M,_,F,A).
runtime_node(module_node(M)):-
	current_module(M).
runtime_node(clause_node(M:F/A,I)):-
    clause_of(M,F,A,I).
runtime_node_property(module_node(M),type(module)):-
    runtime_node(module_node(M)).
runtime_node_property(predicate_node(P),type(predicate)):-
    runtime_node(predicate_node(P)).
runtime_node_property(clause_node(M),type(clause)):-
    runtime_node(clause_node(M)).
runtime_node_property(module_node(M),is_runtime):-
    runtime_node(module_node(M)).
runtime_node_property(predicate_node(P),is_runtime):-
    runtime_node(predicate_node(P)).
runtime_node_property(clause_node(M),is_runtime):-
    runtime_node(clause_node(M)).
runtime_node_property(module_node(M),type(module)):-
    runtime_node(module_node(M)).
runtime_node_property(module_node(M),file(File)):-
    current_module(M,File).
runtime_node_property(module_node(M),predicate(predicate_node(M:_X))):-
    runtime_node(predicate_node(M:_X)).
runtime_node_property(predicate_node(T),type(predicate)):-
    runtime_node(predicate_node(T)).
runtime_node_property(predicate_node(M:_X),parent(module_node(M)))    :-
    runtime_node(predicate_node(M:_X)).
runtime_node_property(predicate_node(M:F/A),name(F)):-
	runtime_node(predicate_node(M:F/A)).
runtime_node_property(predicate_node(M:F/A),arity(A)):-
	runtime_node(predicate_node(M:F/A)).    
runtime_node_property(predicate_node(M:F/A),term(Term)):-
	runtime_node(predicate_node(M:F/A)),
	functor(Term,F,A).
runtime_node_property(predicate_node(M:F/A),clause(clause_node(M:F/A,I))):-
    runtime_node(predicate_node(M:F/A)),
	functor(Term,F,A),
	M:nth_clause(Term,I,_).
runtime_node_property(predicate_node(M:F/A),real_line_count(RealLine)):-
    runtime_node(predicate_node(M:F/A)),
	functor(Term,F,A),
	M:predicate_property(Term,line_count(Line)),
	M:predicate_property(Term,file(File)),
	starts_at(File,Offset),
	RealLine is Line - Offset + 1.
runtime_node_property(predicate_node(M:F/A),Prop):-
    runtime_node(predicate_node(M:F/A)),
	functor(Term,F,A),
	M:predicate_property(Term,Prop).
runtime_node_property(clause_node(M:F/A,I),type(clause)):-
    runtime_node(clause_node(M:F/A,I)).
runtime_node_property(clause_node(M:F/A,I),clause_ref(Ref)):-
    runtime_node(clause_node(M:F/A,I)),
    functor(Term,F,A),
	M:nth_clause(Term,I,Ref).    
runtime_node_property(clause_node(M:F/A,I),parent(predicate_node(M:F/A))):-
    runtime_node(clause_node(M:F/A,I)).
runtime_node_property(clause_node(M:F/A,I),clause_number(I)):-
	runtime_node(clause_node(M:F/A,I)).
runtime_node_property(clause_node(M:F/A,I),head_term(Head)):-
    runtime_node(clause_node(M:F/A,I)),
    functor(P,F,A),
    M:nth_clause(P,I,Ref),
    M:clause(Head,_,Ref).    
runtime_node_property(clause_node(M:F/A,I),body_term(Body)):-
    runtime_node(clause_node(M:F/A,I)),
    functor(P,F,A),
    M:nth_clause(P,I,Ref),
    M:clause(_,Body,Ref).
runtime_node_property(clause_node(M:F/A,I),real_line_count(RealLine)):-
    runtime_node(clause_node(M:F/A,I)),
    functor(Head,F,A),
    M:nth_clause(Head,I,Ref),
	clause_property(Ref,line_count(Line)),
	clause_property(Ref,file(File)),
	starts_at(File,Offset),
	RealLine is Line - Offset + 1.
runtime_node_property(clause_node(M:F/A,I),Prop):-
    runtime_node(clause_node(M:F/A,I)),
    functor(Head,F,A),
    M:nth_clause(Head,I,Ref),
	clause_property(Ref,Prop).



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
    