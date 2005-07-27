:- module(model,[write_node/1,
	write_tree/1,
	delete_tree/1,
	node/1,
	node/2,
	node_property/2,
	parse_property/2,
	parse_property_list/2,
	child/2,
	is_source/1,
	is_runtime/1]
	).
:- import(plparser:node_id(_)).	
:- import(plparser:node_attr(_,_)).
:- import(consult_server:starts_at(_,_)).

parse_property_list(In,Out):-
    maplist(parse_property,In,Out).

parse_property(In,Out):-
	In=..[Functor|Args],
	parse_property(Functor,Args,Out).
	
parse_property(Functor,[],Functor->true).

parse_property(Functor,[Arg|[]],Functor->Arg).		    

is_runtime(N):-
    node_property(N,is_runtime).
is_source(A):-
    node_property(A,is_source).


node(source_folder_node(N)):-
    node_id(source_folder_node(N)).
node(compilation_unit_node(N)):-
    node_id(compilation_unit_node(N)).
node(atom_node(N)):-
    node_id(atom_node(N)).
node(variable_node(N)):-
    node_id(variable_node(N)).
node(string_node(N)):-
    node_id(string_node(N)).
node(brace_node(N)):-
    node_id(brace_node(N)).
node(list_node(N)):-
    node_id(list_node(N)).
node(compound_node(N)):-
    node_id(compound_node(N)).                     
node(predicate_node(M:F/A)):-
    my_writeln('node(predicate_node(M:F/A))'),    
	predicates_defined_in(M,_,F,A).
node(module_node(M)):-
    my_writeln('node(module_node(M))'),    
	current_module(M).
node(clause_node(M:F/A,I)):-
    my_writeln('node(clause_node(M:F/A,I))'),    
    clause_of(M,F,A,I).
	
node(Id,List):-
   	my_writeln('node(Id,List)'),    
    var(List),
    !,
    setof(Att,node_property(Id,Att),List).

node(Id,[]):-
   	my_writeln('node(Id,[])'),
    node(Id).

    
node(Id,[HeadAttr|TailAttr]):-
  	my_writeln('node(Id,[HeadAttr|TailAttr])'),
    nonvar(HeadAttr),
    nonvar(TailAttr),
    !,
	my_writeln('uuuund...'),
    node_property(Id,HeadAttr),
   	node(Id,TailAttr).
   	

node_property(module_node(M),type(module)):-
    node(module_node(M)).
node_property(predicate_node(P),type(predicate)):-
    node(predicate_node(P)).
node_property(clause_node(M),type(clause)):-
    node(clause_node(M)).

node_property(module_node(M),is_runtime):-
    node(module_node(M)).
node_property(predicate_node(P),is_runtime):-
    node(predicate_node(P)).
node_property(clause_node(M),is_runtime):-
    node(clause_node(M)).


    
node_property(source_folder_node(Num),type(source_folder)):-
    node(source_folder_node(Num)).
node_property(compilation_unit_node(Num),type(compilation_unit)):-
    node(compilation_unit_node(Num)).
node_property(atom_node(Num),type(atom)):-
    node(atomt_node(Num)).
node_property(variable_node(Num),type(variable)):-
    node(variable_node(Num)).
node_property(string_node(Num),type(string)):-
    node(string_node(Num)).
node_property(brace_node(Num),type(brace)):-
    node(brace_node(Num)).
node_property(list_node(Num),type(list)):-
    node(list_node(Num)).
node_property(compound_node(Num),type(compound)):-
    node(compound_node(Num)).
    
node_property(source_folder_node(Num),is_source):-
    node(source_folder_node(Num)).
node_property(compilation_unit_node(Num),is_source):-
    node(compilation_unit_node(Num)).
node_property(atom_node(Num),is_source):-
    node(atomt_node(Num)).
node_property(variable_node(Num),is_source):-
    node(variable_node(Num)).
node_property(string_node(Num),is_source):-
    node(string_node(Num)).
node_property(brace_node(Num),is_source):-
    node(brace_node(Num)).
node_property(list_node(Num),is_source):-
    node(list_node(Num)).
node_property(compound_node(Num),is_source):-
    node(compound_node(Num)).
node_property(Id,Prop):-%%applies to all source nodes.
    node_attr(Id,Prop).    

node_property(module_node(M),type(module)):-
    node(module_node(M)).
node_property(module_node(M),file(File)):-
    current_module(M,File).
node_property(module_node(M),predicate(predicate_node(M:_X))):-
    node(predicate_node(M:_X)).

node_property(predicate_node(T),type(predicate)):-
    my_writeln('node_property(predicate_node(T),type(predicate))'),
    node(predicate_node(T)).
node_property(predicate_node(M:_X),parent(module_node(M)))    :-
    my_writeln('node_property(predicate_node(M:_X),parent(module_node(M)))'),
    node(predicate_node(M:_X)).
node_property(predicate_node(M:F/A),name(F)):-
    my_writeln('node_property(predicate_node(M:F/A),name(F))'),
	node(predicate_node(M:F/A)).
node_property(predicate_node(M:F/A),arity(A)):-
    my_writeln('node_property(predicate_node(M:F/A),arity(A))'),
	node(predicate_node(M:F/A)).    
node_property(predicate_node(M:F/A),term(Term)):-
    my_writeln('node_property(predicate_node(M:F/A),term(Term))'),
	node(predicate_node(M:F/A)),
	functor(Term,F,A).
node_property(predicate_node(M:F/A),clause(clause_node(M:F/A,I))):-
    my_writeln('node_property(predicate_node(M:F/A),clause(clause_node(M:F/A,I)))'),
    node(predicate_node(M:F/A)),
	functor(Term,F,A),
	M:nth_clause(Term,I,_).
node_property(predicate_node(M:F/A),real_line_count(RealLine)):-
    my_writeln('predicate_property(clause_node(M:F/A),real_line_count(Line))'),
    node(predicate_node(M:F/A)),
	functor(Term,F,A),
	M:predicate_property(Term,line_count(Line)),
	M:predicate_property(Term,file(File)),
	starts_at(File,Offset),
	RealLine is Line - Offset + 1.
node_property(predicate_node(M:F/A),Prop):-
    my_writeln('node_property(predicate_node(M:F/A),Prop)'),
    node(predicate_node(M:F/A)),
	functor(Term,F,A),
	M:predicate_property(Term,Prop).

		

node_property(clause_node(M:F/A,I),type(clause)):-
    my_writeln('node_property(clause_node(M:F/A,I),type(clause))'),
    node(clause_node(M:F/A,I)).
node_property(clause_node(M:F/A,I),clause_ref(Ref)):-
    my_writeln('node_property(clause_node(Ref),clause_ref(M:F/A,I))'),
    node(clause_node(M:F/A,I)),
    functor(Term,F,A),
	M:nth_clause(Term,I,Ref).    
node_property(clause_node(M:F/A,I),parent(predicate_node(M:F/A))):-
    my_writeln('node_property(clause_node(M:F/A,I),parent(predicate_node(M:F/A)))'),
    node(clause_node(M:F/A,I)).
node_property(clause_node(M:F/A,I),clause_number(I)):-
    my_writeln('node_property(clause_node(M:F/A,I),clause_number(I))'),
	node(clause_node(M:F/A,I)).
node_property(clause_node(M:F/A,I),head_term(Head)):-
    my_writeln('node_property(clause_node(M:F/A,I),head_term(Term))'),
    node(clause_node(M:F/A,I)),
    functor(P,F,A),
    M:nth_clause(P,I,Ref),
    M:clause(Head,_,Ref).    
node_property(clause_node(M:F/A,I),body_term(Body)):-
    my_writeln('node_property(clause_node(M:F/A,I),body_term(Body))'),
    node(clause_node(M:F/A,I)),
    functor(P,F,A),
    M:nth_clause(P,I,Ref),
    M:clause(_,Body,Ref).
node_property(clause_node(M:F/A,I),real_line_count(RealLine)):-
    my_writeln('node_property(clause_node(M:F/A,I),real_line_count(Line))'),
    node(clause_node(M:F/A,I)),
    functor(Head,F,A),
    M:nth_clause(Head,I,Ref),
	clause_property(Ref,line_count(Line)),
	clause_property(Ref,file(File)),
	starts_at(File,Offset),
	RealLine is Line - Offset + 1.
node_property(clause_node(M:F/A,I),Prop):-
    my_writeln('node_property(clause_node(M:F/A,I),Prop)'),
    node(clause_node(M:F/A,I)),
    functor(Head,F,A),
    M:nth_clause(Head,I,Ref),
	clause_property(Ref,Prop).
    
    
write_node(Id):-
    format("<<~w>>~n",Id),
    forall(node_property(Id,P),format(" --> ~w~n",[P])).
   	

write_tree(Id):-
	write_tree(Id,'','').
	   	
write_tree(Id,Indent,Arrow):-
    format("~a~a<<~w>>~n",[Indent,Arrow,Id]),
    atom_concat(Indent,'|    ',ChildIndent),
    forall(child(Id,Child),write_tree(Child,ChildIndent,'|--')).    

delete_tree(Node):-
    is_source(Node),
    delete_tree_rec(Node).

delete_tree_rec(Node):-
	forall(child(Node,Child),delete_tree_rec(Child)),
	retractall(node_attr(Node,_)),
	retractall(node_id(Node)).
			
    

child(module_node(M),predicate_node(M:X)):-
    node(predicate_node(M:X)).
    
child(predicate_node(X),Y):-
	node_property(predicate_node(X),clause(Y)).    

child(source_folder_node(P),compilation_unit_node(C)):-
    node_attr(source_folder_node(P),compilation_unit(compilation_unit_node(C))).


child(compilation_unit_node(P),Clause):-
    node_attr(compilation_unit_node(P),clause(Clause)).

child(atom_node(_),_):-
	!,
	fail.

child(variable_node(_),_):-
	!,
	fail.

child(string_node(_),_):-
	!,
	fail.
	
child(brace_node(P),Arg):-
	node_attr(brace_node(P),argument(Arg)),!.

child(list_node(P),Elm):-
    node_attr(list_node(P),elements(Elms)),
    member(Elm,Elms).
child(list_node(P),Tail):-
    node_attr(list_node(P),tail(Tail)),!.
    
child(compound_node(P),Arg):-
    node_attr(compound_node(P),arguments(Args)),
    !,
    member(Arg,Args).


	
my_writeln(_):-
	true.
%    writeln(Term).



  