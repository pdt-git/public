:- module(model,[write_node/1,
	write_tree/1,
	write_tree2/1,
	node/1,
	node/2,
	node_property/2,
	child/2]
	).
:- import(plparser:node_id(_)).	
:- import(plparser:node_attr(_,_)).
:- import(consult_server:starts_at(_,_)).

node(Id):-
    my_writeln('node(predicate_node(M:F/A))'),
    node_id(Id).
    
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
   	



node_property(Id,Prop):-
  	my_writeln('node_property(Id,Prop)'),
    node_attr(Id,Prop).
    
node_property(source_node(Num),is_source):-
    my_writeln('node_property(source_node(Num),is_source)'),
    node(source_node(Num)).
    
node_property(module_node(M),type(module)):-
    my_writeln('node_property(module_node(M),type(module))'),
    node(module_node(M)).

node_property(module_node(M),file(File)):-
    my_writeln('node_property(module_node(M),file(File))'),
    current_module(M,File).

node_property(module_node(M),predicate(predicate_node(M:_X))):-
    my_writeln('node_property(module_node(M),predicate(predicate_node(M:_X)))'),
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
    forall(node_property(Child,parent(Id)),write_tree(Child,ChildIndent,'|--')).    

write_tree2(Id):-
	write_tree(Id,'','').
	   	
write_tree2(Id,Indent,Arrow):-
    format("~a~a<<~w>>~n",[Indent,Arrow,Id]),
    atom_concat(Indent,'|    ',ChildIndent),
    forall(child(Id,Child),write_tree(Child,ChildIndent,'|--')).    


child(module_node(M),predicate_node(M:X)):-
    node(predicate_node(M:X)).
    
child(predicate_node(X),Y):-
	node_property(predicate_node(X),clause(Y)).    

my_writeln(Term):-
	true.
%    writeln(Term).

  