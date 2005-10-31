:- module(treewriter,[write_tree/2,write_tree/1]).
:- use_module(plast).
:- use_module(plast_utils).

write_tree(Node):-
    write_tree(current_output,Node).
    
write_tree(Stream,compilation_unit_node(I)):-
	forall(
		plast_prop(compilation_unit_node(I),child(J)),    
		(	write_tree(Stream,J),
			put(Stream,'.'),
			nl(Stream)
		)
	).
	
write_tree(Stream,atom_node(I)):-
	plast_prop(atom_node(I),term(T)),
	write(Stream,T).

write_tree(Stream,variable_node(I)):-
	plast_prop(variable_node(I),name(T)),
	write(Stream,T).
	
	
write_tree(Stream,string_node(I)):-
	plast_prop(string_node(I),term(T)),
	write(Stream,T).	
	
write_tree(Stream,compound_node(I)):-
	plast_prop(compound_node(I),functor(N/_)),	
	plast_prop(compound_node(I),arguments(Args)),
	write(Stream,N),
	put(Stream,'('),
	write_sequence(Stream,Args),
	put(Stream,')').
	
write_tree(Stream,list_node(I)):-
    put(Stream,'['),
	(	plast_prop(list_node(I),elements(Elms))
	->	write_sequence(Stream,Elms),
		(	plast_prop(list_node(I),tail(Tail))
		->	put(Stream,'|'),
			write_tree(Stream,Tail)
		;	true
		)
	;	true
	),
	put(Stream,']').
	
write_tree(Stream,brace_node(I)):-
	put(Stream,'{'),
	plast_prop(brace_node(I),argument(Arg)),
	write_tree(Stream,Arg),
	put(Stream,'}').
	
write_sequence(_,[]).
write_sequence(Stream,[Last|[]]):-
	write_tree(Stream,Last).
write_sequence(Stream,[Head|Tail]):-
	write_tree(Stream,Head),
	put(Stream,','),
	write_sequence(Stream,Tail).		