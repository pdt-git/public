	%defines the functionality formerly offered by the 
%java interface of the same name.
:-module(pdt_meta_info,[]).

/**
pdt_contains_clause(+File,-DefModule,-Name,-Arity,-aterm(Anot,Term):-
*/

pdt_contains_clause(File,DefModule,Name,Arity,aterm(Anns,Term)):-
    pdt_filespec(File,Abs),
	current_file_annotation(Abs,_,Terms),
	member(aterm(Anns,Term),Terms),
	pdt_member(clause_of(DefModule:Name/Arity),Anns).
  


pdt_find_predicate(Context, Name, Arity, DefModule, DefFile).

pdt_predicate_help(DefModule,Name,Arity,Summary,Detail).
    
pdt_predicate_reference(DefModule,Name,Arity, File, Term, Path).
	
	