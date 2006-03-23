/**
some predicate definitions for queries frequently used by the pdt.core 
*/

:-module(pdt_meta_info,[
	pdt_file_contains_clause/5,
	pdt_file_includes/2,
	pdt_file_depends/2
]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).


pdt_file_contains_clause(File,DefModule,Name,Arity,aterm(Anns,Term)):-
    pdt_file_spec(File,Abs),
	current_file_annotation(Abs,_,Terms),
	member(aterm(Anns,Term),Terms),
	pdt_member(clause_of(DefModule:Name/Arity),Anns).
  

pdt_file_includes(FileSpec,IncludeSpec):-
    pdt_file_spec(FileSpec,Abs),
    current_file_annotation(Abs,Terms,_),
    member(references_files(Refs),Terms),
    member(IncludeSpec,Refs).
    

pdt_file_depends(File,File).
pdt_file_depends(DependentFile,DependencyFile):-
	pdt_file_includes(DependentFile,IncludedFile),
	pdt_file_depends(IncludedFile,DependencyFile).
    
	
	