:- module(rename_file,[]).
:- use_module(library('facade/pdt_workspace')).
:- use_module(library('facade/pdt_delta')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('pef/pef_base')).
:- use_module(library('util/ast_transform')).
:- use_module(library('util/ast_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/parse')).
:- use_module(library('builder/targets/ast')).

:- use_module(transform).



transform:check_conditions(rename_file(OldPath,NewPath),Severity,Message):-
    pdt_with_targets(
    	[parse(workspace)],
    	do_check(OldPath,NewPath,Severity,Message)
    ).
    
transform:perform_change(rename_file(OldPath,NewPath)):-
	pdt_with_targets(
    	[parse(workspace)],
    	do_it(OldPath,NewPath)
    ).


do_check(OldPath,NewPath,warning, "Old and new path are equivalent."):-
    OldPath == NewPath,
    !.
do_check(_,NewPath,fatal, "File exists in filesystem."):-    
    exists_file(NewPath),
    !.
do_check(_,NewPath,fatal, "A conflicting directory entry exists in the filesystem."):-
    exists_directory(NewPath),
    !.    
do_check(_,NewPath,warning, "The specified path is outside the projects source path."):-
    \+ pdt_relative_path(NewPath,_,_),
    !.    
do_check(_,NewPath,error,Message):-
    pdt_relative_path(NewPath,Alias,RelPath),
    pdt_relative_path(ExistingPath,Alias,RelPath),
    pef_file_query([path=ExistingPath]),
    format(string(Message), "Conflicting file in search path: ~a",[ExistingPath]).
    
	
do_it(OldPath,NewPath):-
    get_pef_file(OldPath,File),
    pef_file_retractall([id=File]),
    pef_file_assert([id=File,path=NewPath]),
    mark_file_renamed(File,OldPath),
    forall(
    	pef_file_dependency_query([dependency=File,toplevel=Toplevel]),
    	pdt_with_targets([],update_dependency(Toplevel,File,OldPath))
    ).
    
    
update_dependency(Toplevel,File,OldPath):-
	pef_toplevel_query([id=Toplevel, file=ReferingFile]),
	get_pef_file(ReferingPath,ReferingFile),
	pdt_request_target(ast(file(ReferingPath))),
	generate_file_reference(File,ReferingFile,NewReference),
	find_file_reference(Toplevel,RefTerm,Reference),
	resolve_reference(RefTerm,ReferingFile,OldPath),
	!,
	ast_replace(Reference,NewReference),
	pef_property_assert([pef=NewReference,key=copy,value=true]),	
	mark_toplevel_modified(Toplevel).

generate_file_reference(File,ReferingFile,NewReference):-
	get_pef_file(Path,File),
	get_pef_file(ReferingPath,ReferingFile),
	file_directory_name(ReferingPath,ContainerPath),
	atom_concat(ContainerPath,'/',Prefix),
	(	atom_concat(Prefix,RelativePath,Path)
	->  generate_plain_file_reference(RelativePath,NewReference)
	;	pdt_relative_path(Path,Alias,RelativePath)
	->	generate_alias_file_reference(Alias,RelativePath,NewReference)
	;	generate_plain_file_reference(Path,NewReference)
	).
	
generate_plain_file_reference(RelativePath,NewReference):-
    pef_reserve_id(pef_term,NewReference),
    pef_term_assert([id=NewReference,arity=0,name=RelativePath]).
    
generate_alias_file_reference(Alias,RelativePath,NewReference):-
	pef_reserve_id(pef_term,NewReference),
    pef_term_assert([id=NewReference,arity=1,name=Alias]),
    generate_plain_file_reference(RelativePath,Child),
    pef_arg_assert([parent=NewReference,num=1,child=Child]).


find_file_reference(Toplevel,ReferenceTerm,ReferenceNode):-
	ast_root(Toplevel,Root),
	file_ref_pattern(Pattern,Kind,Refs),
	ast_simple_match(Pattern,Root),
	ast_functor(Refs,Name,_),
	(	Name==[]
	->	fail
	;	Kind==list
	->  ast_member(Ref,Refs)
	;	Kind==nolist,
		Ref=Refs
	),
	ast_apply(Ref,_,ReferenceTerm),
	ast_node(Ref,ReferenceNode).
 



file_ref_pattern((:-R),list,R).
file_ref_pattern((:-load_files(R,_)),nolist,R).
file_ref_pattern((:-load_files(R,_)),list,R).
file_ref_pattern((:-consult(R)),list,R).
file_ref_pattern((:-consult(R)),nolist,R).
file_ref_pattern((:-ensure_loaded(R)),list,R).
file_ref_pattern((:-ensure_loaded(R)),nolist,R).
file_ref_pattern((:-use_module(R)),list,R).
file_ref_pattern((:-use_module(R)),nolist,R).
file_ref_pattern((:-use_module(R,_)),list,R).
file_ref_pattern((:-use_module(R,_)),nolist,R).


resolve_reference(RefTerm,ReferingFile,Path):-
	get_pef_file(ReferingPath,ReferingFile),
	file_directory_name(ReferingPath,Dir),
	pdt_file_spec(RefTerm,Dir,Path).
	
ast_member(Ref,Refs):-
    ast_functor(Refs,(.),2),
    ast_arg(1,Refs,Head),
    ast_arg(2,Refs,Tail),
    (	ast_simple_match(Ref,Head)
    ;	ast_member(Ref,Tail) 
    ).