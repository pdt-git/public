:- module(rename_file2,[]).
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

:- use_module(library('transform/transform')).

transform:interprete_selection('pdt.refactorings.RenameFile',file(Path),params(File,Name)):-
    get_pef_file(Path,File),
    file_base_name(Path,Name).


transform:perform_transformation('pdt.refactorings.RenameFile',params(File,NewName)):-
    pdt_with_targets([],
    	(	request_relevant_targets(File,NewName),
    		execCTS(ctseq(pdt_rename_file(File,NewName)) )
    	)
    ).
    
request_relevant_targets(File,NewName):-
	pdt_request_target(parse(workspace)),
	forall(
		pef_file_dependency_query([dependency=File,depending=ReferingFile,toplevel=Toplevel]),	
		(	get_pef_file(ReferingPath,ReferingFile),
			pdt_request_target(ast(file(ReferingPath)))
		)
	).
	    

ctseq(pdt_rename_file(File,NewName),
	negseq(
		pdt_rename_file__generate_problems(File,NewName,fatal),
		orseq( 
			pdt_rename_file__generate_problems(File,NewName,_),
			andseq(
				pdt_rename_file__do_rename(File,NewName,OldPath),
				pdt_rename_file__update_dependencies(File,OldPath )
			)
		)
	)
).


ct(pdt_rename_file__do_rename(File,NewName,OldPath),
    condition(
        pef_file_query([file=File,path=OldPath]),
        file_directory_name(OldPath,OldDir),
        concat_atom([OldDir,NewName],'/',NewPath)
    ),
    transformation(
        pef_file_retractall([id=File]),
        pef_file_assert([id=File,path=NewPath])
    )
).

ct( pdt_rename_file__generate_problems(File,NewName,Severity), 
      condition(
        get_pef_file(OldPath,File),
        file_directory_name(OldPath,OldDir),
        concat_atom([OldDir,NewName],'/',NewPath),
        do_check(OldPath,NewPath,Severity,Msg),
        (   string(Msg)
                    ->  Msg=MsgString
                    ;   string_to_list(MsgString,Msg)
        )
      ),
      transformation(
        pef_transformation_problem_assert([severity=Severity,message=MsgString])
      )
).   
    
ctseq( pdt_rename_file__update_dependencies(File,OldPath ),
	andseq(
		ct(
			condition(
		   		pef_file_dependency_query([dependency=File,depending=ReferingFile,toplevel=Toplevel]),	
				%get_pef_file(ReferingPath,ReferingFile),
				%pdt_request_target(ast(file(ReferingPath))),
				find_file_reference(Toplevel,RefTerm,Reference),
				resolve_reference(RefTerm,ReferingFile,OldPath)
			),
			action(skip)
		),
		andseq(
			pdt_generate_file_reference(File,ReferingFile,NewReference),
			pdt_ast_replace(Reference,NewReference)
		)	
		%pef_property_assert([pef=NewReference,key=copy,value=true]),	
		%mark_toplevel_modified(Toplevel).
	)
).

ctseq( pdt_generate_file_reference(File,ReferingFile,NewReference),
	andseq(
		ct(
			condition(
				pef_file_query([id=File,path=Path]),
				pef_file([id=ReferingFile,path=ReferingPath]),
				file_directory_name(ReferingPath,ContainerPath),
				atom_concat(ContainerPath,'/',Prefix)
			),
			action(
				skip
			)
		),
		negseq(
			&>( %if
				ct(condition(atom_concat(Prefix,RelativePath,Path)),action(skip)), 
				%then
			    pdt_generate_plain_file_reference(RelativePath,NewReference)
			),
			negseq(
				&>( %else if
					ct(
						condition(
							pdt_relative_path(Path,Alias,RelativePath),
							!
						),
						action(skip)
					), 
					%then
			    	pdt_generate_alias_file_reference(Alias,RelativePath,NewReference)
				),
				% else
				generate_plain_file_reference(Path,NewReference) 	
			)
		)
	)
).


ct( pdt_generate_plain_file_reference(RelativePath,NewReference),
	condition(pef_reserve_id(pef_term,NewReference)),
    action(pef_term_assert([id=NewReference,arity=0,name=RelativePath]))
).


ct( pdt_generate_alias_file_reference(Alias,RelativePath,NewReference),
	condition(
		pef_reserve_id(pef_term,NewReference),
		pef_reserve_id(pef_term,NewReference)
	),    
    action(
    	pef_term_assert([id=NewReference,arity=1,name=Alias]),
    	pef_term_assert([id=Child,arity=0,name=RelativePath]),
    	pef_arg_assert([parent=NewReference,num=1,child=Child])
    )
).


    
    
    


do_check(OldPath,NewPath,fatal, "Old and new path are equivalent."):-
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
do_check(_,_,warning,Message):-
	pef_file_not_found_query([file_spec=Spec]),
	format(string(Message), "Unresolved file reference: ~w",[Spec]).    
	
do_it(OldPath,NewPath):-
    get_pef_file(OldPath,File),
    pef_file_retractall([id=File]),
    pef_file_assert([id=File,path=NewPath]),
    mark_file_renamed(File,OldPath),
    forall(
    	pef_file_dependency_query([dependency=File,toplevel=Toplevel]),
    	pdt_with_targets([],update_dependency(Toplevel,File,OldPath))
    ).
    
    
find_dependency(File,OldPath,ReferingFile,Reference):-
    pef_file_dependency_query([dependency=File,toplevel=Toplevel]),
    pef_toplevel_query([id=Toplevel, file=ReferingFile]),
    pef_file_query([path=ReferingPath,file=ReferingFile]),
    %%FIXME
    %pdt_request_target(ast(file(ReferingPath))),
    find_file_reference(Toplevel,RefTerm,Reference),
    resolve_reference(RefTerm,ReferingFile,OldPath).
  %  !, warum?
   % generate_file_reference(File,ReferingFile,NewReference),
   % ast_replace(Reference,NewReference),
   % pef_property_assert([pef=NewReference,key=copy,value=true]),    
   % mark_toplevel_modified(Toplevel).   	





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
	pef_file_query([path=ReferingPath,id=ReferingFile]),
	file_directory_name(ReferingPath,Dir),
	pdt_file_spec(RefTerm,Dir,Path).
	
ast_member(Ref,Refs):-
    ast_functor(Refs,(.),2),
    ast_arg(1,Refs,Head),
    ast_arg(2,Refs,Tail),
    (	ast_simple_match(Ref,Head)
    ;	ast_member(Ref,Tail) 
    ).