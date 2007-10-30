:-module(pdt_workspace,[
	pdt_file_contents_changed/1,
	pdt_file_existence_changed/1,
	pdt_add_source_path/4,
	pdt_remove_source_path/2,
	pdt_contains/2,
	pdt_belongs_to/2
	]
).

:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/workspace')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).


	

pdt_contains(Container,Resource):-
	(	ground(Container)
	->	pdt_with_targets([Container],contains(Container,Resource))
	;	ground(Resource)
	->	pdt_with_targets([workspace],belongs_to(Resource,Container))
	;	throw(error(instantiation_error, context(pdt_workspace:pdt_contains/2,_)))
	).

pdt_belongs_to(Resource,Container):-
    pdt_contains(Container,Resource).
	
contains(A,C):-
	contains_edge(A,B),
	(	B=C
	;	contains(B,C)
	).
	
belongs_to(A,C):-
	belongs_to_edge(A,B),
	(	B=C
	;	belongs_to(B,C)
	).

	
contains_edge(workspace,project(Name)):-
    pef_project_query([name=Name]).
contains_edge(project(Name),directory(Path,IP,EP)):-
	pef_project_query([name=Name,id=Project]),
	pef_source_path_query([project=Project,path=Path,include_pattern=IP,exclude_pattern=EP]).
contains_edge(directory(Path,IP,EP),Resource):-
    pef_directory_query([path=Path,include_pattern=IP,exclude_pattern=EP, id=Parent]),
    pef_directory_entry_query([parent=Parent,child=Child]),
    (	pef_type(Child,pef_directory)
	->	pef_directory_query([id=Child,path=CPath,include_pattern=CIP,exclude_pattern=CEP]),
		Resource=directory(CPath,CIP,CEP)
	;	pef_file_query([id=Child,path=CPath]),
		Resource=file(CPath)
	).

belongs_to_edge(file(CPath),directory(Path,IP,EP)):-
    pef_file_query([id=Child,path=CPath]),
    pef_directory_entry_query([parent=Parent,child=Child]),
    pef_directory_query([path=Path,include_pattern=IP,exclude_pattern=EP, id=Parent]).
belongs_to_edge(directory(CPath,CIP,CEP),directory(Path,IP,EP)):-
    pef_directory_query([id=Child,path=CPath,include_pattern=CIP,exclude_pattern=CEP]),
    pef_directory_entry_query([parent=Parent,child=Child]),
    pef_directory_query([path=Path,include_pattern=IP,exclude_pattern=EP, id=Parent]).
belongs_to_edge(directory(Path,IP,EP),project(Name)):-    
	pef_source_path_query([project=Project,path=Path,include_pattern=IP,exclude_pattern=EP]),
	pef_project_query([name=Name,id=Project]).
belongs_to_edge(project(Name),workspace):-
	pef_project_query([name=Name]).	

        
    
pdt_file_contents_changed(Abs):-
    pdt_invalidate_target(parse(Abs)).
    
	
pdt_file_existence_changed(Abs):-
    pdt_invalidate_target(file(Abs)).
    


    
pdt_remove_source_path(Project,Path):-
	(	pef_project_query([name=Project,id=PRJID])
	->	pef_source_path_retractall([project=PRJID,path=Path]),
		pdt_invalidate_target(project(Project))
	;	true
	).
pdt_add_source_path(Project,Path,Include,Exclude):-
	(	pef_project_query([name=Project,id=PRJID])
	->	true
	;	pef_reserve_id(pef_project,PRJID),
		pef_project_assert([id=PRJID,name=Project])
	),	
    (	pef_source_path_query([path=Path,id=SPID,include_pattern=OldIncl, exclude_pattern=OldExcl])
    ->	pef_source_path_retractall([id=SPID]),
    	pdt_invalidate_target(directory(Project,OldIncl,OldExcl))
    ;	pef_reserve_id(pef_source_path,SPID)
    ),    
    pef_source_path_assert([
    	id=SPID,
    	project=PRJID,
    	path=Path,
    	include_pattern=Include, 
    	exclude_pattern=Exclude
    ]),
    pdt_invalidate_target(project(Project)).
    
