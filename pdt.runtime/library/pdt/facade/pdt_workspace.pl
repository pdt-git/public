:-module(pdt_workspace,[
	pdt_file_added/1,
	pdt_file_changed/1,
	pdt_file_removed/1,
	pdt_add_source_path/4,
	pdt_remove_source_path/2
	]
).

:-use_module(library('builder/builder')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).


	

pdt_file_added(Abs):-
    pdt_invalidate_target(file(Abs)).    
    
pdt_file_changed(Abs):-
    pdt_invalidate_target(file(Abs)).
	
pdt_file_removed(Abs):-
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
    	path=Path,
    	include_pattern=Include, 
    	exclude_pattern=Exclude
    ]),
    pdt_invalidate_target(project(Project)).
    