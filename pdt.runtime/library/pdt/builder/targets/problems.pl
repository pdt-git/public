:- module(problems,[]).

:-use_module(library('pef/pef_base')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/workspace')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).



exists_project(Name):-
    pef_project_query([name=Name]).
    
pdt_builder:build_hook(problems(workspace)):-
    setof(problems(project(ProjectName)),pef_project_query([name=ProjectName]),Deps),
    pdt_with_targets([workspace|Deps],true).
pdt_builder:build_hook(problems(project(Resource))):-
    setof(problems(Path,IP,EP),
    	(	pef_project_query([id=Project,name=Resource]),
    		pef_source_path_query([project=Project,path=Path,include_pattern=IP,exclude_pattern=EP])
    		
    	),
    	Deps
    ),
    pdt_with_targets([project(Resource)|Deps],true).
pdt_builder:build_hook(problems(file(Resource))):-    	    
    pdt_with_targets([file(Resource),parse(Resource),interprete(Resource),singletons(Resource)],true).
pdt_builder:build_hook(problems(directory(Dir,IP,EP))):-
	pdt_request_target(directory(Dir,IP,EP)),
	pef_directory_query([path=Dir,include_pattern=IP,exclude_pattern=EP,id=Parent]),
	forall(
		pef_directory_entry([parent=Parent,child=Child]),
		(	pef_type(Child,directory)
		->	pef_directory_query([id=Child,path=CPath,exclude_pattern=CIP,include_pattern=CEP]),
			pdt_request_target(problems(directory(CPath,CIP,CEP)))
		;	pef_file_query([id=Child,path=CPath]),
			pdt_request_target(problems(file(CPath)))
		)
	).	
