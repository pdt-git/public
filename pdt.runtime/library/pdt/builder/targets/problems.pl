:- module(problems,[]).

:-use_module(library('pef/pef_base')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/workspace')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).

:- use_module(library('facade/pdt_workspace')).

pdt_builder:target_file(problems(directory(Path,_,_)),Path).
pdt_builder:target_file(problems(file(Path)),Path).
pdt_builder:target_mutable(problems(workspace),true).
pdt_builder:target_mutable(problems(project(_)),true).


pdt_builder:estimate_hook(problems(Resource),problems(file(Path)),1):-
    pdt_contains(Resource,file(Path)).

pdt_builder:report_progress(problems(workspace)).
    
pdt_builder:build_hook(problems(workspace)):-
    spyme,
    setof(problems(project(ProjectName)),pef_project_query([name=ProjectName]),Deps),
    pdt_with_targets([workspace|Deps],true).
pdt_builder:build_hook(problems(project(Resource))):-
    spyme,
    setof(problems(directory(Path,IP,EP)),
    	(	pef_project_query([id=Project,name=Resource]),
    		pef_source_path_query([project=Project,path=Path,include_pattern=IP,exclude_pattern=EP])
    		
    	),
    	Deps
    ),
    pdt_with_targets([project(Resource)|Deps],true).
pdt_builder:build_hook(problems(file(Resource))):-    	    
    spyme,
    pdt_with_targets([file(Resource),parse(Resource),interprete(Resource),singletons(Resource)],true).
pdt_builder:build_hook(problems(directory(Dir,IP,EP))):-
    spyme,
	pdt_request_target(directory(Dir,IP,EP)),
	pef_directory_query([path=Dir,include_pattern=IP,exclude_pattern=EP,id=Parent]),
	forall(
		pef_directory_entry_query([parent=Parent,child=Child]),
		(	pef_type(Child,pef_directory)
		->	pef_directory_query([id=Child,path=CPath,exclude_pattern=CEP,include_pattern=CIP]),
			pdt_request_target(problems(directory(CPath,CIP,CEP)))
		;	pef_file_query([id=Child,path=CPath]),
			pdt_request_target(problems(file(CPath)))
		)
	).	
spyme.