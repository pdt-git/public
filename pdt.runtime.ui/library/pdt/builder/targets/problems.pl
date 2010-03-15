:- module(problems,[]).

:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/workspace')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).
:-use_module(library('builder/targets/literals')).

:- use_module(library('facade/pdt_workspace')).

:-multifile kind/3.
pdt_builder:target_container(problems(Resource,Filter),problems(Container,Filter)):-
    pdt_builder:target_container(Resource,Container).
pdt_builder:target_file(problems(directory(Path,_,_),_),Path).
pdt_builder:target_file(problems(file(Path),_),Path).
pdt_builder:target_mutable(problems(workspace,_),true).
pdt_builder:target_mutable(problems(project(_),_),true).

pdt_builder:estimate_hook(problems(Resource,[cheap]),parse(file(Path)),1):-
    pdt_contains_star(Resource,file(Path)).
pdt_builder:estimate_hook(problems(workspace,[expensive]),T,W):-
    pef_project_query([name=Project]),
    pdt_builder:estimate_hook(problems(project(Project),[expensive]),T,W).

pdt_builder:estimate_hook(problems(project(ProjectName),[expensive]),T,1):-
	pef_project_query([name=ProjectName,id=Project]),
	pef_entry_point_query([project=Project,path=Path]),
	get_pef_file(Path,File),
	file_depends_star(File,Dep),
	get_pef_file(DepPath,Dep),
	(	T=literals(first,file(DepPath))
	;	T=interprete(file(DepPath))
	;	T=ast(file(DepPath))
	).

pdt_builder:report_progress(problems(workspace,_)).

pdt_builder:build_hook(problems(Resource,Filter)):-
    
    pdt_request_target(Resource),
    
    (	Resource=file(Path)    	
    ->	forall(member(Kind,Filter),request_problems(Kind,Path))
    ;	Resource=project(Name)
    ->	pef_project_query([id=Project,name=Name]),
    	forall(
    		pef_entry_point_query([project=Project,path=Path]),
    		pdt_request_target(problems(file(Path),Filter))    		
    	)
    ;   pdt_contains(Resource,Element),
    	pdt_request_target(problems(Element,Filter))
    ).

request_problems(Kind,Path):-
    forall(kind(Kind,Path,Dep),pdt_request_target(Dep)).
	    

