:- module(problems,[]).

:-use_module(library('pef/pef_base')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/workspace')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).
:-use_module(library('builder/targets/literals')).

:- use_module(library('facade/pdt_workspace')).

:-multifile kind/3.

pdt_builder:target_file(problems(directory(Path,_,_),_),Path).
pdt_builder:target_file(problems(file(Path),_),Path).
pdt_builder:target_mutable(problems(workspace,_),true).
pdt_builder:target_mutable(problems(project(_),_),true).


pdt_builder:estimate_hook(problems(Resource,Filter),problems(file(Path),Filter),1):-
    pdt_contains_star(Resource,file(Path)).

pdt_builder:report_progress(problems(workspace,_)).

pdt_builder:build_hook(problems(Resource,Filter)):-
    (	Resource=file(Path)
    ->	forall(member(Kind,Filter),request_problems(Kind,Path))
    ;    pdt_request_target(Resource),
    	pdt_contains(Resource,Element),
    	pdt_request_target(problems(Element,Filter))
    ).

request_problems(Kind,Path):-
    forall(kind(Kind,Path,Dep),pdt_request_target(Dep)).
	    

