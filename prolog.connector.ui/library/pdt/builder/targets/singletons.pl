:- module(singletons,[]).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/ast')).
:-use_module(library('facade/pdt_workspace')).



pdt_builder:build_hook(singletons(Resource)):-
    pdt_request_target(Resource),
	(	Resource=file(AbsFile)    
	->  singletons:my_build_hook(AbsFile)    
	;	forall(pdt_contains(Resource,Element),pdt_request_target(singletons(Element)))
	).

   
pdt_builder:target_container(singletons(Resource),singletons(Container)):-
    pdt_builder:target_container(Resource,Container).
pdt_builder:target_file(singletons(file(F)),F).
pdt_builder:target_file(singletons(directory(F,_,_)),F).
pdt_builder:target_mutable(singletons(workspace),true).
pdt_builder:target_mutable(singletons(project(_)),true).

my_build_hook(Abs):-
    pdt_request_target(parse(file(Abs))),
	singletons:check_singletons(Abs).



 
toplevel_with_singletons(FID,TL,Singletons):-
 	pef_toplevel_query([file=FID,id=TL,singletons=Singletons/*,varnames=VarNames*/]),
	\+ pef_term_expansion_query([expanded=TL]). %for expansion: ignore expanded term.

check_singletons(Abs):-
	get_pef_file(Abs,FID),
	forall(
		toplevel_with_singletons(FID,TL,Singletons),
		(	check_singletons(Singletons,TL)/*,
			check_no_singletons(VarNames,Singletons,TL)*/
		)
	).    
check_singletons([],_).
check_singletons([Name=_Var|Singletons],Tl):-
    pef_toplevel_query([id=Tl,file=File]),
    get_pef_file(Path,File),
    pdt_request_target(ast(file(Path))),
    pef_ast_query([toplevel=Tl,id=Ast]),
    pef_variable_query([ast=Ast,name=Name,id=VarId]),
    pef_reserve_id(pef_singleton,ID),
    pef_singleton_assert([id=ID,variable=VarId]),
    check_singletons(Singletons,Tl).

check_no_singletons([],_,_).
check_no_singletons([Name=_Var|VarNames],Singletons,Tl):-
    (	atom_prefix(Name,'_'),
    	\+ atom_prefix(Name,'__'),
    	\+ memberchk(Name=_,Singletons)
    ->	pef_toplevel_query([id=Tl,file=File]),
    	get_pef_file(Path,File),
    	pdt_request_target(ast(file(Path))),
    	pef_ast_query([toplevel=Tl,id=Ast]),
	    pef_variable_query([ast=Ast,name=Name,id=VarId]),
	    pef_reserve_id(pef_singleton,ID),
	    pef_no_singleton_assert([id=ID,variable=VarId])
	;	true
	),
    check_no_singletons(VarNames,Singletons,Tl).
		