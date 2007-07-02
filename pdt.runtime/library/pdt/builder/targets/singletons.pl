:- module(singletons,[]).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/ast')).

pdt_builder:build_hook(singletons(Abs)):-
    pdt_with_targets([parse(Abs)],
    	(	singletons:forget_singletons(Abs),
    		singletons:check_singletons(Abs)).

pdt_builder:invalidate_hook(parse(Abs)):-
	pdt_invalidate_target(singletons(Abs)).



forget_singletons(Abs):-
    get_pef_file(Abs,FID),
    forall(
    	(	pef_toplevel_query([file=FID,id=TlID])
    		
    	)
    	(	pef_singelton_retractall([toplevel=TlID])
    ).
    
check_singletons(Abs):-
	get_pef_file(Abs,FID),
	forall(
		pef_toplevel_query([file=FID,id=TL,singletons=Singletons,varnames=VarNames]),
		(	check_singletons(Singletons,TL),
			check_no_singletons(VarNames,Singletons,TL)
		)
	).    
	
check_singletons([],_).
check_singletons([Name=_Var|Singletons],Tl):-
    pdt_request_target(ast(Tl)),
    pef_variable_query([toplevel=Tl,name=Name,id=VarId]),
    pef_reserve_id(pef_singleton,ID),
    pef_singleton_assert([id=ID,variable=VarId]),
    check_singletons(Singletons,Tl).

check_no_singletons([],_,_).
check_no_singletons([Name=_Var|VarNames],Singletons,Tl):-
    (	atom_prefix(Name,'_'),
    	\+ atom_prefix(Name,'__'),
    	\+ memberchk(Name=_,Singletons)
    ->	pdt_request_target(ast(Tl)),
	    pef_variable_query([toplevel=Tl,name=Name,id=VarId]),
	    pef_reserve_id(pef_singleton,ID),
	    pef_singleton_assert([id=ID,variable=VarId])
	;	true
	),
    check_no_singletons(VarNames,Singletons,Tl).
		