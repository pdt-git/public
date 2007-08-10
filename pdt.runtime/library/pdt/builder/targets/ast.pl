:- module(ast,[]).
	

:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/parse')).

:- pdt_define_context(cx(toplevel,positions,root)).

pdt_builder:build_hook(asts(AbsFile)):-
    get_pef_file(AbsFile,FID),
    pdt_request_targets(parse(AbsFile)),
	forall(
    	pef_toplevel_query([file=FID,id=Tl]),
    	pdt_request_target(ast(Tl))
    ).
pdt_builder:build_hook(ast(Tl)):-
    ast:rebuild(Tl).


pdt_builder:invalidate_hook(parse(AbsFile)):-
    get_pef_file(AbsFile,FID),
    forall(
    	pef_toplevel_query([file=FID,id=Tl]),
    	pdt_invalidate_target(ast(Tl))
    ).
pdt_builder:invalidate_hook(ast(Tl)):-
    pef_toplevel_query([id=Tl,file=FID]),
    get_pef_file(AbsFile,FID),
    pdt_invalidate_target(asts(AbsFile)).


rebuild(Tl):-    
    %forget_ast(Tl),
    build_ast(Tl).

/*
forget_ast(TL):-
    pef_ast_cleanupall([toplevel=TL]).
  */  

build_ast(Tl):-
    pef_reserve_id(pef_ast,Id),
    (	build_ast(Tl,Id,Root)    
    ->	pef_ast_assert([id=Id,toplevel=Tl,root=Root])
    ;	throw(failed(build_ast(Tl,Id,Root)))
    ).

%%
% pdt_generate_ast(+ToplevelRef, Id).
% 
% generate ast facts for a toplevel term.
% @param ToplevellRef the reference of the toplevel record.
% @param Id is unified with the id of root of the generated syntax tree.
build_ast(TID, RootId,Id):-
    cx_new(Cx),
    cx_root(Cx,RootId),
    cx_toplevel(Cx,TID),
    cx_positions(Cx,Positions),
    pef_toplevel_query([id=TID,term=Term,varnames=VarNames,positions=Positions]),    
    process_variables(VarNames,Term,Cx),
    generate_ast(Term, Id,Cx).
    
process_variables([],Term,Cx):-
	term_variables(Term,DCs),
	process_dcs(DCs,Cx).
process_variables([Name=Variable|Variables],Term,Cx):-
    pef_reserve_id(pef_variable,Id),
    cx_root(Cx,Root),
    pef_variable_assert([id=Id,name=Name,ast=Root]),
    Variable='$var'(Id),
	process_variables(Variables,Term,Cx).


process_dcs([],_Cx).
process_dcs([Var|Vars],Cx):-
    pef_reserve_id(pef_variable,Id),
    cx_root(Cx,Root),
    pef_variable_assert([id=Id,name='_',ast=Root]),
    Var='$var'(Id),
	process_dcs(Vars,Cx).
    
generate_ast('$var'(VarId), Id,Cx):-
    !,
    pef_reserve_id(pef_variable_occurance,Id),    
    pef_variable_occurance_assert([id=Id,variable=VarId]),
    cx_positions(Cx,Positions),
    (	Positions==none
    ->	pef_property_assert([pef=Id,key=implicit,value=true])
    ;	top_position(Positions,From,To),
    	pef_property_assert([pef=Id,key=start,value=From]),
    	pef_property_assert([pef=Id,key=end,value=To])
    ).
    
generate_ast(Term, Id,Cx):-
    functor(Term,Name,Arity),
	pef_reserve_id(pef_term,Id),
	pef_term_assert([id=Id,name=Name,arity=Arity]),
	cx_positions(Cx,Positions),
    (	Positions==none
    ->	pef_property_assert([pef=Id,key=implicit,value=true])
    ;	top_position(Positions,From,To),
    	pef_property_assert([pef=Id,key=start,value=From]),
    	pef_property_assert([pef=Id,key=end,value=To])
    ),
	generate_args(1,Arity,Term,Id,Cx).

generate_args(I,N,_Term,_Id,_Cx):-
    I>N,
    !.
generate_args(I,N,Term,Id,Cx):-	
	arg(I,Term,Arg),
	cx_positions(Cx,Positions),
	sub_position(Positions,I,ArgPositions),
	cx_set_positions(Cx,ArgPositions,ArgCx),
	generate_ast(Arg,ArgId,ArgCx),
	pef_arg_assert([num=I,parent=Id,child=ArgId]),
	J is I + 1,
	generate_args(J,N,Term,Id,Cx).

forget_subtree(Id):-
    pef_variable_occurance_query([id=Id,variable=Ref]),
    !,
    pef_variable_occurance_retractall([id=Id]),
    % if this was the last occurance, remove the variable.
	(	pef_variable_occurance_query([variable=Ref])
	->	true
	;	pef_variable_retractall([id=Ref])
	),
	pef_arg_retractall([child=Id]).
    
forget_subtree(Id):-
	forall(
		pef_arg_query([parent=Id,child=ChildId]),
		forget_subtree(ChildId)
	),
	pef_term_retractall([id=Id]),
	pef_arg_retractall([child=Id]).    
    
    