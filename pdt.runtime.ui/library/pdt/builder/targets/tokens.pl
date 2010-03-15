:- module(tokens,[]).
	

:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/ast')).
:- use_module(library('facade/pdt_workspace')).

:- pdt_define_context(cx(toplevel,positions,root)).

pdt_builder:target_container(tokens(Resource),tokens(Container)):-
    pdt_builder:target_container(Resource,Container).
pdt_builder:build_hook(tokens(Resource)):-
    pdt_request_target(Resource),
	(	Resource=file(AbsFile)    
	->  (	get_pef_file(AbsFile,FID)
	    ->	pdt_request_target(ast(file(AbsFile))),
			forall(
		    	pef_toplevel_query([file=FID,id=Tl]),    	
		    	tokens:attach_tokens(Tl)
		    )
		;	true
		)
	;	forall(pdt_contains(Resource,Element),pdt_request_target(tokens(Element)))
	).

   

pdt_builder:target_file(tokens(file(F)),F).
pdt_builder:target_file(tokens(directory(F,_,_)),F).
pdt_builder:target_mutable(tokens(workspace),true).
pdt_builder:target_mutable(tokens(project(_)),true).

attach_tokens(Tl):-
    pef_toplevel_query([id=Tl,positions=Positions]),
    pef_ast_query([toplevel=Tl,root=Root]),
    attach_tokens(Positions,Root).
    
attach_tokens(none,_):-!.
attach_tokens(Positions,Node):-
	term_tokens(Positions,Tokens),
	pef_property_assert([pef=Node,key=tokens,value=Tokens]),
	forall(
		pef_arg_query([parent=Node,num=Num,child=Child]),
		(	sub_position(Positions,Num,ArgPositions),
			attach_tokens(ArgPositions,Child)
		)
	).    
