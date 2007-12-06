:- module(pdt_find_declaration,
	[	pdt_resolve_predicate/5,
		pdt_predicate_contribution/4,
		pdt_predicate_reference/6
	]).


:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('util/ast_util')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/literals')).



%%
% pdt_resolve_predicate(+CxFile,+CxModule,+Name,+Arity,-PredId).
% resolve a predicate reference in a given context.
%
% This predicate is intended to be used by the "Find Declaration" and "Find References" action of the 
% PDT's Editor for buffers that are "dirty". Its up to the java client to correctly guess 
% context, name and arity. The backend will use the last parsed known version of the file
% to do the rest.  
%
% @param CxFile absolute path to the entry point file.
% @param CxModule name of the context module from within which the predicate is referenced.
% @param Name The functor name of the referenced predicate.
% @param Arity The functor arity of the referenced predicate.
% @param PredId will be unified with the PEF Identifier of the resolved predicate.
pdt_resolve_predicate(CxFile,CxModule,Name,Arity,Pred):-
    get_pef_file(CxFile,FID),
    pdt_request_target(interprete(CxFile)),
    % if the module is not bound, try to guess it from the file.
    (	var(CxModule)
    ->	(	pef_module_definition_query([file=FID,name=CxModule])
    	->	true
    	;	CxModule=user
    	)
    ;	true
    ),    
    pef_program_query([file=FID,id=Prog]),
    resolve_module(Prog,CxModule,MID),
    resolve_predicate(Prog,MID,Name,Arity,Pred).


%%     
% pdt_predicate_contribution(+PredId,-File,-Start,-End).
% find source code contributing to a given predicate.
%
% As "contribution" we consider clauses and predicate property definitions.
%
% This predicate is intended to be used by the "Find Declaration" action of the 
% PDT's Editor.
%
% @param PredId the PEF identifier of the predicate.
% @param File will be unified with the absolute path of a file contributing to the predicate.
% @param Start character start offset of the contribution within the file
% @param End character end offset of the contribution within the file  
pdt_predicate_contribution(PredId,File,Start,End):-	
	(	pef_clause_query([predicate=PredId,toplevel=Toplevel])		
    ;	pef_predicate_property_definition_query([predicate=PredId,toplevel=Toplevel])
    ),
    toplevel_source_position(Toplevel,FID,Start,End),
	get_pef_file(File,FID).




	 
pdt_predicate_reference(Pred,File,Start,End,Element,Type):-
	pef_predicate_query([id=Pred,name=Name,arity=Arity]),
	% make sure already existing call edges are up to date
	(	literals:granularity(predicate)
	->	forall(
			pef_call_query([predicate=Pred,cx=Cx]),
			(	literals:cx_predicate(Cx,Caller),
				pef_predicate_query([id=Caller,name=CName, arity=CArity]),
				pdt_request_target(literals(full,predicate(CName/CArity)))
			)
		)
	;	forall(
			pef_call_query([predicate=Pred,cx=Cx]),
			(	literals:cx_toplevel(Cx,Tl),
				pef_toplevel_query([id=Tl,file=FID]),
				get_pef_file(PATH,FID),
				pdt_request_target(literals(full,file(PATH)))
			)
		)
	),
	% make sure all clauses containing the functor symbol are up to date
	pdt_request_target(inverse_search(full,Name)),
	
	% Now, finally, lookup the references.
	% start by finding all textual references, then categorize them into
	% a) calls to the given predicate
	% b) calls to another predicate
	% c) references to predicates that cannot be resolved
	% c) functor references, i.e. it does not look like the matching term constitutes a call.
	pef_term_query([name=Name,arity=Arity,id=Goal]),	
	pef_property_query([pef=Goal,key=start,value=Start]),
	pef_property_query([pef=Goal,key=end,value=End]),
	ast_toplevel(Goal,Tl),
	pef_toplevel_query([id=Tl,file=FID]),
		get_pef_file(File,FID),
		
	(	pef_call_query([predicate=Callee,goal=Goal,cx=Cx])
	->	(	Callee==Pred -> Type=this_pred_ref; Type=other_pred_ref ),
		literals:cx_predicate(Cx,Caller),
		pef_predicate_query([id=Caller,name=CName, arity=CArity,module=CMID]),
		module_name(CMID,CModule),
		Element=CModule:CName/CArity
	;	(	pef_unresolved_predicate_symbol_query([goal=Goal])
		->	Type=unresolved_pred_ref
		;	Type=functor_ref
		),		
		ast_root(Tl,Root),
		ast_head_body(Root,Module,Head,_),
		(	Head == []
		->	Element=directive(Tl)
		;	ast_functor(Head,CName,CArity),
			(	Module == []
			->	(	pef_module_definition_query([file=FID,name=CModule])
				->	true
				;	CModule=user
				)
			;	ast_apply(Module,_,CModule)
			),
			Element=CModule:CName/CArity
		)
	).