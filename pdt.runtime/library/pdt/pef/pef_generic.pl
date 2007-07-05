%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the pef base should create specialized versions of the predicates in this file for each 
% concrete pef type.
%
% My idea is to create a very simple partial evaluator specialized on the predicates
% in this file. 

 
pef_base:cleanall(Data):-   
	debug(pef_generic,"~w~n",[cleanall(Data)]), 
    functor(Data,Type,_),
    '$metapef_template'(Type,Template),
    (	find_id(Template,IdNum)
    ->  findall(ref(RType,RArg),metapef_ref(Type,RType,RArg),Refs)
    ;	Refs=[]
    ),
    findall(csc(CArg,CArgType),metapef_cascade(Type,CArg,CArgType),Cscs),
    findall(IxArg,metapef_index_arg(Type,IxArg),IxArgs),
    forall(
    	pef_retract(IxArgs,Data),
    	(	debug(pef_generic,"retracted ~w~n",[Data]),
    		clean_refs(Refs,Data,IdNum),
    		clean_cascade(Cscs,Data)
    	)
    ).
    	
pef_base:pef_retract(IxArgs,Data):-    
    pef_query(IxArgs,Data,Ref),
	pef_retract_ixs(IxArgs,Data,Ref),
	erase(Ref).
	
pef_base:pef_retract_ixs([],_Data,_Ref).
pef_base:pef_retract_ixs([IxArg|IxArgs],Data,Ref):-
	ix_lookup(Data,IxArg,Ref,IxRef),
	erase(IxRef),
	pef_retract_ixs(IxArgs,Data,Ref).


pef_base:pef_query([],Data,Ref):-
    clause(Data,_,Ref).
pef_base:pef_query([IxArg|IxArgs],Data,Ref):-
	arg(IxArg,Data,Val),
	(	nonvar(Val)
	->	ix_lookup(Data,IxArg,Ref,_),
		clause(Data,_,Ref)	
	;	pef_query(IxArgs,Data,Ref)
	).


pef_base:ix_lookup(Data,IxArg,ClauseRef,IxRef):-
    functor(Data,Type,_),
	arg(IxArg,Data,Val),
	index_name(Type,IxArg,IxName),
	IxHead=..[IxName,Val,ClauseRef],
	(	var(ClauseRef)
	->	clause(IxHead,_,IxRef),
		clause(Data,_,ClauseRef)
	;	clause(IxHead,_,IxRef)
	).

pef_base:clean_refs(Refs,Data,IdNum):-
    arg(IdNum,Data,Id),
    clean_refs(Refs,Id).

pef_base:clean_refs([],_Id).
pef_base:clean_refs([ref(RType,RArg)|Refs],Id):-
    '$metapef_template'(RType,RTemplate),
    functor(RTemplate,_,Arity),
    functor(RHead,RType,Arity),
    arg(RArg,RHead,Id),
    debug(pef_generic,"clearing possible references to ~w from ~w.~n",[Id,RHead]), 
	cleanall(RHead),
	clean_refs(Refs,Id).

pef_base:clean_cascade([],_Data).	
pef_base:clean_cascade([csc(CArg,CArgType)|Cscs],Data):-
	'$metapef_template'(CArgType,CArgTemplate),
	find_id(CArgTemplate,CArgIDNum),
	functor(CArgTemplate,_,Arity),
    functor(CArgHead,CArgType,Arity),
    arg(CArg,Data,CArgID),
    arg(CArgIDNum,CArgHead,CArgID),
    debug(pef_generic,"cascading from ~w to ~w.~n",[Data,CArgHead]),
    cleanall(CArgHead),
    clean_cascade(Cscs,Data).