% the pef base should create specialized versions of the predicates in this file for each 
% concrete pef type.
%
% My idea is to create a very simple partial evaluator specialized on the predicates
% in this file. 

 
cleanall(Data):-    
    functor(Data,Type,_),
    '$metapef_template'(Type,Template),
    find_id(Template,IdNum),
    findall(ref(RType,RArg),metapef_ref(Type,RType,RArg),Refs),
    findall(csc(CArg,CArgType),metapef_cascade(Type,CArg,CArgType),Cscs),
    findall(IxArg,metapef_index_arg(Type,IxArg),IxArgs),
    forall(
    	pef_retract(IxArgs,Data),
    	(	clean_refs(Refs,Data,IdNum),
    		clean_cascade(Data,Cscs)
    	)
    ).
    	
pef_retract(IxArgs,Data):-
    functor(Data,Type,_),    
    pef_query(IxArgs,Data,Ref),
	pef_retract_ixs(IxArgs,Data,Ref),
	erase(Ref).
	
pef_retract_ixs([],_Data,_Ref).
pef_retract_ixs([IxArg|IxArgs],Data,Ref):-
	ix_lookup(Data,IxArg,Ref,IxRef),
	erase(IxRef),
	pef_retract_ixs(IxArgs,Data,Ref).


pef_query([],Data,Ref):-
    clause(Data,_,Ref).
pef_query([IxArg|IxArgs],Data,Ref):-
	arg(IxArg,Data,Val),
	(	nonvar(Val)
	->	ix_lookup(Data,IxArg,Ref,_),
		clause(Data,_,Ref),			
	;	pef_query(IxArgs,Data,Ref)
	).


ix_lookup(Data,IxArg,ClauseRef,IxRef):-
    functor(Data,Type,_),
	arg(IxArg,Data,Val),
	index_name(Type,IxArg,IxName),
	IxHead=..[IxName,Val,ClauseRef],
	(	var(ClauseRef)
	->	clause(IxHead,_,IxRef),
		clause(Data,_,ClauseRef)
	;	clause(IxHead,_,IxRef)
	).

clean_refs(Refs,Data,IdNum):-
    arg(IdNum,Data,Id),
    clean_refs(Refs,Id).

clean_refs([],Id).
clean_refs([ref(RType,RArg)|Refs],Id):-
    '$metapef_template'(Rtype,RTemplate),
    functor(RTemplate,_,Arity),
    functor(RHead,_,Arity),
    arg(RArg,RHead,Id),
