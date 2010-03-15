:-module(predicates,[	derive_all_predicates/0,
						derive_predicate_for_clause/6,
						derive_onloads/0,
						analyse_directive/4]).
:- ensure_loaded(parse_util).

derive_all_predicates:-
    forall( ruleT(CId,File,Module,Functor,Arity),
    		derive_predicate_for_clause(CId,Functor,Arity,Module,File,_PId)
    ).
    	  
derive_predicate_for_clause(CId,Functor,Arity,Module,_File,PId):-
	predicateT_ri(Functor,Arity,Module,PId),
	!,
	compute_new_length(PId,CId),
    assert(pred_edge(CId,PId)).
derive_predicate_for_clause(CId,Functor,Arity,Module,File,PId):-    
    new_node_id(PId),	
    assert(node_id(PId)),
    assert(predicateT(PId,File,Functor,Arity,Module)),
    assert(predicateT_ri(Functor,Arity,Module,PId)),
    slT(CId,Begin,Length),
    assert(slT(PId,Begin,Length)),  
    assert(pred_edge(CId,PId)).
    
    
derive_onloads:-
    forall( directiveT(Id,File,Module),
    	  	(	(	(	onloadT(PId,File,Module)	
    				->	(	assert(onload_edge(Id,PId)),
    						compute_new_length(PId,Id)
    					)	  					
    				;	(	new_node_id(PId),	
    						assert(node_id(PId)),
    						assert(onloadT(PId,File,Module)),  
    						
    						slT(Id,Begin,Length),
    						assert(slT(PId,Begin,Length)),  
   
    						assert(onload_edge(Id,PId))
    					)	
    		  		) 			
    			)
    		; 	termT(Id,Term),
    			writeln(Term)
    		)
    	  ). 

compute_new_length(PId,Id) :-
	slT(PId,PBegin,PLength),
    PEnd is PBegin + PLength,
    slT(Id,Begin,Length),
   	End is Begin + Length,
    NewBegin is min(PBegin,Begin),
    NewEnd is max(PEnd,End),
    NewLength is NewEnd - NewBegin,
    retract(slT(PId,PBegin,PLength)),
    assert(slT(PId,NewBegin,NewLength)).

compute_all_predicate_properties:-
    forall(	property_dir(FileId,Functor,Args,ParentId,Pos),
    		compute_predicate_property(Functor, Args, ParentId, Module, Pos)
    	).
    	
compute_predicate_property(Functor, Args, ParentId, Module, Pos).

/**
 * analyse_directive(+Directive,+Pos,+ParentId,+Module)
 *   looks into the term of Arg1 and if it is a known 
 *   kind of directive term stores accordingly information
 *   that can be used in the former parsing or x-referencing
 *   process (like modules, operators, dynamics, transparencies,
 *   metafile,...)
 **/
analyse_directive(Directive,Pos,ParentId,Module):-     % dynamic
	Directive =.. [dynamic|Dyns],
	conjunction_to_list(Dyns,Dynamics),
	Pos = term_position(_From,_To,_FFrom,_FTo, SubPos),
	forall(	nth1(Nr,Dynamics,Dynamic),
		  	(	nth1(Nr,SubPos,SubPosi),
				SubPosi = term_position(F,T,_FF,_FT,_S),
				Dynamic=Functor/Arity,
				assert_new_node(Dynamic,F,T,Id),
				assert(dynamicT(Id,Functor,Arity,Module,ParentId))
			)
		).	
analyse_directive(Directive,Pos,ParentId,Module):-     % module_transparent
	Directive =.. [module_transparent|Transps],
	conjunction_to_list(Transps,Transparents),
	Pos = term_position(_From,_To,_FFrom,_FTo, SubPos),
	forall(	nth1(Nr,Transparents,Transparent),
		  	(	nth1(Nr,SubPos,SubPosi),
				SubPosi = term_position(F,T,_FF,_FT,_S),
				Transparent=Functor/Arity,
				assert_new_node(Transparent,F,T,Id),
				assert(transparentT(Id,Functor,Arity,Module,ParentId))
			)
		).	
analyse_directive(Directive,Pos,ParentId,Module):-     % multifile
	Directive =.. [multifile|Multis],
	conjunction_to_list(Multis,Multifiles),
	Pos = term_position(_From,_To,_FFrom,_FTo, SubPos),
	forall(	nth1(Nr,Multifiles,Multifile),
		  	(	nth1(Nr,SubPos,SubPosi),
				SubPosi = term_position(F,T,_FF,_FT,_S),
				Multifile=Functor/Arity,
				assert_new_node(Multifile,F,T,Id),
				assert(multifileT(Id,Functor,Arity,Module,ParentId))
			)
		).
/*analyse_directive(Directive,Pos,ParentId,Module):-     % module_transparent
	Directive =.. [multifile|Multis],
	conjunction_to_list(Multis,Multifiles),
	Pos = term_position(_From,_To,_FFrom,_FTo, SubPos),
	forall(	nth1(Nr,Multifiles,Multifile),
		  	(	nth1(Nr,SubPos,SubPosi),
				SubPosi = term_position(F,T,_FF,_FT,_S),
				Multifile=Functor/Arity,
				assert_new_node(Multifile,F,T,Id),
				assert(multifileT(Id,Functor,Arity,Module,ParentId))
			)
		).	*/	
analyse_directive(_,_,_,_). 
/*****
* Eva: wirklich assert_new_node oder doch nur new_id? Und wirklich ...T???
***/   	

conjunction_to_list([],[]).
conjunction_to_list([A|B],[A|B]).
conjunction_to_list((A,B),[A,B]) :-
   atom(B),!. 
conjunction_to_list((A,B),[A|BasList]) :-
   conjunction_to_list(B,BasList). 