:-module(predicates,[	derive_all_predicates/0,
						derive_predicate_for_clause/6,
						derive_onloads/0,
						compute_all_predicate_properties/0]).
:- ensure_loaded(parse_util).

derive_all_predicates:-
    forall( clauseT(CId,File,Module,Functor,Arity),
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
    forall(	parse_util:property_dir(Functor, Args, DirectiveId),
    		(	directiveT(DirectiveId, _, Module),
    			compute_predicate_property(Functor, Args, DirectiveId, Module)
    		)
    	).

/**
 * analyse_directive(+Directive,+ParentId,+Module)
 *   looks into the term of Arg1 and if it is a known 
 *   kind of directive term stores accordingly information
 *   that can be used in the former parsing or x-referencing
 *   process (like modules, operators, dynamics, transparencies,
 *   metafile,...)
 **/
compute_predicate_property(Prop, Preds, DirectiveId, Module):-     % dynamic
	conjunction_to_list(Preds,Predicates),
	forall(	
		member(Functor/Arity, Predicates),
		(	(	predicateT_ri(Functor,Arity,Module,PId)
			->	true
			;	(	(Prop = dynamic)
				->	(	new_node_id(PId),	
    					assert(node_id(PId)),
    					directiveT(DirectiveId, File, _),
    					assert(predicateT(PId,File,Functor,Arity,Module)),
    					assert(predicateT_ri(Functor,Arity,Module,PId))
					)
				;	fail	
				)
			),
			assert_prop(Prop, PId, DirectiveId)
		)
	).	
compute_predicate_property(_,_,_,_). 

 	

conjunction_to_list([],[]).
conjunction_to_list([A|B],[A|B]).
conjunction_to_list((A,B),[A,B]) :-
   atom(B),!. 
conjunction_to_list((A,B),[A|BasList]) :-
   conjunction_to_list(B,BasList).
   
   
assert_prop(dynamic, PredId, DirectiveId):-
    assert(dynamicT(PredId, DirectiveId)).
assert_prop(module_transparent, PredId, DirectiveId):-
    assert(transparentT(PredId, DirectiveId)).
assert_prop(multifile, PredId, DirectiveId):-
    assert(multifileT(PredId, DirectiveId)).
assert_prop(meta_predicate, PredId, DirectiveId):-
    assert(meta_predT(PredId, DirectiveId)). 
assert_prop(_,_,_).