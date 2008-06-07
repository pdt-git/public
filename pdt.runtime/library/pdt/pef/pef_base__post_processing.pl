meta_edge_clause(Clause):-
    '$metapef_edge'(FromT,ArgNum,ToT),
    '$metapef_template'(FromT,FromTemplate),
    functor(FromTemplate,_,Arity),
    functor(FromHead,FromT,Arity),
    arg(ArgNum,FromTemplate,ArgName),
    arg(ArgNum,FromHead,To),
	metapef_is_a(SubT,ToT),
	'$metapef_concrete'(SubT),
	(	find_id(FromTemplate,IdNum)  
    ->	arg(IdNum,FromHead,From),
    	Clause=
    		(	'$pef_edge'(From,FromT,ArgName,To,SubT):-
        			call(FromHead),
        			pef_type(To,SubT)
        	)
	;   Clause =
			(	'$pef_edge'(From,FromT,ArgName,To,SubT):-
        			clause(FromHead,_,From),
        			pef_type(To,SubT)
        	)
    ).


index_name__legacy(Tmpl,ArgNum,IxName):-    
    integer(ArgNum),
    !,    
    functor(Tmpl,Type,_),
    arg(ArgNum,Tmpl,ArgName),
    concat_atom([Type,revix,ArgName],'$',IxName).
index_name__legacy(Tmpl,[ArgName|ArgNames],IxName):-  
	!,  
    functor(Tmpl,Type,_),
    concat_atom([Type,cmpix|[ArgName|ArgNames]],'$',IxName).
index_name__legacy(Tmpl,ArgName,IxName):-    
    functor(Tmpl,Type,_),
    concat_atom([Type,revix,ArgName],'$',IxName).


inverse_meta_edge_clause(Clause):-
    '$metapef_edge'(FromT,ArgNum,ToT),
    '$metapef_template'(FromT,FromTemplate),
    functor(FromTemplate,_,Arity),
    functor(FromHead,FromT,Arity),
    arg(ArgNum,FromTemplate,ArgName),
    arg(ArgNum,FromHead,To),   	
   	% use reverse index, if available.
   	(	'$metapef_attribute_tag'(FromT,ArgNum,index)
   	->	index_name__legacy(FromTemplate,ArgNum,IxName),
    	IndexQuery=..[IxName,To,FromRef],
    	Query=(IndexQuery,clause(FromHead,_,FromRef))
    ;	Query=clause(FromHead,_,FromRef)
    ),    
	metapef_is_a(SubT,ToT),
	'$metapef_concrete'(SubT),
	(	find_id(FromTemplate,IdNum)  
    ->	arg(IdNum,FromHead,From)	    	
	;   From=FromRef
    ),
    Clause= (	'$pef_inverse_edge'(To,SubT,ArgName,From,FromT):-
        			Query
        	).		    
	


meta_node_clause(Clause):-
    '$metapef_concrete'(Name),
    '$metapef_template'(Name,Tmpl),
    functor(Tmpl,Name,Arity),
    functor(Head,Name,Arity),
    findall(ArgNum,'$metapef_attribute_tag'(Name,ArgNum,label), ArgNums),
    args(ArgNums,Head,Labels),
    (	find_id(Tmpl,IdNum)  
    ->	arg(IdNum,Head,Id),
    	(	IdNum==1
    	->	Clause=('$pef_node'(Name,Id,Labels):-Head)
    	;	index_name__legacy(Tmpl,IdNum,IxName),
    		IxQuery=..[IxName,Id,Ref],
    		Clause=('$pef_node'(Name,Id,Labels):-IxQuery,clause(Head,_,Ref))
    	)
	;   Clause =
			(	'$pef_node'(Name,Id,Labels):-
        			clause(Head,_,Ref),
        			Id=Ref
        	)
    ).
    	

    
args([],_,[]).
args([Num|Nums],Term,[Arg|Args]):-
    arg(Num,Term,Arg),
    args(Nums,Term,Args).

user:term_expansion((:-postprocess_pefs),Clauses):-
   findall(Clause,
   		(	meta_edge_clause(Clause)
   		;	inverse_meta_edge_clause(Clause)
   		;	meta_node_clause(Clause)
   		),
   		Clauses
   	).
    