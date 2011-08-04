:- consult('../load_builder').


generate_factbase_with_metapred_analysis(File):-
    %format('###File: ~w~n', [File]), 
    with_mutex(meta_pred_finder,
    	(	generate_factbase(File),
    		find_all_meta_predicates
    	)
    ),
    get_all_userdefined_meta_predicates(MetaPreds),
    format('### Userdefined meta pred: ~w~n', [MetaPreds]).

find_undeclared_meta_predicates_position(File, Offset, MetaSpec):-
    %generate_factbase_with_metapred_analysis(File),
    with_mutex(meta_pred_finder,
    	get_all_userdefined_meta_predicates(MetaPreds)
    ),
    !,
    member(Module:MetaSpec,MetaPreds),
    filter_undeclared_predicate(Module, MetaSpec,File,Offset).
   

filter_undeclared_predicate(Module, MetaSpec, FileName, Offset):-
    \+(predicate_property(Module:MetaSpec,meta_predicate(_))),
    lookup_filename_and_offset(Module:MetaSpec, FileName, Offset).
    

  
%% find_missdeclared_meta_predicates_position(?File, ?Offset, ?DeclaredMetaSpec, ?CorrectMetaSpec)
find_missdeclared_meta_predicates_position(File, Offset, DeclaredMetaSpec, CorrectMetaSpec):-
	get_all_userdefined_meta_predicates(MetaPreds),  
    !,
    member(CorrectMetaSpec,MetaPreds),
    CorrectMetaSpec = Module:MetaSpec,
    predicate_property(Module:MetaSpec, meta_predicate(DeclaredMetaSpec)),
    \+(same_meta_specs(MetaSpec, DeclaredMetaSpec)),
    lookup_filename_and_offset(Module:MetaSpec, File, Offset).
    
    
same_meta_specs(MetaSpec, DeclaredMetaSpec):-
    MetaSpec =.. [Functor|Args],
    DeclaredMetaSpec =.. [Functor|DeclaredArgs],
    same_meta_args(Args,DeclaredArgs).
    	
    	
same_meta_args([],[]).
same_meta_args([A1|Rest1],[A2|Rest2]):-
    number(A1), 
    !,
    number(A2),
    same_meta_args(Rest1, Rest2).
same_meta_args([A1|Rest1],[A2|Rest2]):-
    member(A1, [?,+,-,:]),				%":" also counts as non-meta-arg	 
    !,
    member(A2, [?,+,-,:]),				%":" also counts as non-meta-arg
	same_meta_args(Rest1, Rest2).     
    
    
lookup_filename_and_offset(Module:Term, FileName, Offset):-
    functor(Term, Functor ,Arity),
    parse_util:predicateT_ri(Functor, Arity, Module, PId),
    parse_util:predicateT(PId,FileId,_,_,_),
    parse_util:fileT(FileId,FileName,_),
    parse_util:slT(PId,Offset,_).  