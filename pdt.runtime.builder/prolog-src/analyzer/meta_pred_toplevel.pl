:- consult('../load_builder').

find_undeclared_meta_predicates_position(File, Offset, MetaSpec):-
    generate_factbase,
    get_all_userdefined_meta_predicates(MetaPreds),
    !,
    member(Module:MetaSpec,MetaPreds),
    filter_undeclared_predicate(Module, MetaSpec,File,Offset).
   

filter_undeclared_predicate(Module, MetaSpec, FileName, Offset):-
    \+(predicate_property(Module:MetaSpec,meta_predicate(_))),
    functor(MetaSpec, Functor ,Arity),
    parse_util:predicateT_ri(Functor, Arity, Module, PId),
    parse_util:predicateT(PId,FileId,_,_,_),
    parse_util:fileT(FileId,FileName,_),
    parse_util:slT(PId,Offset,_).
    
    
    