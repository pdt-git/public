:-module(member_annotator,[]).

:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_rbtree')).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).

%we require that the export_annotator is also registered
:- register_annotator(library('/org/cs3/pdt/annotate/export_annotator')).

term_post_annotation_hook(_,_,FileAnos,InTerm,OutTerm):-
    (	module_definition(FileAnos,FileModule,_)
    ->	true
    ;	FileModule=user
    ),
    pdt_strip_annotation(InTerm, Term, (TopAnnos, ArgAnnos)),
	process_member(Term,FileModule,AddAnno),
    pdt_splice_annotation(Term, ([AddAnno|TopAnnos],ArgAnnos),OutTerm).

process_member(Term,FileModule,clause_of(Module:Functor/Arity)):-
    has_head(Term,FileModule, Module, Functor, Arity),
    Functor\=':-'.
process_member(':-'(Term),FileModule,Annotation):-
    property_definition(Term,Property,Signatures),
    module_qualified_signatures(FileModule,Signatures,ModuleQualifiedSignatures),
    Annotation=..[Property,ModuleQualifiedSignatures].

property_definition(Term,defines_dynamic,Sigs):-
    Term=..[dynamic|Sigs].
property_definition(Term,defines_multifile,Sigs):-
    Term=..[multifile|Sigs].
property_definition(Term,defines_module_transparent,Sigs):-
    Term=..[module_transparent|Sigs].    
    
module_qualified_signatures(_,[],[]).
module_qualified_signatures(FileModule,[InH|InT],[OutH|OutT]):-
    module_qualified_signature(FileModule,InH,OutH),
    module_qualified_signatures(FileModule,InT,OutT).

module_qualified_signature(FileModule,Name/Arity,FileModule:Name/Arity).
module_qualified_signature(_,Module:Name/Arity,Module:Name/Arity).
module_qualified_signature(_,(Module:Name)/Arity,Module:Name/Arity).
    


file_post_annotation_hook(_,_,Terms,InAnnos,[defines(Definitions)|OutAnnos]):-
    collect_definitions(Terms,Definitions),
    findall(Property,property_definition(_,Property,[_]),Properties),
    collect_properties(Properties,Terms,InAnnos,OutAnnos).

collect_properties([],_,In,In).
collect_properties([Property|Properties],Terms,In,[PropTerm|PropTerms]):-
    collect_property(Property,Terms,Sigs),
    PropTerm=..[Property,Sigs],
    collect_properties(Properties,Terms,In,PropTerms).
    
collect_property(_,[],Sigs,Sigs).
collect_property(Property,[Term|Terms],InSigs,OutSigs):-    
    pdt_top_annotation(Term,TopAn),
    collect_property_X(Property,TopAn,MySigs),
    merge_set(InSigs,MySigs,NextSigs),  
    collect_property(Property,Terms,NextSigs,OutSigs).

collect_property_X(Property,TopAn,OutSigs):-
    PropTerm=..[Property,Sigs],
    	pdt_member(PropTerm,TopAn),
    	!,
    sort(Sigs,OutSigs).
collect_property_X(_,_,[]).

collect_definitions(Terms,Definitions):-
    pdt_rbtree_new(Tree0),
    collect_definitions(Terms,Tree0,Tree),
    findall(A,pdt_rbtree_next('',A,_,Tree),Definitions).
    

collect_definitions([],Tree,Tree).   
collect_definitions([Term|Terms],InTree,OutTree):-
    pdt_top_annotation(Term,TopAn),
    pdt_member(clause_of(Definition),TopAn),
    !,
    pdt_rbtree_insert(InTree,Definition,Definition,NextTree),
	collect_definitions(Terms,NextTree,OutTree).
collect_definitions([Term|Terms],InTree,OutTree):-
	collect_definitions(Terms,InTree,OutTree).

    
module_definition(FileAnos,Module,Exports):-
	pdt_member(defines_module(Module),FileAnos),
	pdt_member(exports(Exports),FileAnos).
	
	
has_head(':-'(Module:HeadTerm,_),_,Module,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).
    
has_head(':'(Module,':-'(HeadTerm,_)),_,Module,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).
has_head(':-'(HeadTerm,_),FileModule,FileModule,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).
has_head(':'(Module,HeadTerm),_,Module,Functor,Arity):-
    !,   
    functor(HeadTerm,Functor,Arity).    
has_head(HeadTerm,FileModule,FileModule,Functor,Arity):-
    !,
    functor(HeadTerm,Functor,Arity).    
    