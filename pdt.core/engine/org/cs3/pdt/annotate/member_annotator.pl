:-module(member_annotator,[]).

:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
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
collect_properties([H|T],Terms,In,[Prop|Tmp]):-
    collect_properties(T,Terms,In,Tmp),
    collect_property(H,Terms,Sigs),
    Prop=..[H,Sigs].
    
collect_property(_,[],[]).
collect_property(Property,[TermH|TermT],Sigs):-    
    collect_property(Property,TermT,SigsT),
    pdt_strip_annotation(TermH,_,(TopAn,_)),
    PropTerm=..[Property,SigsH],
    (	pdt_member(PropTerm,TopAn)    	
    ->	append(SigsT,SigsH,Appended),
    	list_to_set(Appended,Sigs)
    ;	Sigs=SigsT
    ).

collect_definitions([],[]).    
collect_definitions([TermH|TermT],Definitions):-
    collect_definitions(TermT,DefinitionT),
    pdt_strip_annotation(TermH,_,(TopAn,_)),
    (	( pdt_member(clause_of(DefinitionH),TopAn),
    	  \+ memberchk(DefinitionH,DefinitionT)
    	)
    ->	Definitions=[DefinitionH|DefinitionT]
    ;	Definitions=DefinitionT
    ).
    
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
    