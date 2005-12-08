:-module(member_annotator,[]).

:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).

%we require that the export_annotator is also registered
:- register_annotator(library('/org/cs3/pdt/annotate/export_annotator')).

term_post_annotation_hook(_,_,FileAnos,InTerm,OutTerm):-
    module_definition(FileAnos,FileModule,_),
    pdt_strip_annotation(InTerm, Term, (TopAnnos, ArgAnnos)),
	has_head(Term,FileModule, Module, Functor, Arity),
    pdt_splice_annotation(Term, ([clause_of(Module:Functor/Arity)|TopAnnos],ArgAnnos),OutTerm).

file_post_annotation_hook(_,_,Terms,InAnos,[defines(Definitions)|InAnos]):-
    collect_definitions(Terms,Definitions).
    

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
    