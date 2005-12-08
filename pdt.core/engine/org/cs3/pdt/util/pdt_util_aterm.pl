:- module(pdt_util_aterm,[
	pdt_strip_annotation/3,
	pdt_splice_annotation/3
]).

pdt_strip_annotation(AnotatedTerm,Term,Anotation):-
    nonvar(AnotatedTerm),check_aterm(AnotatedTerm),
    !,
    strip(AnotatedTerm,Term,Anotation).

pdt_splice_annotation(Term,Anotation,AnotatedTerm):-
    check_annotation(Term,Anotation),
    !,
    splice(Term,Anotation,AnotatedTerm).

check_annotation(Term,(TopAn,ArgsAn)):-
    nonvar(ArgsAn),    
    unbound_tail(TopAn),
 	(   var(Term)
 	->	ArgsAn=[]
	;	Term=..[_|Args],
		functor(Term,_,Arity),
    	length(ArgsAn,Arity),
    	check_arg_annotations(Args,ArgsAn)
    ),!.
check_annotation(Term,Anotation):-
    throw(invalid_annotation(term(Term),annotation(Anotation))).
    
check_arg_annotations([],[]).
check_arg_annotations([H|T],[AnH|AnT]):-
    check_annotation(H,AnH),
    check_arg_annotations(T,AnT).

check_aterm(aterm(Top,AnotatedTerm)):-
    unbound_tail(Top),
    (	var(AnotatedTerm)
    ->	true
    ;	AnotatedTerm=..[_|Args],
    	check_arg_aterms(Args)
    ),!.
check_aterm(Term):-
	throw(invalid_aterm(Term)).    

check_arg_aterms([]).
check_arg_aterms([H|T]):-
	check_aterm(H),
	check_arg_aterms(T).
	
	    
unbound_tail(Term):-
    var(Term),!.
unbound_tail('.'(_,Tail)):-
    unbound_tail(Tail).

strip(aterm(HeadAnotation,AnotatedTerm),AnotatedTerm,(HeadAnotation,[])):-
	var(AnotatedTerm),
	!.
strip(aterm(HeadAnotation,AnotatedTerm),Term,(HeadAnotation,ArgAnotations)):-
	AnotatedTerm=..[Functor|AnotatedArgs],
	strip_args(AnotatedArgs,Args,ArgAnotations),
	Term=..[Functor|Args].

strip_args([],[],[]).
strip_args(	[AnotatedArgsH|AnotatedArgsT],
			[ArgsH|ArgsT],
			[ArgAnotationsH|ArgAnotationsT]):-
	strip(AnotatedArgsH,ArgsH,ArgAnotationsH),
	strip_args(AnotatedArgsT,ArgsT,ArgAnotationsT).
	
splice(Term,(HeadAnotation,[]),aterm(HeadAnotation,Term)):-
	var(Term),
	!.	
splice(Term,(HeadAnotation,ArgAnotations),aterm(HeadAnotation,AnotatedTerm)):-
    Term=..[Functor|Args],
    splice_args(Args,ArgAnotations,AnotatedArgs),
    AnotatedTerm=..[Functor|AnotatedArgs].

splice_args([],[],[]).
splice_args([ArgsH|ArgsT],
			[ArgAnotationsH|ArgAnotationsT],
			[AnotatedArgsH|AnotatedArgsT]):-
    splice(ArgsH,ArgAnotationsH,AnotatedArgsH),
    splice_args(ArgsT,ArgAnotationsT,AnotatedArgsT).
