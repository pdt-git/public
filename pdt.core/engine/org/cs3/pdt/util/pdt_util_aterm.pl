:- module(pdt_util_aterm,[
	pdt_strip_anotation/3,
	pdt_splice_anotation/3
]).

pdt_strip_anotation(AnotatedTerm,Term,Anotation):-
    nonvar(AnotatedTerm),check_aterm(AnotatedTerm),
    !,
    strip(AnotatedTerm,Term,Anotation).

pdt_splice_anotation(Term,Anotation,AnotatedTerm):-
    check_anotation(Term,Anotation),
    !,
    splice(Term,Anotation,AnotatedTerm).

check_anotation(Term,(TopAn,ArgsAn)):-
    nonvar(ArgsAn),    
    unbound_tail(TopAn),
 	(   var(Term)
 	->	ArgsAn=[]
	;	Term=..[_|Args],
		functor(Term,_,Arity),
    	length(ArgsAn,Arity),
    	check_arg_anotations(Args,ArgsAn)
    ),!.
check_anotation(Term,Anotation):-
    throw(invalid_anotation(term(Term),anotation(Anotation))).
    
check_arg_anotations([],[]).
check_arg_anotations([H|T],[AnH|AnT]):-
    check_anotation(H,AnH),
    check_arg_anotations(T,AnT).

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
