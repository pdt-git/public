:- module(pdt_check_singletons,[pdt_check_singletons/2]).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_dfs')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_annotation')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).


%pdt_check_singletons(+File,-Occurance)
%succeeds if Occurance is an annotated subterm in File that is a singleton variable.
pdt_check_singletons(File,Occurance):-
    pdt_file_annotation(File,_,Terms),
    check_singletons(Terms,Occurance).


check_singletons_terms([Term|_],Occurance):-
    check_singletons_term(Term,Occurance).
check_singletons_terms([_|Terms],Occurance):-
	check_singletons_terms(Terms,Occurance).    
	
check_singletons_term(Term,Occurance):-
    pdt_term_annotation(Term,_,_),
    
	pdt_subterm(Term,_,Occurance),
	singleton(Occurance).
	
%singleton(Occurance)	:-
%    pdt_aterm(Occurance),
%    pdt_term_annotation(Occurance,_,Anno),
%    pdt_annotation_get(Anno,singletons,Singletons)