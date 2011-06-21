:-module(metapred_finder, [find_meta_pred_call_in_clause/3]).

:-use_module(metafile_referencer).

%analyse_a_module(Module,MetaVars).

find_meta_pred_call_in_clause(Module, Head, MetaVars):-
	 clause(Module:Head, Body),
	 find_meta_vars_in_body(Body, Module,  MetaVars).
	 
find_meta_vars_in_body(true, _, []):-
    !.
find_meta_vars_in_body(A, _, []):-
    atomic(A),
    !.
find_meta_vars_in_body(A, _, []):-
    var(A),
    !.
find_meta_vars_in_body(Module:Term, _; MetaVars):-
    !, 
    find_meta_vars_in_body(Term, Module, MetaVars).
    
find_meta_vars_in_body((TermA, TermB), Context, MetaVars):-
	!, 														% weiterer Parameter für bekannte, dieser dann 
   	find_meta_vars_in_body(TermB, Context, MetaVarsB),		% im anderen Zweig nutzen (gleiche bei oder?)
   	find_meta_vars_in_body(TermA, Context, MetaVarsA),		% (erst A dann B, damit "nach vorne" propagiert)
   	append(MetaVarsA, MetaVarsB, MetaVars).					% alternativ evtl einfach aliasse / unifizierungen merken
   	   	
find_meta_vars_in_body((TermA; TermB), Context, MetaVars):-
    !, 
   	find_meta_vars_in_body(TermB, Context, MetaVarsB),
   	find_meta_vars_in_body(TermA, Context, MetaVarsA),
   	append(MetaVarsA, MetaVarsB, MetaVars).
   	  
%find_meta_vars_in_body((TermA = TermB), Context, MetaVars):-
%   	find_meta_vars_in_body
    	
     
find_meta_vars_in_body(Term, Context, MetaVars):-
    is_metaterm(Context, Term, MetaCombos), !, 
    extract_vars(MetaCombos, MetaArgs),
    handel_meta_args(MetaArgs, Context, MetaVars).

find_meta_vars_in_body(_Term, _Context, []). % everything else is a direct call without relevance
      
extract_vars([],[]).
extract_vars([(_,Var)|RestCombo], [Var|RestVars]):-
    extract_vars(RestCombo, RestVars).
    
    
handel_meta_args(MetaArgs, Context, MetaVars):-
    handel_meta_args_(MetaArgs, Context, [], MetaVars).	%zum Set machen
    
handel_meta_args_([], _, Known, Known).
handel_meta_args_([A|Rest], Context, Known, All):-
    var(A), !,
    handel_meta_args_(Rest, Context, [A|Known], All).
handel_meta_args_([A|Rest], Context, Known, All):-
    handel_meta_args_(Rest, Context, Known, AllOthers),
    find_meta_vars_in_body(A, Context, NewMetaVars),
    append(AllOthers, NewMetaVars, All).
    
    
    
    
    
	 
	