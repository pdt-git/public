:-module(metapred_finder, [find_meta_pred_args_in_clause/3]).

:-use_module(metafile_referencer).

%analyse_a_module(Module,MetaVars).








find_meta_pred_args_in_clause(Module, Head, MetaArgs):-
	 clause(Module:Head, Body),
	 find_meta_vars_in_body(Body, Module, [],  MetaVars),
	 find_meta_vars_in_head(Head, MetaVars, MetaArgs).
	 
find_meta_vars_in_body(A, _, MetaVars, MetaVars):-
    atomic(A),
    !.
find_meta_vars_in_body(A, _, MetaVars, MetaVars):-
    var(A),
    !.
find_meta_vars_in_body(Module:Term, _, KnownMetaVars, MetaVars):-
    !, 
    find_meta_vars_in_body(Term, Module, KnownMetaVars, MetaVars).
    
find_meta_vars_in_body((TermA, TermB), Context, KnownMetaVars, MetaVars):-
	!, 														
   	find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),		
   	find_meta_vars_in_body(TermA, Context, MetaVarsB, MetaVars).		%erst B dann A -> nach vorne propagieren
   															% alternativ evtl einfach aliasse / unifizierungen merken
find_meta_vars_in_body((TermA; TermB), Context, KnownMetaVars, MetaVars):-
    !, 
   	find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),
   	find_meta_vars_in_body(TermA, Context, MetaVarsB, MetaVars).
   	  
find_meta_vars_in_body((TermA = TermB), _Context, KnownMetaVars, MetaVars):-
    !,
%    find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),
%   	find_meta_vars_in_body(TermA, Context, MetaVarsB, OwnMetaVars),
   	(	occurs_in(TermA, KnownMetaVars/*OwnMetaVars*/)
   	->	add_var_to_set(TermB, KnownMetaVars/*OwnMetaVars*/, OwnMetaVars2)
   	;	OwnMetaVars2 = KnownMetaVars%OwnMetaVars
   	),
   	(	occurs_in(TermB, OwnMetaVars2)
   	->	add_var_to_set(TermA, OwnMetaVars2, MetaVars3)
   	;	MetaVars3 = OwnMetaVars2
   	),
   	check_inner_vars(TermA, TermB, MetaVars3, MetaVars).
     
find_meta_vars_in_body(Term, Context, KnownMetaVars, MetaVars):-
    is_metaterm(Context, Term, MetaCombos), !, 
    extract_vars(MetaCombos, MetaArgs),
    handel_meta_args(MetaArgs, Context, KnownMetaVars, MetaVars).

find_meta_vars_in_body(_Term, _Context, MetaVars, MetaVars). 
		% everything else is a direct call without relevance
      
      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_meta_vars_in_head(Head, MetaVars, MetaArgs):-		%TODO: hier noch sharing realisieren
    Head =.. [_Functor|Args],
    find_args_in_list(Args,MetaVars,MetaArgs).
    
find_args_in_list([],_,[]).
find_args_in_list([Arg|Rest], MetaVars, MetaArgs):-
    find_args_in_list(Rest,MetaVars,RestMetaArgs),
    (	occurs_in(Arg,MetaVars)
    ->	MetaArgs=[0|RestMetaArgs]
    ;	MetaArgs=[?|RestMetaArgs]
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
      
extract_vars([],[]).
extract_vars([(_,Var)|RestCombo], [Var|RestVars]):-
    extract_vars(RestCombo, RestVars).
    
    
    
    
  
handel_meta_args([], _, Known, Known).
handel_meta_args([A|Rest], Context, Known, MetaVars):-
    var(A), !,  
    add_var_to_set(A, Known, OwnMetaVars),
    handel_meta_args(Rest, Context, OwnMetaVars, MetaVars).
handel_meta_args([A|Rest], Context, Known, MetaVars):-
    handel_meta_args(Rest, Context, Known, AllOthers),
    find_meta_vars_in_body(A, Context, AllOthers, MetaVars).
   
   

	 
	
check_inner_vars(TermA,TermB,OldMetaVars,NewMetaVars):-
	unifiable(TermA, TermB, Unifiers),	!,		%TODO: diese Lösung funktioniert nur für Variablen nicht für
	check_unifier_list(Unifiers,OldMetaVars,NewMetaVars).%      Terme in OldMetaVars
%   	findall(	B,								
%   				(	member(A=B, Unifiers),		
%   					occurs_in(A,OldMetaVars)
%   				),
%   				MetaBs
%   			),
%	findall(	A,
%				(	member(A=B, Unifiers),
%					occurs_in(B, OldMetaVars)
%				),
%				MetaAs
%			),
%	combine_sets_nonbinding(MetaBs, OldMetaVars, OldAndBMetaVars),
%	combine_sets_nonbinding(MetaAs, OldAndBMetaVars, NewMetaVars).				
check_inner_vars(_, _, MetaVars, MetaVars).


check_unifier_list([], Metas, Metas).
check_unifier_list([A=B|Rest], OldMetas, Metas):-	%	TODO: p(A):- term(A,B)= term(C,C), call(B)
	(	occurs_in(A, OldMetas)						% 	funktioniert so nicht! 
	->	add_var_to_set(B, OldMetas, Metas1)			
	;	Metas1 = OldMetas
	),
	(	occurs_in(B, OldMetas)
	->	add_var_to_set(A, Metas1, Metas2)
	;	Metas2 = Metas1
	),
	check_unifier_list(Rest, Metas2, Metas). 


combine_sets_nonbinding([],Set,Set).
combine_sets_nonbinding([E|Rest],OldSet,NewSet):-
    add_var_to_set(E,OldSet,Set),
    combine_sets_nonbinding(Rest,Set,NewSet).
    
    
/* add_var_to_set(?Var, +Set, ?NewSet) is det.
 * 
 * Arg3 is the same as Arg2 but if Arg1 is not already an element
 * of Arg2 it is addes as a first element to Arg3.
 * 
 * Attention: the comparision is based on == instead of =, so
 * 			  different variables are treated differently.
 */
add_var_to_set(Var, Set, NewSet):-
    (	occurs_in(Var, Set)
    ->	NewSet = Set
    ;	NewSet = [Var|Set]
    ).
    
    
/* occurs_in(?Var, +Set) is det.
 * 
 * Succseds, if Arg1 is equal to a member of Arg2.
 * The comparision is done with == instead of =!
 */ 
occurs_in(Var, Set):-
	findall(	OldVar, 
    			(	nth1(_, Set, OldVar),
    				OldVar == Var
    			),
    			AllOldVar
    		),
    not(AllOldVar == []).  
    
    
    
