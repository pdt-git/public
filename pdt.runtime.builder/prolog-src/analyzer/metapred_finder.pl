:-module(metapred_finder, [find_meta_pred_call_in_clause/3]).

:-use_module(metafile_referencer).

%analyse_a_module(Module,MetaVars).

find_meta_pred_call_in_clause(Module, Head, MetaVars):-
	 clause(Module:Head, Body),
	 find_meta_vars_in_body(Body, Module, [],  MetaVars).
	 
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
   	  
find_meta_vars_in_body((TermA = TermB), Context, KnownMetaVars, MetaVars):-
    !,
    find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),
   	find_meta_vars_in_body(TermA, Context, MetaVarsB, OwnMetaVars),
   	(	occurs_in(TermA, OwnMetaVars)
   	->	add_var_to_set(TermB, OwnMetaVars, OwnMetaVars2)
   	;	OwnMetaVars2 = OwnMetaVars
   	),
   	(	occurs_in(TermB, OwnMetaVars)
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
	 
	
check_inner_vars(A,B,C,C).	