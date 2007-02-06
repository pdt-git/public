/**
a term is a goal if either of the following conditions holds:
 1) it is the body of a clause, 
 2) it is on a call site, that is, it is the i-th argument of a goal
   and the goal is resolved to a predicate which is known to call its i-th argument.
 3) it is unified with a VARIABLE that is on a call side.
 
 ad 3: a term that is unified with a variable cannot be on a call site.
 
 Thus:
 - if a variable is found on a call site, 
    * if the variable occurs in a clause head, i have to annotate this in the predicate properties.
 	* i have to go through the list of terms which are already known
   	  to be possible substitutions for the variable. Each of them has to be marked as a goal·
   	 
 
*/

ai_annotate_goal(InTerm,InContext,OutTerm,OutContext):-
    (	ai_resolve_predicate(InTerm,InContext,PredicateData)
    ->	findall(Path,ai_predicate_call_site(PredicateData,Path),Paths),
    	ai_annotate_call_sites(InTerm,InContext,Paths,OutTerm,OutContext)
    ;	ai_mark_as_unresolved(InTerm,InContext,OutTerm,OutContext)
    ).

ai_annotate_call_sites(Term,Context,[],Term,Context).    
ai_annotate_call_sites(InTerm,InContext,[Path|Paths],OutTerm,OutContext):-
    ai_annotate_call_site(InTerm,InContext,Path,NextTerm,NextContext),
    ai_annotate_call_sites(NextTerm,NextContext,Paths,OutTerm,OutContext).
    
ai_annotate_call_site(InTerm,InContext,Path,OutTerm,OutContext):-
	pdt_aterm_subterm(InTerm,Path,InCallSite),
	ai_annotate_goal(InCallSite,InContext,OutCallSite,OutContext),
	pdt_aterm_subst(InTerm,Path,OutCallSite,OutTerm).
	