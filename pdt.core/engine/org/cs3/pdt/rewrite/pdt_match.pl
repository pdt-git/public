:- module(pdt_match,[
	pdt_match/3
]).



/*
pdt_match(+Term, +Pattern, -Match)

Searches for a sub term of Term that is unifyable with Pattern.

Match will be unified with a term of the form

  match(<path>,<unifier>)
  
 <path> is a list of integers representing argument positions that can be used to
 recursively traverse the term. They form a path to the matching subterm
 
 <unifier> is a list of Var=Subst terms representing the substitution that 
 is required to unify Pattern with the matching term. See unifyable/3.

*/    
pdt_match(Term,Pattern,match([ArgNum|SubPath],Unifier)):-
    compound(Term),   
	arg(ArgNum,Term,SubTerm),
    pdt_match(SubTerm,Pattern,match(SubPath,Unifier)).
pdt_match(Term,Pattern,match([],Unifier)):-
    copy_term(Term,Cpy),
    numbervars(Cpy,0,_),
    unifyable(Cpy,Pattern,Unifier).

%hey, that was easy, wasn't it ? :-)