:- module(treefactwriter,[
writeTreeFacts/1,
clearPersistantFacts/0,
clearTreeFactbase/0
]).

/* 
  treefactwriter writes all current tree elements 
  to a specified pl file.
*/


writeTreeFacts(File):-
    open(File, write, Stream,[]),
    forall((persistant(Fact), %Fact = sourceLocation(_,_,_,_), 
    		 call(Fact),
    		 term_to_atom(Fact, Atom)
    	    ),
    		format(Stream, '~a.~n',Atom)
    ),
    		
    forall(globalIds(FQN,Id),
    	   format(Stream, 'globalIds(''~a'',~a).~n',[FQN,Id])
    ),
    
    lastID(LastID),
    format(Stream, ':- retractall(lastID(_)),assert(lastID(~a)).',[LastID]),
    close(Stream).

/**
 * persistant(-Fact)
 * All facts tree facts, 
 * 
 
*/    
    
persistant(Fact) :-
    treeFact(Fact);
    (
     (
      (Head=ct,Arity=3);
      (Head=aj_ct_list,Arity=1)
     ),
     uniqueArgumentList(Arity,Arguments),
     Fact =.. [Head|Arguments]
    ).
    
/*
 treeFact(-Fact)
*/    
treeFact(Fact) :-
    (
      treeSignature(Head,Arity);
      attribSignature(Head,Arity)
    ),
    uniqueArgumentList(Arity,Arguments),
    Fact =.. [Head|Arguments].

clearTreeFactbase :-
   forall((treeFact(A),user:call(A)),user:retract(A)).

clearPersistantFacts :-
   persistant(A),
   call(A),
   fail.
   
clearPersistantFacts.