:- module(treefactwriter,[writeTreeFacts/1]).

/* 
  treefactwriter writes all current tree elements 
  to a specified pl file.
*/


writeTreeFacts(File):-
    open(File, write, Stream,[]),
    forall((treeFact(Fact), %Fact = sourceLocation(_,_,_,_), 
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

/*
 treeFact(-Fact)
*/    
    
treeFact(Fact) :-
    (treeSignature(Head,Arity);
    (Head=ct,Arity=3);
    attribSignature(Head,Arity)),
    uniqueArgumentList(Arity,Arguments),
    Fact =.. [Head|Arguments].
    
    