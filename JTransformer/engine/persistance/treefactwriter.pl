:- module(treefactwriter,[writeTreeFacts/1]).

/* 
  treefactwriter writes all current tree elements 
  to a specified pl file.
*/


writeTreeFacts(File):-
    open(File, write, Stream,[]),
    findall(Fact,(treeFact(Fact), 
    			  call(Fact),
    			  term_to_atom(Fact, Atom),
    			  format(Stream, '~a.~n',Atom)),
    _),
    findall(Fact,(globalIds(FQN,Id), 
    			  format(Stream, 'globalIds(''~a'',~a).~n',[FQN,Id])),
    _),
    lastID(LastID),
    format(Stream, ':- retractall(lastID(_)),assert(lastID(~a)).',[LastID]),
    close(Stream).

/*
 treeFact(-Fact)
*/    
    
treeFact(Fact) :-
    (treeSignature(Head,Arity);
    attribSignature(Head,Arity)),
    uniqueArgumentList(Arity,Arguments),
    Fact =.. [Head|Arguments].
    
    