:- module(assuming,[assuming/2]).

/*
warning:

the implementation is currently very naive, straight 
forward and limitid in its functionality. 

I assume it to work in my special application. Probably it 
needs a lot of tweaking to reasonably work with other tasks.


*/


/*
some of the many limitations:
- queries can only be sinlge literals. I.e. no junctors, let alone meta-calls.
- only allowed changes are retractall/1 and assert/1.
- only facts can be assumed, not rules.
*/


/*
changes should be understood as a stack: the head/top element represents the last change.
*/

assume(Module, Changes, Query):-
    Module:call(Query), 
    \+ retracted(Module,Changes,Query).
assume(Module, Changes, Query):-
    asserted(Module,Changes,Query).
    
asserted    