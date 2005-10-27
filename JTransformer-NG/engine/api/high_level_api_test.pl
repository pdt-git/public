/**************************** tests *********************/


/*******************fullQualifiedName *********************/

%:- use_module('../test/punit.pl').
%:- import_module(punit,_).

setUp(fullQualifiedName1) :- setUpFQN.

test(fullQualifiedName1):-
    fullQualifiedName(c2,'Test').

tearDown(fullQualifiedName1) :- tearDownFQN.
    
setUp(fullQualifiedName2) :- setUpFQN.

test(fullQualifiedName2):-
    fullQualifiedName(c1,'pckg.Test').

tearDown(fullQualifiedName2) :- tearDownFQN.


setUpFQN :-
    add(classDefT(c1,p1,'Test',[])),
    add(globalIds(c1,'pckg.Test')),
    assert(packageT(p1,'pckg')),
    assert(classDefT(c2,null,'Test',[])).
    
tearDownFQN :-
	rollback.	    
