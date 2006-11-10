

test(assertTree) :-
    assert_true(inTe(classDefT(1,2,3,4))),
    assert_true(ri_parent(2,1)).

tearDown(assertTree):-
	retractall(ri_parent(2,1)).
    
    