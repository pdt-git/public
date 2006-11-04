:- module(punit_for_modules,
	[runTestsInModule/1,
	runTestsInModules/1,
	runTest/2]).

:- thread_local active_module/1.

runTestsInModule(Module) :-
    nonvar(Module),
    current_module(Module),
    assert(active_module(Module)),
    failed_in_module(FailedTests, Failed, All,Module),
	counter(0),
    writef("\n=======================\nfailed tests:\n"),
    write(FailedTests),
    plus(Failed,Success,All),
    writef("\n=======================\nsuccessful tests: (%d/%d)\n", [Success, All]),
    retract(active_module(Module)).

runTestsInModules(Modules) :-
    ground(Modules),
    runTestsInModules(FailedTests, Failed, All, Modules),
    counter(0),
    maplist(print_failed_module_tests, Modules, FailedTests),
    plus(Failed,Success,All),
    writef("\n=======================\nsuccessful tests: (%d/%d)\n", [Success, All]).

print_failed_module_tests(Module, FailedTests) :-
    writef("\n=======================\nfailed tests in module %d:\n", [Module]),
    write(FailedTests).

runTestsInModules([], 0, 0, []).

runTestsInModules(FailedTests, Failed, All, [Module|Tail]) :-
	process_module(Module, Module_FailedTests, Module_Failed, Module_All),
    runTestsInModules(Tail_FailedTests, Tail_Failed, Tail_All, Tail),
    append([Module_FailedTests], Tail_FailedTests, FailedTests),
    plus(Module_Failed, Tail_Failed, Failed),
    plus(Module_All, Tail_All, All).

process_module(Module, FailedTests, Failed, All) :-
    current_module(Module),
    assert(active_module(Module)),
    failed_in_module(FailedTests, Failed, All, Module),
    retractall(active_module(_)).

process_module(Module, '***Unknown module***', 1, 1) :-
    not(current_module(Module)).

/**
* runTest(+Testname, +Module)
*
* Runs the test with given name from Module.
*/
runTest(Testname, Module) :-
    current_module(Module),
    assert(active_module(Module)),
	not(testfailed(Testname, Module)),
	retractall(active_module(_)).  

runTest(_, _) :-
    retractall(active_module(_)),
    fail.

failed_in_module(Set, Length, All, Module) :- 
	findall(
    	[Testname],
        (
        	test_case_in_module(Testname,Module),
            testfailed(Testname, Module)
        ), 
        List
    ),
    list_to_set(List, Set), %remove duplicates
    length(Set, Length),
    findall(
    	[Test],
        test_case_in_module(Testname,Module),
        Test
    ),
    length(Test, All).

test_case_in_module(Name, Module):-
   clause(':'(Module, test(Name)),_).

testfailed(Testname, Module) :- 
    not(clause(':'(Module, test(Testname)), _)),
    format('the test case ~w does not exist.',[Testname]),
    !.

testfailed(Testname, Module) :- 
	writef('.'),
    reportException(Module, setUp, Testname),
    reportException(Module, test, Testname),
    reportException(Module, tearDown, Testname),
    !,
    failed,
	retractall(failed).

reportException(Module, Query, Testname):-
    Head =.. [Query, Testname],
	not(clause(':'(Module, Head),_)),
	!.

reportException(Module, Pred,Testname):-
    Goal =.. [Pred,Testname],
    catch(':'(Module, Goal), Catcher, 
    	( term_to_atom(Catcher, Atom),
    	  format('predicate ~a for test ~a in module ~a catched exception: ~a~n',
    	  	[Pred, Testname, Module, Atom]),
    	  assert(failed))
    ).
    
reportException(Module, Pred, Testname):-
    format('predicate ~w for test ~w in module ~w failed~n',[Pred, Testname, Module]),
    assert(failed).