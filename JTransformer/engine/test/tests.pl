% Author: Tobias
% Date: 31.07.04

:- multifile test/1.
:- multifile setUp/1.
:- multifile tearDown/1.

:- dynamic failed/0.
:- dynamic test/1.

/*
runTests

Test framework for JTransformer.
Use runTests/0 to start the testing.
For every predicate test/1 with optional setUp/1 and tearDown/1
predicates the test is executed.
e.g.:
test(name1) :- ...
setUp(name1) :- ...
tearDown(name1) :- ...
will be excecuted in the order setUp, test, tearDown.
Every predicate may fail or throw exceptions, 
the other predicates will run nevertheless.
Every exception will be printed to the screen.
After the execution of all tests a summary is shown.
Presenting all failed test names and how many test were
failed und how many succeeded.
*/

runTests :- failed(_list, _length, _all),
		counter(0),
        writef("\n=======================\nfailed tests:\n"),
        write(_list),
        writef("\n=======================\nnumber of failed tests: (%d/%d)\n", [_length, _all]).

runTest(Testname):-
	not(testfailed(Testname)).
        

testfailed(Testname) :- 
%    format('the test case ~a does not exist.',[Testname]),
    not(clause(test(Testname), _)),
    format('the test case ~a does not exist.',[Testname]),
    !.

testfailed(Testname) :- 
	writef('.'), 
%	print(_testname),
%	incCounter(Counter),
%	print(Counter),
%	debugme,
    reportException(setUp,Testname),
    reportException(test,Testname),
    reportException(tearDown,Testname),
    !,
    failed,
	retractall(failed).

reportException(_query,_testname):-
    Head =.. [_query,_testname],
	not(clause(Head,_)),
	!.

reportException(Pred,Testname):-
    Goal =.. [Pred,Testname],
    catch(Goal, Catcher, 
    	( term_to_atom(Catcher, Atom),
    	  format('predicate ~a for test ~a catched exception: ~a~n',[Pred, Testname,Atom]),
    	  assert(failed))
    ).
    
reportException(Pred,Testname):-
    format('predicate ~a for test ~a failed~n',[Pred, Testname]),
    assert(failed).
    
% testfailed(_testname) :- test(_testname), writef('.').


failed(_list, _length, _all) :- findall(
                                    [_testname],
                                    (clause(test(_testname), _),
                                     testfailed(_testname)
                                    ), 
                                    _list
                                  ),
                                  length(_list, _length),
                                  findall([_test],
                                          clause(test(_test),_),
                                          _tests
                                  ),
                                  length(_tests, _all).


assert_true(Goal) :-
    assert_true('', Goal).

assert_true(Comment, Goal) :-
  call(Goal)
    ->  true
    ; (
		prolog_current_frame(Frame),
%		prolog_frame_attribute(Frame,parent,Parent),
		stack_for_frame(Frame,Info),
		%term_to_atom(Info,StackTrace),
		list_to_line_sep_string(Info,StackTrace),
		term_to_atom(Goal, AtomGoal),
    	sformat(FormattedComment, 'The goal ~n  ~a~n failed:~n~a.~n~a~n', [AtomGoal, Comment,StackTrace]),
   		throw(assertion_failed(FormattedComment))
   	  ).
   	  
   	  
list_to_line_sep_string([],'').

list_to_line_sep_string([Head|Tail],String):-
	list_to_line_sep_string(Tail,StringTail),
	sformat(String,'~a~n~a',[Head,StringTail]).
  
assert_fail(Goal) :-
    assert_fail('', Goal).
    

assert_fail(Comment, Goal) :-
  (call(Goal)
    ->  (
		term_to_atom(Goal, AtomGoal),
    	sformat(FormattedComment, 'The goal ~n   ~a~n unexpectedly succeeded:~n~a.', [AtomGoal, Comment]),
   		throw(assertion_failed( FormattedComment))
   	);
   	true).


/*
surr2:-
    testAssertTrue(A).

testAssertTrue('sadlfkj') :-
	assert_true('aha',fail).
*/