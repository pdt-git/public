%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * (Module) punit.
 *
 * Test framework for Prolog.
 * Use runTests/0 to start the testing.
 * For every predicate test/1 with optional setUp/1 and tearDown/1
 *  * predicates the test is executed.
 * e.g.:
 *   punit:test(name1) :- ...
 *   punit:setUp(name1) :- ...
 *   punit:tearDown(name1) :- ...
 * will be excecuted in the order setUp, test, tearDown.
 * Every predicate may fail or throw exceptions, 
 * the other predicates will run nevertheless.
 * Every exception will be printed to the screen.
 * After the execution of all tests a summary is shown.
 * Presenting all failed test names and how many test were
 * failed und how many succeeded.
 * 
 * Use assert_true/2, assert_fail/2 and assert_bound/2 for
 * your test cases to check your predicates.
 *
 * @author Tobias Rho
 * @created 31.07.04
 * @updated 27.10.05
 */

/* TODO: uncomment after JT refactoring:
:- module(punit, 
     [runTests/0, 
      runTestsStartingWith/1,
      test/1,
      setUp/1,
      tearDown/1]).
*/

:- multifile test/1.
:- multifile setUp/1.
:- multifile tearDown/1.

:- dynamic failed/0.

/**
 * runTests
 * 
 * Run all consulted
 * punit:test cases.
 * 
 */

runTests :- 
    runTestsStartingWith(_).

/**
 * runTestsStartingWith(+Prefix)
 * 
 * run all tests starting with Prefix
 */
runTestsStartingWith(Prefix):- 
        failed(FailedTests, Failed, All,Prefix),
		counter(0),
        writef("\n=======================\nfailed tests:\n"),
        write(FailedTests),
        plus(Failed,Success,All),
        writef("\n=======================\nsuccessful tests: (%d/%d)\n", [Success, All]).


runTestsStartingInDir(Prefix):- 
        failed_in_dir(FailedTests, Failed, All,Prefix),
		counter(0),
        writef("\n=======================\nfailed tests:\n"),
        write(FailedTests),
        plus(Failed,Success,All),
        writef("\n=======================\nsuccessful tests: (%d/%d)\n", [Success, All]).


/**
 * runTest(+Testname)
 * 
 * evaluate the test case Testname.
 */
runTest(Testname):-
	not(testfailed(Testname)).
        

/*************** internal ****************/

test_case(Name,Prefix):-
   var(Prefix),
   !,
   clause(test(Name), _).

test_case(Name,Prefix):-
   clause(test(Name),_), 
   term_to_atom(Name,Atom),
   atom_concat(Prefix,_,Atom).
   
   
test_case_in_dir(Name,Prefix):-
   var(Prefix),
   !,
   clause(test(Name), _).

test_case_in_dir(Name,Prefix):-
   clause(test(Name),_,Ref), 
   clause_property(Ref,file(File)),
   atom_concat(Prefix,_,File).
    
 
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


failed(_list, _length, _all,Prefix) :- findall(
                                    [_testname],
                                    (test_case(_testname,Prefix),
                                     testfailed(_testname)
                                    ), 
                                    _list
                                  ),
                                  length(_list, _length),
                                  findall([_test],
                                          test_case(_testname,Prefix),
                                          _tests
                                  ),
                                  length(_tests, _all).


failed_in_dir(_list, _length, _all,Prefix) :- findall(
                                    [_testname],
                                    (test_case_in_dir(_testname,Prefix),
                                     testfailed(_testname)
                                    ), 
                                    _list
                                  ),
                                  length(_list, _length),
                                  findall([_test],
                                          test_case_in_dir(_testname,Prefix),
                                          _tests
                                  ),
                                  length(_tests, _all).


assert_true(Goal) :-
    Goal \= ':'(_, _),
    punit_for_modules:active_module(Module),
    nonvar(Module),
    assert_true('', ':'(Module, Goal)), !.

assert_true(Goal) :-
    assert_true('', Goal), !.
/**
 * assert_true(+Comment, +Goal)
 * 
 * Checks if Goal is true, otherwise throws
 * exception assertion_failed('formated exception').
 */

assert_true(Comment, Goal) :-
  call(Goal)
    ->  true
    ; (
		prolog_current_frame(Frame),
		stack_for_frame_atom(Frame,StackTrace),
		!,
%		prolog_frame_attribute(Frame,parent,Parent),
%		stack_for_frame(Frame,Info),
		%term_to_atom(Info,StackTrace),
%		list_to_line_sep_string(Info,StackTrace),
		term_to_atom(Goal, AtomGoal),
    	sformat(FormattedComment, 'The goal ~n  ~a~n failed:~n~a.~n~a~n', [AtomGoal, Comment,StackTrace]),
   		throw(assertion_failed(FormattedComment))
   	  ), !.
   	  
   	  

/**
 * assert_fail(+Comment, +Goal)
 * 
 * Checks if Goal fails, otherwise throws
 * exception assertion_failed('formated exception').
 */
assert_fail(Goal) :-
    Goal \= ':'(_, _),
    punit_for_modules:active_module(Module),
    nonvar(Module),
    assert_fail('', ':'(Module, Goal)), !. 
  
assert_fail(Goal) :-
    assert_fail('', Goal), !.
    
assert_fail(Comment, Goal) :-
  (call(Goal)
    ->  (
		term_to_atom(Goal, AtomGoal),
    	sformat(FormattedComment, 'The goal ~n   ~a~n unexpectedly succeeded:~n~a.', [AtomGoal, Comment]),
   		throw(assertion_failed( FormattedComment))
   	);
   	true), !.


/**
 * assert_ground(+Comment, +Term)
 * assert_bound(+Comment, +Term) obsolete
 *
 * Checks if Term is ground (that is, contains no free
 * variables), otherwise throws
 * exception assertion_failed('formated exception').
 */

% obsolete:
assert_bound(         Term) :- assert_ground(         Term).
assert_bound(Comment, Term) :- assert_ground(Comment, Term).

% Use this instead:
assert_ground( Term):-
    assert_ground('',Term).
	
assert_ground(Comment, Term):-
	not(ground(Term)),
	% numbervars(Term,0,Num),
	% Num > 0,
    term_to_atom(Term,Atom),
    sformat(Msg, 'The term ~a contains variables.~n~a~n',[Atom,Comment]),
    write(Msg),
    flush_output,
    throw(assertion_failed(Msg)).
assert_ground(_,_).

/*
surr2:-
    testAssertTrue(A).

testAssertTrue('sadlfkj') :-
	assert_true('aha',fail).
*/

test_suite(Test) :-
    clause(test(Test),_).