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

:- module(test000,[test000/1]).
check_node(Id,Type,Label,Props):-        
      assert_true(node(Id,Type,Label)),
      properties(Id,Props).

check_edge(Id,Type,Label,From,To):-
    assert_true(edge(Id,Type,Label,From,To)).
properties(_,[]).
properties(Id,[Prop|PTail]):-  
	assert_true(property(Id,Prop)),      
    properties(Id,PTail).


%% the following is part of jtransformer. i copy it here as i am to 
%% lazy to think of an other solution right now.
% Author: Tobias
% Date: 06.02.2003

:- dynamic errorhandling/0.
:- dynamic halt_on_error/0.

% temporary necessary while aspects are not 
% completely represented in prolog factbase:
:- dynamic slAspectT/4. 
:- multifile slAspectT/4.

%
% from error_handling.pl:
%

/**
 * error_handling_add_error_term(+SourceLocation, +Goal, +Message)
 *
 * SourceLocation: sourceLocation(+File, +Start, +Length)
 *                 use the predicate tree_source_location/2 
 *                 to retrieve the source location.
 */

error_handling_add_error_term(_,_term, _) :-
    call(_term),
    !.
error_handling_add_error_term(SourceLocation,_, Err) :-
    %term_to_atom(_term,_err),
    sformat(ErrorString, 'err ~w',[Err]),
    write(ErrorString),
    addErrorFacts(SourceLocation,Err),  
    flush_output,
    debugme,
    halt_on_error ->
	    halt;
	    throw(ErrorString).
       
error_handling(_term, _,_) :-
    call(_term),
    !.
error_handling(_,_format,_term) :-
    term_to_atom(_term,_atom),
    sformat(_err,_format,[_atom]),
    error_handling_add_error_term(null,fail,_err).


/**
 * tree_source_location(+Tree,?sourceLocation(Path, Begin,Length))
 *
 * Bind sourceLocation/3 fact for tree.
 */

tree_source_location(Tree,sourceLocation(Path, Begin,Length)):-
	slT(Tree,Begin,Length), 
	getToplevel(Tree,TL),
	toplevelT(TL,_,Path,_).


addErrorFacts(null,_).
addErrorFacts(sourceLocation(File, Start, Length),Err):-
   new_id(ID),
   add(isErrorWarningMessage('declare error', ID, Err)), 
   add(slAspectT(ID, File,Start, Length)).


/**
 * Public API
 *
 * stack_for_frame_atom(+Frame,-StackTraceAtom)
 *
 * will bind StackTraceAtom to an linefeed separated list
 * of stack trace elements.
 *
 * use prolog_current_frame(Frame) 
 * to retrieve current frame.
 */

stack_for_frame_atom(Frame,StackTrace):-
    stack_for_frame(Frame,List),
    list_to_line_sep_string(List, StackTrace).

/**
 * Public API
 *
 * stack_for_frame(+Frame,-StackTraceList)
 *
 * use prolog_current_frame(Frame) to retrieve current frame.
 */
   
stack_for_frame(Frame,[]):-
    prolog_frame_attribute(Frame,top,true),
    !.
    
stack_for_frame(Frame,[Info|Stack]) :-
    frame_info(Frame,Info),
    prolog_frame_attribute(Frame,parent,Parent),
	stack_for_frame(Parent,Stack).

stack_for_frame(_,['stack inspection failed']):-
    !.

    
frame_info(Frame,Info):-
    prolog_frame_attribute(Frame,clause,Ref),
    prolog_frame_attribute(Frame,level,Level),
    prolog_frame_attribute(Frame,goal,Goal),
    term_to_atom(Goal, GoalAtom),
%    nth_clause(Pred,_,Ref),
    clause_property(Ref,file(File)),
    clause_property(Ref,line_count(Line)),
%    functor(Pred,Name,Arity),
%    term_to_atom(Pred,Atom),
%    write(Atom),
    sformat(Info,'~a:~a ~a level: ~a~n',[File,Line,GoalAtom,Level]).
       
       
list_to_line_sep_string([],'').

list_to_line_sep_string([Head|Tail],String):-
	list_to_line_sep_string(Tail,StringTail),
	sformat(String,'~a~n~a',[Head,StringTail]).


%
% from test/tests.pl
%       
% Author: Tobias
% Date: 31.07.04

:- multifile test/1.
:- multifile setUp/1.
:- multifile tearDown/1.

:- dynamic failed/0.

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

Use assert_true/2, assert_fail/2 and assert_bound/2 for
your test cases to check your predicates.

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
   	  ).
   	  
   	  

/**
 * assert_fail(+Comment, +Goal)
 * 
 * Checks if Goal fails, otherwise throws
 * exception assertion_failed('formated exception').
 */
  
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


/**
 * assert_bound(+Comment, +Term)
 * 
 * Checks if Term is bound, otherwise throws
 * exception assertion_failed('formated exception').
 */

assert_bound( Term):-
    assert_bound('',Term).
	
assert_bound(Comment, Term):-
	numbervars(Term,0,Num),
	Num > 0,
    term_to_atom(Term,Atom),
    sformat(Msg, 'The term ~a contains variables.~n~a~n',[Atom,Comment]),
    write(Msg),
    flush_output,
    throw(assertion_failed(Msg)).
assert_bound(_,_).

/*
surr2:-
    testAssertTrue(A).

testAssertTrue('sadlfkj') :-
	assert_true('aha',fail).
*/

test_suite(Test) :-
    clause(test(Test),_).
    
node(node_id(0),predicate,user:rumpel/1).
node(node_id(1),clause,rumpel/1).
edge(edge_id(2),clause,null,node_id(0),node_id(1)).
property(node_id(1),position(0,127)).
property(node_id(1),file(test000)).
node(node_id(3),literal,forall/2).
edge(edge_id(4),body_literal,null,node_id(1),node_id(3)).
property(node_id(3),position(17,98)).
node(node_id(5),literal,pumpel/1).
edge(edge_id(6),body_literal,null,node_id(1),node_id(5)).
property(node_id(5),position(31,9)).
node(node_id(7),literal,'='/2).
edge(edge_id(8),body_literal,null,node_id(1),node_id(7)).
property(node_id(7),position(50,3)).
node(node_id(9),literal,writeln/1).
edge(edge_id(10),body_literal,null,node_id(1),node_id(9)).
property(node_id(9),position(63,13)).
node(node_id(11),literal,writeln/1).
edge(edge_id(12),body_literal,null,node_id(1),node_id(11)).
property(node_id(11),position(85,15)).
node(node_id(13),literal,true/0).
edge(edge_id(14),body_literal,null,node_id(1),node_id(13)).
property(node_id(13),position(122,4)).
test000(Filename):-
check_node(node_id(0),predicate,user:rumpel/1,[]),
check_node(node_id(1),clause,rumpel/1,[file(Filename),position(0,127)]),
check_edge(_,clause,_,0,1),
check_node(node_id(3),literal,forall/2,[position(17,98)]),
check_edge(_,body_literal,_,1,3),
check_node(node_id(5),literal,pumpel/1,[position(31,9)]),
check_edge(_,body_literal,_,1,5),
check_node(node_id(7),literal,(=)/2,[position(50,3)]),
check_edge(_,body_literal,_,1,7),
check_node(node_id(9),literal,writeln/1,[position(63,13)]),
check_edge(_,body_literal,_,1,9),
check_node(node_id(11),literal,writeln/1,[position(85,15)]),
check_edge(_,body_literal,_,1,11),
check_node(node_id(13),literal,true/0,[position(122,4)]),
check_edge(_,body_literal,_,1,13).
