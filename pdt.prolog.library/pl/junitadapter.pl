:-module(junitadapter, []).

:- use_module(library(plunit)).

unit_test(UnitName,Name):-
    plunit:current_test_set(UnitName),
    plunit:unit_from_spec(_, UnitName, Tests, Module, _),
    Module:'unit test'(Name, _, _, _), plunit:matching_test(Name, Tests).

unit_test(UnitName,Name,File,Line):-
    plunit:current_test_set(UnitName),
    plunit:unit_from_spec(_, UnitName, Tests, Module, _),
    current_module(Module,File),
    Module:'unit test'(Name, Line,_, _), 
    plunit:matching_test(Name, Tests).
	
	
/*
	junit_adapter(+TestName,-ResultKind,-Comment,-File,-Line)
	
	see exception_kind/3 for result kinds
*/

junit_adapter(TestName,ResultKind,Comment):-
	catch(test(TestName), TestException, true),
	exception_kind(TestException,ResultKind,Comment).
	
	
/*
	exception_kind(+Exception,-Kind,-Comment)
	
	-Kind
		test succeeded:   'true'
		test failed:      'fail'
		thrown exception: 'exception'
*/

exception_kind(TestException,true,''):-
    var(TestException),
    !.

exception_kind(TestException,fail,TestComment):-
	TestException=assertion_failed(TestComment),
    !.

exception_kind(TestException,exception,Message):-
    message_to_string(TestException,MessageString),
    string_to_atom(MessageString,Message).

file_information(TestName,File,Line):-
%    nth_clause(test(TestName),_,Ref),
    clause(test(TestName),_,Ref),
	clause_property(Ref,file(File)),
	clause_property(Ref,line_count(Line)).

file_information(TestName,__File,__Line):-
    sformat(Msg, ' no test case ''~w'' defined in the factbase.',[TestName]),
    throw(Msg). 
    


%mypred2(Info):-
%	prolog_current_frame(Frame),
%	stack_for_frame(Frame,Info).


