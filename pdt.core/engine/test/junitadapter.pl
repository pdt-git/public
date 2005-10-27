
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


%mypred2(Info):-
%	prolog_current_frame(Frame),
%	stack_for_frame(Frame,Info).


