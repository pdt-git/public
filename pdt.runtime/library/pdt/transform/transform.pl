:- module(transform,
	[	pdt_interprete_selection/3,
		pdt_perform_transformation/2,
		pdt_rollback_transformation/1,
		pdt_transformation_problem/3
	]
).

:- multifile interprete_selection/3, perform_transformation/2.
:- use_module(library('pef/pef_base')).
:- use_module(library('builder/builder')).


pdt_interprete_selection(Refactoring,Selection,Params):-
    interprete_selection(Refactoring,Selection,Params).

pdt_perform_transformation(Refactoring,Params):-
    %TODO: push transaction 
    perform_transformation(Refactoring,Params).

pdt_rollback_transformation(_Refactoring).
	%TODO: pop and rollback transaction.

pdt_transformation_problem(Id,Severity,Message):-
	pef_transformation_problem_query([id=Id,severity=Severity,message=Msg]),
	(	string(Msg)
	->	Msg=Message
	;	string_to_list(Message,Msg)
	).
