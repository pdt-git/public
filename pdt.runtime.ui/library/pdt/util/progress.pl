:- module(progress,
	[	pdt_report_progress/1,
		pdt_with_progress/2

	]
).


:- multifile report_progress_hook/3,estimate_hook/3,report_start_hook/2,report_done_hook/1.
:- dynamic '$subtask'/3.
:- dynamic '$thread_local'/3.

:- module_transparent pdt_with_progress/2.
pdt_with_progress(Task,Goal):-
    progress:push_task(Task),
    call_cleanup(
    	Goal,
    	progress:pop_task(Task)
    ).

pdt_report_progress(Subtask):-
	report_progress(Subtask,1).    
    
push_task(Task):-
	findall(
		r(Task,SubTask,Units),
		estimate_hook(Task,SubTask,Units),
		Rs			
	),	
	calculate_total(Rs,Total),
	ignore(report_start_hook(Task,Total)),
	forall(member(r(Task,SubTask,Units),Rs), asserta('$subtask'(SubTask,Task,Units/Total))).

pop_task(Task):-
    ignore(report_done_hook(Task)),
	repeat,
		(	clause('$subtask'(_,Task2,_),_,Ref)
		->	(	Task2==Task
			->	erase(Ref),
				fail
			;	true
			)
		;	true
		),
	!.

calculate_total(Rs,Total):-
    calculate_total(Rs,0,Total).
    
calculate_total([],Sum0,Sum0).
calculate_total([r(_,_,Units)|Rs],Sum0,Sum):-
    Sum1 is Sum0 + Units,
    calculate_total(Rs,Sum1,Sum).

    

report_progress(SubTask,Work):-
    (	Work==1
    ->	(	retract('$subtask'(SubTask,Task,Units/Total))
		->	ignore(report_progress_hook(Task,SubTask,Units * Work )),		
			report_progress(Task, (Units/Total) * Work)
		;	true
		)
    ;	(	'$subtask'(SubTask,Task,Units/Total)
		->	ignore(report_progress_hook(Task,SubTask,Units * Work )),		
			report_progress(Task, (Units/Total) * Work)
		;	true
		)
	).