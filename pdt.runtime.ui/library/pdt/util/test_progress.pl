:- use_module(progress).
:- use_module(library('facade/pdt_workspace')).
:- use_module(library('builder/targets/parse')).
:- use_module(library('builder/builder')).

progress:estimate_hook(task_A,task_A1,1).
progress:estimate_hook(task_A,task_A2,10).
progress:estimate_hook(task_A,task_A3,5).
progress:estimate_hook(task_A2,task_A2a,1).
progress:estimate_hook(task_A2,task_A2b,2).


progress:estimate_hook(parse_workspace,Key,1):-
    pdt_contains_star(workspace,file(Path)),
    pdt_builder:target_key(parse(file(Path)),Key),
    \+ pdt_builder:available(Key).

progress:report_progress_hook(parse_workspace,Key,Work):-
    !,
    pdt_builder:target_key(parse(file(Path)),Key),
    format("parsed: ~w, units:~w~n",[Path,Work]).
progress:report_progress_hook(Task,SubTask,Work):-
    format("Progress for ~w: subtask ~w complete, units:~w~n",[Task,SubTask,Work]).

test_me:-
	pdt_with_progress(task_A,
	 	(	pdt_report_progress(task_A1),
	 		pdt_with_progress(task_A2,
				(	pdt_report_progress(task_A2a),
					pdt_report_progress(task_A2b),
					pdt_report_progress(task_A3)
				)
			),
			pdt_report_progress(task_A3)
		)
	).

test_me2:-
    pdt_with_progress(parse_workspace,pdt_request_target(parse(workspace))).