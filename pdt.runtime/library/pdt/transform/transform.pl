:- module(transform,
	[	pdt_check_conditions/3,
		pdt_perform_change/1,
		pdt_cleanup_transformation/1
	]
).

:- multifile check_conditions/3, perform_change/1.
:- use_module(library(pef/pef_base)).
:- use_module(library(builder/builder)).


pdt_check_conditions(Head,Severity,Message):-
    check_conditions(Head,Severity,Message0),
    (	string(Message0)
    ->	Message=Message0
    ;	string_to_list(Message,Message0)
    ).
pdt_check_conditions(_Head,info,String):-
	string_to_list(String,"Check complete.").
    
pdt_perform_change(Head):-
    pef_start_recording(Head,full),
    call_cleanup(
    	perform_change(Head),
    	pef_stop_recording
    ).    
    
pdt_cleanup_transformation(Head):-
	pef_clear_record(Head),
	pdt_invalidate_target(delta).    