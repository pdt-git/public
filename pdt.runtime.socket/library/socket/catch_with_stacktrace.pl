:- module(with_stacktrace,
	[	call_cleanup_with_stacktrace/4,
		catch_with_stacktrace/4
	]
).

:- module_transparent call_cleanup_with_stacktrace/4,
		catch_with_stacktrace/4.
:- thread_local '$have_stacktrace'/1,'$stacktrace'/1.
:- dynamic '$have_stacktrace'/1,'$stacktrace'/1.

find_catcher_frame(CatcherGoal,Current,Frame):-
    prolog_frame_attribute(Current,goal,Goal),
    (	unifiable(Goal,CatcherGoal,_)
    ->	Frame=Current  
    ;	prolog_frame_attribute(Current,parent,Parent),
    	find_catcher_frame(CatcherGoal,Parent,Frame)
    ).
    
catch_with_stacktrace(Goal,Catcher,Stack,Handler):-    
	call_cleanup_with_stacktrace(Goal,exception(Catcher),Stack,Handler).
	
call_cleanup_with_stacktrace(Goal,Catcher,Stack,Cleanup):-    
	context_module(Cx),
	call_cleanup(
		with_stacktrace:my_call(Catcher,Frame,Cx:Goal),			
		Catcher,
		with_stacktrace:my_cleanup(Cx:Cleanup,Frame,Stack)
	).
	
my_call(Catcher,Frame,Goal):-
    prolog_current_frame(Current),
    G=system:setup_and_call_cleanup(_,_,Catcher,_),
	find_catcher_frame(G,Current,Frame),
	asserta('$have_stacktrace'(Frame)),
	Goal.	

my_cleanup(Cleanup,Frame,Stack):-
	retract('$have_stacktrace'(Frame)),
	ignore(retract('$stacktrace'(Stack))),	
	Cleanup.
	
user:prolog_exception_hook(E,E,Frame,CFrame):-
    my_exception_hook(Frame,CFrame).

my_exception_hook(Frame,CFrame):-        
    format("catcher goal is ~w~n",[Catcher]),
    (	with_stacktrace:'$have_stacktrace'(CFrame)
    ->	get_prolog_backtrace(Frame,50,Stack),
    	with_stacktrace:asserta('$stacktrace'(Stack))
    ),
    fail.
	