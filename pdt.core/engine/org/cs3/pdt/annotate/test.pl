


/*
:- use_module(library('org/cs3/pdt/util/pdt_util_cs')).
cscs(In,Out):-
	pdt_cs(CS),
	pdt_cs_subterm(CS,T),
	pdt_cs_condition(CS,
		(	fail
		)
	),
	pdt_cs_substitution(CS,TT),
	pdt_cs_apply(In,CS,Out).
*/







    


prolog_exception_hook(E, E, Frame, 	_CatcherFrame):-
 	stack_trace(Frame,StackTrace),
 	stack_trace_lines(StackTrace,Lines),
 	print_stack_trace_lines(Lines).

print_stack_trace_lines([]).
print_stack_trace_lines([Format-Args|Lines]):-
    format(Format,Args),
   	print_stack_trace_lines(Lines).
   	

stack_trace(Frame,[F|Fs]):-
	first_source_frame(Frame,F),
	stack_trace_continue(F,Fs).
	
stack_trace_continue(F,[]):-	
	\+ prolog_frame_attribute(F,parent,_Parent),
	!.
stack_trace_continue(F,Fs):-
    prolog_frame_attribute(F,parent,Parent),
	stack_trace(Parent,Fs).
	
first_source_frame(Frame,First):-
    prolog_frame_attribute(Frame,hidden,true),
    !,
    prolog_frame_attribute(Frame,parent,Next),
    first_source_frame(Next,First).
first_source_frame(Frame,Frame).

/*
print_stack_trace([]).
print_stack_trace([Frame|Frames]):- 
	format("from ~p ~w:~w~n",[Head,File,Line]),
	print_stack_trace(Frames).
	    
*/
stack_trace_lines([],[]).
stack_trace_lines([Frame|Frames], [Line|Lines]):-    
	(	prolog_frame_attribute(Frame,clause,Ref)
	->	
		( 	clause_property(Ref,file(File))
		;	File='no file'
		),
		!,
		
		
	    (	clause_property(Ref,line_count(LineNo))
	    ;	LineNo='no line'
	    ),
	    !,
	    
		%prolog_frame_attribute(Frame,goal,Goal),	    
	    clause(Head,_,Ref),
	    

	    Line="from ~p ~w:~w~n"-[Head,File,LineNo]
	;	Line="no info"-[]
	

	),
		Line=Format-Args,
		format(Format,Args),
	stack_trace_lines(Frames,Lines).

print_stack_trace(_Stream,[]).
print_stack_trace(Stream,[Frame|Frames]):-
	prolog_frame_attribute(Frame,clause,Ref),
	clause_property(Ref,file(File)),
    clause_property(Ref,line_count(Line)),
    clause(Head,_,Ref),
    format(Stream, "~t,from ~p ~w:~w~n",[Head,File,Line]),
    print_stack_trace(Stream,Frames).