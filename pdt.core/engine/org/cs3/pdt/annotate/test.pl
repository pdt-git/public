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

/*
prolog_exception_hook(error(Formal,context(Location,Message)), error(Formal,context(Location,(File:Line,Message))), Frame, _CatcherFrame):-
	writeln(gotcha),
	frame_clause(Frame,Ref),
	writeln(gotcha),
    clause_property(Ref,file(File)),
	writeln(gotcha),
    clause_property(Ref,line_count(Line)),
   	writeln(gotcha).    
   	
   	
frame_clause(Frame,Ref):-
	prolog_frame_attribute(Frame,clause,Ref),
	!.
frame_clause(Frame,Ref):-
	prolog_frame_attribute(Frame,parent,Parent),
	frame_clause(Parent,Ref).
	*/