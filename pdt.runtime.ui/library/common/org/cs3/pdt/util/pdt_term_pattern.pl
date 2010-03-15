:- module(pdt_term_pattern,[
	pdt_term_pattern_match/2,
	pdt_input_arg/3,
	pdt_input_functor/3,
	pdt_input_expand/2,
	pdt_input_annotation/2
	]).

:- module_transparent pdt_input_arg/3,
	pdt_input_functor/3,
	pdt_input_expand/2,
	pdt_input_annotation/2.
	
pdt_input_arg(Input,N,Arg):-
    context_module(Mod),
    input_arg(Input,Mod,N,Arg).

pdt_input_functor(Input,Name,Arity):-
    context_module(Mod),
    input_functor(Input,Mod,Name,Arity).

pdt_input_expand(Input,Expansion):-
    context_module(Mod),
    input_expand(Input,Mod,Expansion).

pdt_input_annotation(Input,Annotation):-
    context_module(Mod),
    input_annotation(Input,Mod,Annotation).

term_pattern_expansion(Head,Body,Transformed):-
    %source_location(File,Line),
    transform(Head,Body,Transformed).

/*
	a rule matches a term if
		-its principle functor matches that of the term.
		-the rule arguments match the term arguments.
	functor may be
		{Goal} matches if Goal succeeds. No further matching of the arguments is done.
		$(Data) Data is unified with the (expanded) Input. No further matching of the arguments is done.
		§(Annos) Data is unified with the anootations of Input. No further matching of the arguments is done.
		<any other term> matches if principal functor matches that of the (expanded) input.

	arguments may be
		{Goal}	matches if Goal succeeds
		(A,B)	matches if both A and B match 
		(A;B) 	matches if A or B match.
		$(Data)  Data is unified with the (expanded) argument of the term at the corresponding position,
		§(Annos) Annos is unified with the annotations of the argument.
		<any other literal> interpreted as another rule.
*/


:-module_transparent pdt_term_pattern_match/2.

pdt_term_pattern_match(Pattern,Input):-
    context_module(Module),
    pdt_term_pattern:add_argument(Pattern,Input,Pattern1),
    pdt_term_pattern:add_argument(Pattern1,Module,Pattern2),
    call(Module:Pattern2).
    


transform(Head,Body,(TTHead:-TBody)):-
    add_argument(Head,Input,THead),
    add_argument(THead,Module,TTHead),
    transform_body(Body,Input,Module,TBody).


transform_body($(Data),Input,Module,(pdt_term_pattern:functor_match($(Data),Input,Module))):-
    !.
transform_body(Pattern,Input,Module,(pdt_term_pattern:functor_match(Pattern,Input,Module))):-
    functor(Pattern,Pattern,0),
    !.
transform_body(Pattern,Input,Module,(pdt_term_pattern:functor_match(Pattern,Input,Module),ArgMatcher)):-
	functor(Pattern,_,Arity),
    transform_args(Pattern,Input,Module,1,Arity,ArgMatcher).

transform_args(Pattern,Input,Module,M,M,pdt_term_pattern:arg_match(PatternArg,Input,Module,M)):-
    !,
    arg(M,Pattern,PatternArg).
transform_args(Pattern,Input,Module,N,M,(pdt_term_pattern:arg_match(PatternArg,Input,Module,N),ArgMatcher)):-    
	arg(N,Pattern,PatternArg),
	O is N + 1,
	transform_args(Pattern,Input,Module,O,M,ArgMatcher).
	

functor_match($(Data),Input,Module):-
    !,
    input_expand(Input,Module,Data).
functor_match(Pattern,Input,Module):-
    input_functor(Input,Module,F,A),
    functor(Pattern,F,A).
    
arg_match(Pattern,Input,Module,N):-
	input_arg(Input,Module,N,Arg),
	arg_match(Pattern,Arg,Module).

arg_match($(Data),Input,Module):-
    !,
    input_expand(Input,Module,Data).
arg_match(a(Data),Input,Module):-
    !,
    input_annotation(Input,Module,Data).
arg_match(#(Input),Input,_Module):-
	!.
arg_match(Pattern,Input,Module):-
	add_argument(Pattern,Input,Pattern1),
	add_argument(Pattern1,Module,Pattern2),
    call(Pattern2).



input_functor(Input,Module,Name,Arity):-
    Module:current_predicate(input_functor_hook/3),
    Module:input_functor_hook(Input,Name,Arity),
    !.
input_functor(Input,_Module,Name,Arity):-
    functor(Input,Name,Arity).

input_arg(Input,Module,N,Arg):-
    Module:current_predicate(input_arg_hook/3),
    Module:input_arg_hook(Input,N,Arg),
    !.
input_arg(Input,_Module,N,Arg):-
	arg(N,Input,Arg).
	
input_annotation(Input,Module,Annotation):-
    Module:current_predicate(input_annotation_hook/2),
	Module:input_annotation_hook(Input,Annotation),
	!.	
input_annotation(_Input,_Module,[]).

input_expand(Input,Module,Expanded):-
    Module:current_predicate(input_expand_hook/2),
    Module:input_expand_hook(Input,Expanded),
    !.
input_expand(Input,_Module,Input).



add_argument(Input,Arg,Output):-
    functor(Input,Name,N),
	M is N+1,
    functor(Output,Name,M),
	unify_args(Input,Output,1,N),
    arg(M,Output,Arg).
    
unify_args(_Input,_Output,N,M):-
    N>M,
    !.
unify_args(Input,Output,N,M):-
	arg(N,Input,Arg),
	arg(N,Output,Arg),
	O is N + 1,
	unify_args(Input,Output,O,M).
