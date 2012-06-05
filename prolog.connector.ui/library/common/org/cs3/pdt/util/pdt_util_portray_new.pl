:-use_module(library('org/cs3/pdt/util/pdt_term_pattern')).

:-op(1200,xfx,==>).

term_expansion((Head==>Body),Expansion):-
    term_pattern_expansion(Head,Body,Expansion).

repeat_fail(Operands,Remain) ==>
	$(repeat),
	repeat_fail_body(Operands,Remain).

repeat_fail_body([],true)==>
	$(!).	
repeat_fail_body([],Remain)==>
	$(!),
	#(Remain).
repeat_fail_body([Operand|Operands],Remain)==>
	#(Operand),
	repeat_fail_body(Operands,Remain).


/*	    
layout_vertical(A:-B):-
	horizontal(A),
	write(:-),
	nl,
	right,
	vertical(B).

layout_vertical((A,B)):-
    horizontal(A),
    write(','),
    nl,
    vertical(B).
    

layout_horizontal((repeat,B)):-
    layout_vertical((repeat,B)).
layout_vertical((repeat,B)):-
    write(repeat),
    write(','),
    nl,
    right,
    layout_until_cut(B,C),
    left,
    write(!),
    write(','),
    nl,
    vertical(C).


layout_until_cut((!,B),B):-!.
layout_until_cut(!,true):-!.
layout_until_cut(A,B,C):-
    horizontal(A),
	write(','),
	nl,	
	layout_until_cut(B).
    
   
horizontal(Term):-
	expand(Term,Expanded),
	layout_horizontal(Expanded),!.   


vertical(Term):-
	expand(Term,Expanded),
	layout_vertical(Expanded),!.   
  */ 
/*
writing a term takes is a recursive process.

for subterm s and current indention level i
	is there a layout hook? 
	  yes -> let it take over.
	  no -> get functor and arguments (hook!)
	        write functor.
	
	
','(
	repeat, 
	','(
		=(_, 1), 
		;(
			->(
				true, 
				!
			), 
			true
		)
	)
)


','(
	repeat, 
	','(
		=(_, 1), 
		;(
			->(
				','(
					true, 
					!
				), 
				true
			), 
			true
		)
	)
)

','(
	repeat, 
	','(
		=(_, 1), 
		;(
			->(
				!, 
				true
			), 
			true
		)
	)
)
	

*/