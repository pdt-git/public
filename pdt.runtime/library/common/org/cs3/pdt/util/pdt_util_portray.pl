:-module(pdt_util_portray,[portray_clause/1]).

portray_clause(Term) :-
	current_output(Out),
	portray_clause(Out, Term).

portray_clause(Stream, Term) :-
	\+ \+ ( numbervars(Term, 0, _, [singletons(true)]), 
		do_portray_clause(Stream, Term)
	      ).

do_portray_clause(Out, (Head :- true)) :- !, 
	portray_head(Out, Head), 
	put(Out, 0'.), nl(Out).
do_portray_clause(Out, (Head :- Body)) :- !, 
	inc_indent(0, 1, Indent),
	portray_head(Out, Head), 
	write(Out, ' :-'), 
	(   nonvar(Body),
	    Body = Module:LocalBody
	->  nlindent(Out, Indent),
	    format(Out, '~q:', [Module]),
	    nlindent(Out, Indent),
	    write(Out, '(   '),
	    portray_body(LocalBody, 2, noindent, Out),
	    nlindent(Out, Indent),
	    write(Out, ')')
	;   inc_indent(0, 2, BodyIndent),
	    portray_body(Body, BodyIndent, indent, Out)
	),
	put(Out, 0'.), nl(Out).
do_portray_clause(Out, (:-use_module(File, Imports))) :-
	length(Imports, Len),
	Len > 3, !,
	format(Out, ':- use_module(~q,', [File]),
	portray_list(Imports, 14, Out),
	write(Out, ').\n').
do_portray_clause(Out, (:-module(Module, Exports))) :- !,
	format(Out, ':- module(~q,', [Module]),
	portray_list(Exports, 10, Out),
	write(Out, ').\n').
do_portray_clause(Out, (:-Directive)) :- !,
	write(Out, ':- '), 
	portray_body(Directive, 3, noindent, Out),
	write(Out, '.\n').
do_portray_clause(Out, Fact) :-
	do_portray_clause(Out, (Fact :- true)).

portray_head(Out, Head) :-
	pprint(Out, Head).

%	portray_body(+Term, +Indent, +DoIndent, +Out)
%	
%	Write Term at current indentation. If   DoIndent  is 'indent' we
%	must first call nlindent/2 before emitting anything.

portray_body(!, _, _, Out) :- !, 
	write(Out, ' !').
portray_body((!, Clause), Indent, _, Out) :- !, 
	write(Out, ' !,'), 
	portray_body(Clause, Indent, indent, Out).
portray_body(Term, Indent, indent, Out) :- !, 
	nlindent(Out, Indent), 
	portray_body(Term, Indent, noindent, Out).
portray_body((A, B), Indent, _, Out) :- !, 
	portray_body(A, Indent, noindent, Out), 
	write(Out, ','), 
	portray_body(B, Indent, indent, Out).
portray_body(Or, Indent, _, Out) :-
	memberchk(Or, [(_;_), (_|_), (_->_), (_*->_)]), !, 
	write(Out, '(   '), 
	portray_or(Or, Indent, Out), 
	nlindent(Out, Indent), 
	write(Out, ')').
portray_body(Meta, Indent, _, Out) :-
	meta_call(Meta, N), !, 
	portray_meta(Out, Meta, N, Indent).
portray_body(Clause, _, _, Out) :-
	pprint(Out, Clause).

portray_or((If -> Then ; Else), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	portray_body(If, NestIndent, noindent, Out), 	
	nlindent(Out, Indent),
	write(Out, '->  '), 
	portray_body(Then, NestIndent, noindent, Out), 
	nlindent(Out, Indent), 
	write(Out, ';   '), 
	portray_or(Else, Indent, Out).
portray_or((If *-> Then ; Else), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	portray_body(If, NestIndent, noindent, Out), 	
	nlindent(Out, Indent),
	write(Out, '*-> '), 
	portray_body(Then, NestIndent, noindent, Out), 
	nlindent(Out, Indent), 
	write(Out, ';   '), 
	portray_or(Else, Indent, Out).
portray_or((If -> Then), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	portray_body(If, NestIndent, noindent, Out), 	
	nlindent(Out, Indent), 
	write(Out, '->  '), 
	portray_or(Then, Indent, Out).
portray_or((If *-> Then), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	portray_body(If, NestIndent, noindent, Out), 	
	nlindent(Out, Indent), 
	write(Out, '*-> '), 
	portray_or(Then, Indent, Out).
portray_or((A;B), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	portray_body(A, NestIndent, noindent, Out), 
	nlindent(Out, Indent), 
	write(Out, ';   '), 
	portray_or(B, Indent, Out).
portray_or((A|B), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	portray_body(A, NestIndent, noindent, Out), 	
	nlindent(Out, Indent), 
	write(Out, '|   '), 
	portray_or(B, Indent, Out).
portray_or(A, Indent, Out) :-
	inc_indent(Indent, 1, NestIndent),
	portray_body(A, NestIndent, noindent, Out).

meta_call(call(_), 1).
meta_call(once(_), 1).
meta_call(not(_), 1).
meta_call(\+(_), 1).
meta_call(ignore(_), 1).

portray_meta(Out, Term, N, Indent) :-
	arg(N, Term, Arg), 
	memberchk(Arg, [(_, _), (_;_), (_->_), (_*->_)]), !, 
	functor(Term, Name, _), 
	write(Out, Name), write(Out, '(('), 
	inc_indent(Indent, 1, NestIndent),
	portray_body(Arg, NestIndent, indent, Out), 
	nlindent(Out, NestIndent), 
	write(Out, '))').	
portray_meta(Out, Term, _, _) :-
	pprint(Out, Term).	

%	portray_list(+List, +Indent, +Out)
%	
%	Portray a list list this.  Right side for improper lists
%	
%		[ element1,		[ element1
%		  element2,	OR	| tail
%		]			]

portray_list([], _, Out) :- !,
	write(Out, []).
portray_list(List, Indent, Out) :-
	nlindent(Out, Indent),
	write(Out, '[ '),
	EIndent is Indent + 2,
	portray_list_elements(List, EIndent, Out),
	nlindent(Out, Indent),
	write(Out, ']').

portray_list_elements([H|T], EIndent, Out) :-
	pprint(Out, H),
	(   T == []
	->  true
	;   nonvar(T), T = [_|_]
	->  write(Out, ','),
	    nlindent(Out, EIndent),
	    portray_list_elements(T, EIndent, Out)
	;   Indent is EIndent - 2,
	    nlindent(Out, Indent),
	    write(Out, '| '),
	    pprint(Out, T)
	).

%	nlindent(+Out, +Indent)
%	
%	Write newline and indent to column Indent.

nlindent(Out, N) :-
	nl(Out),
	Tab is N // 8, 
	Space is N mod 8,
	put_tabs(Out, Tab),
	tab(Out, Space).

put_tabs(Out, N) :-
	N > 0, !,
	put(Out, 0'\t),
	NN is N - 1,
	put_tabs(Out, NN).
put_tabs(_, _).


%	inc_indent(+Indent0, +Inc, -Indent)
%	
%	Increment the indent with logical steps.

inc_indent(Indent0, Inc, Indent) :-
	Indent is Indent0 + Inc*4.

%	pprint(+Out, +Term)
%	
%	Print Term such that it can be read.

pprint(Out, Term) :-
	write_term(Out, Term,
		   [ quoted(true),
		     numbervars(true)
		   ]).
