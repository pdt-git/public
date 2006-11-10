:- module(test_util_unification,[]).

:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_source_term')).
:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).

:- use_module(library('org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('org/cs3/pdt/annotate/op_annotator')).

:- dynamic test_input_file/1.

:- prolog_load_context(file,File),assert(test_input_file(File)).


input(succeed):-
    _A=_B,
    [var(37,0)=var(37,1)].
input(succeed):-
    _A=check(this,out),
    [var(52,0)=term(57)].
input(succeed):-
    check(_A,out)=check(this,out),
    [var(68,0)=term(67)].
input(succeed):-
    check(A,out)=check(out,A),
    [var(68,0)=term(67)].

test_input(A,B,Bindings,Outcome):-
    test_input_file(File),
	pdt_file_term(File,Term),
	source_term_property(Term,clause_of,_:input/1),
	source_term_arg(1,Term,Head),
	source_term_arg(2,Term,Body),
	source_term_arg(1,Head,OutcomeTerm),
	source_term_expand(OutcomeTerm,Outcome),
	source_term_arg(1,Body,Unification),
	source_term_arg(1,Unification,A),
	source_term_arg(2,Unification,B),
	source_term_arg(2,Body,BindingsTerm),
	source_term_expand(BindingsTerm,Bindings).
	
do_test(fail,A,B,_Bindings):-
    \+ pdt_unifiable(A,B,_).
do_test(succeed,A,B,Bindings):-
    pdt_unifiable(A,B,Unifier),
	pdt_multimap_findall(Node,Term,	pdt_bound(Unifier,Node,Term),UnifierBindings),
    check_bindings(Bindings,UnifierBindings).

check_bindings([],Unifier):-
    !,
    pdt_multimap_empty(Unifier).
check_bindings([Node=Term|Bindings],Unifier):-
    pdt_multimap_remove(Unifier,Node,Term,NextUnifier),
    check_bindings(Bindings,NextUnifier).
	   
	
my_setup:-
    test_input_file(File),
    pdt_ensure_annotated(File).
    
my_teardown/*:-
	test_input_file(File),
    pdt_forget_annotation(File)*/.
    
:- begin_tests(unification,[setup(my_setup),cleanup(my_teardown)]).

test(unification_tests):-
    forall(
    	test_input(A,B,Bindings,Outcome),
    	do_test(Outcome,A,B,Bindings)
    ).
:- end_tests(unification).    