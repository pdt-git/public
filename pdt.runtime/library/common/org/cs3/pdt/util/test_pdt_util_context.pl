% unit tests for module pdt_util_context
:- module(test_pdt_util_context, []).

:- use_module(pdt_util_context).

my_setup:-
    pdt_define_context(supi(laber,rababer,schwaetz,schwadronier)).
my_teardown:-
    pdt_undefine_context(supi).


    
:- begin_tests(context,[setup(my_setup),cleanup(my_teardown)]).

    
    
test(constructor):-
	supi_new(T),
	functor(T,supi,4),
	forall(arg(_,T,Var),var(Var)).    
	
test(getter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwaetz(T,schwaetz),	
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwaetz(T,schwaetz).

test(setter):-
    supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwaetz(T,schwaetz),	
	supi_set_laber(T,laber2,T2),
	supi_set_schwaetz(T2,schwaetz2,T3),
	supi_set_schwadronier(T3,schwadronier2,T4),
	supi_laber(T4,laber2),
	supi_schwadronier(T4,schwadronier2),
	supi_schwaetz(T4,schwaetz2).

test(setter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwaetz(T,schwaetz),	
	supi_set_rababer(T,Var,T2),
	supi_rababer(T2,Var2),
	var(Var2),
	Var2==Var.

test(multi_setter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwaetz(T,schwaetz),	
	supi_set(T,[laber=laber2,schwaetz=schwaetz2],T2),
	supi_laber(T2,laber2),
	supi_rababer(T2,rababer),
	supi_schwadronier(T2,schwadronier),
	supi_schwaetz(T2,schwaetz2).


test(multi_getter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwaetz(T,schwaetz),	
	supi_get(T,[rababer=rababer,schwaetz=schwaetz,laber=A]),
	A==laber.    

:- end_tests(context).