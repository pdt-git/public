% unit tests for module pdt_util_context
:- module(pdt_test_context, []).

:- use_module(pdt_util_context).

my_setup:-
    pdt_define_context(supi(laber,rababer,schwätz,schwadronier)).
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
	supi_schwätz(T,schwätz),	
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwätz(T,schwätz).

test(setter):-
    supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwätz(T,schwätz),	
	supi_set_laber(T,laber2,T2),
	supi_set_schwätz(T2,schwätz2,T3),
	supi_set_schwadronier(T3,schwadronier2,T4),
	supi_laber(T4,laber2),
	supi_schwadronier(T4,schwadronier2),
	supi_schwätz(T4,schwätz2).

test(setter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwätz(T,schwätz),	
	supi_set_rababer(T,Var,T2),
	supi_rababer(T2,Var2),
	var(Var2),
	Var2==Var.

test(multi_setter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwätz(T,schwätz),	
	supi_set(T,[laber=laber2,schwätz=schwätz2],T2),
	supi_laber(T2,laber2),
	supi_rababer(T2,rababer),
	supi_schwadronier(T2,schwadronier),
	supi_schwätz(T2,schwätz2).


test(multi_getter):-
	supi_new(T),
	supi_laber(T,laber),
	supi_rababer(T,rababer),
	supi_schwadronier(T,schwadronier),
	supi_schwätz(T,schwätz),	
	supi_get(T,[rababer=rababer,schwätz=schwätz,laber=A]),
	A==laber.    

:- end_tests(context).