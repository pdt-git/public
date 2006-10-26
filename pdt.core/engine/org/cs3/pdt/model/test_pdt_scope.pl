:- module(test_pdt_scope,[]).

:- use_module(pdt_scope).
:- use_module(pdt_namespace).
my_setup:-
    pdt_scope_new(testscope,S),
    assert(testscope(S)).
my_teardown:-
    retractall(testscope(_)).


    
:- begin_tests(scope,[setup(my_setup),cleanup(my_teardown)]).

test(local_bind):-
    testscope(S),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar,barval,_).

test(remote_bind):-
    testscope(S),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar:bau,bauval,S2),
    pdt_scope_bind(S2,bar:baum,baumval,S3),
    pdt_scope_bind(S3,baz,bazval,_).


test(local_resolve,[true(A==fooval)]):-
    testscope(S),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar,barval,S2),
    pdt_scope_resolve(S2,foo,A).

test(remote_resolve_contrib,[true(fooval-bauval-baumval-bazval==Foo,Bau,Baum,Baz)]):-
    testscope(S),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar:bau,bauval,S2),
    pdt_scope_bind(S2,bar:baum,baumval,S3),
    pdt_scope_bind(S3,baz,bazval,S4),
    pdt_scope_resolve(S4,foo,Foo),
    \+ pdt_scope_resolve(S4,bar,_),    
    pdt_scope_resolve(S4,bar:bau,Bau),
    pdt_scope_resolve(S4,bar:baum,Baum),
    pdt_scope_resolve(S4,baz,Baz).

test(remote_resolve_contrib_inherit_stack,[true(fooval-bauval-baumval-bazval-BarNs1==Foo,Bau,Baum,Baz,Bar)]):-
    testscope(S),
    pdt_namespace_new(bar_nsid,BarNs0),
    pdt_namespace_bind(BarNs0,baum,baumval,BarNs1),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar:bau,bauval,S2),
    pdt_scope_bind(S2,bar,BarNs1,S3),
    pdt_scope_bind(S3,baz,bazval,S4),
    pdt_scope_resolve(S4,foo,Foo),  
    pdt_scope_resolve(S4,bar:bau,Bau),
    pdt_scope_resolve(S4,bar,Bar),
    pdt_scope_resolve(S4,bar:baum,Baum),
    pdt_scope_resolve(S4,baz,Baz).

test(remote_resolve_contrib_inherit_heap,[true(fooval-bauval-baumval-bazval-BarNs1==Foo,Bau,Baum,Baz,Bar)]):-
    testscope(S),
    pdt_namespace_new(bar_nsid,BarNs0),
    pdt_namespace_bind(BarNs0,baum,baumval,BarNs1),
    pdt_namespace_store(BarNs1),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar:bau,bauval,S2),
    pdt_scope_bind(S2,bar,bar_nsid,S3),
    pdt_scope_bind(S3,baz,bazval,S4),
    pdt_scope_resolve(S4,foo,Foo),  
    pdt_scope_resolve(S4,bar:bau,Bau),
    pdt_scope_resolve(S4,bar,Bar),
    pdt_scope_resolve(S4,bar:baum,Baum),
    pdt_scope_resolve(S4,baz,Baz).


test(remote_resolve_contrib_overide_stack,[true(fooval-bauval-otherbaumval-bazval-BarNs1==Foo,Bau,Baum,Baz,Bar)]):-
    testscope(S),
    pdt_namespace_new(bar_nsid,BarNs0),
    pdt_namespace_bind(BarNs0,baum,baumval,BarNs1),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar:bau,bauval,S2),
    pdt_scope_bind(S2,bar,BarNs1,S3),
    pdt_scope_bind(S3,baz,bazval,S4),
    pdt_scope_bind(S4,bar:baum,otherbaumval,S5),
    pdt_scope_resolve(S5,foo,Foo),  
    pdt_scope_resolve(S5,bar:bau,Bau),
    pdt_scope_resolve(S5,bar,Bar),
    pdt_scope_resolve(S5,bar:baum,Baum),
    pdt_scope_resolve(S5,baz,Baz).



:- end_tests(scope).