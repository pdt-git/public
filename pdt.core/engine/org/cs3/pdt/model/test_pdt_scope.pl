:- module(test_pdt_scope,[]).

:- use_module(pdt_scope).

my_setup:-
    pdt_scope_new(testscope,S),
    assert(testscope(S)).
my_teardown:-
    retractall(testscope(S)).


    
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


test(local_resolve):-
    testscope(S),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar,barval,S2),
    pdt_scope_resolve(S2,foo,A),
    A==fooval.

test(remote_resolve_contrib):-
    testscope(S),
    pdt_scope_bind(S,foo,fooval,S1),
    pdt_scope_bind(S1,bar:bau,bauval,S2),
    pdt_scope_bind(S2,bar:baum,baumval,S3),
    pdt_scope_bind(S3,baz,bazval,S4),
    pdt_scope_resolve(S4,foo,Foo),
    Foo==fooval,
    \+ pdt_scope_resolve(S4,bar,_),    
    pdt_scope_resolve(S4,bar:bau,Bau),
    Bau==bauval,
    pdt_scope_resolve(S4,bar:baum,Baum),
    Baum==baumval,
    pdt_scope_resolve(S4,baz,Baz),
    Baz=bazval.


:- end_tests(scope).