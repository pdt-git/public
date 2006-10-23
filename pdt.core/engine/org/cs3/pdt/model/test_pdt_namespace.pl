:- module(test_pdt_namespace,[]).

:- use_module(pdt_namespace).

my_setup:-
    pdt_namespace_new(testns,NS),
    assert(myns(NS)).
my_teardown:-
    retractall(myns(_)),
    pdt_namespace_unstore(bar_nsid).

    
my_reset:-
	flag('my_count$counter',_,0).
my_count(_):-
    flag('my_count$counter',N,N+1),
    fail.
my_count(N):-
    flag('my_count$counter',N,0),
    writeln(N).

my_count(Goal,_):-
    flag('my_count$counter',_,0),
    Goal,
    flag('my_count$counter',N,N+1),
    fail.
my_count(_,N):-
    flag('my_count$counter',N,0).

    
bla_set_base(ns(A, B, _), C, ns(A, B, C)).
bla(A):-
    pdt_namespace_new(bla,NS),
    blabla(NS,ja,[umf]),deterministic(A).

blabla(NS,ja,A):-
	pdt_namespace:ns_set_base(NS,A,_).
blabla(_,nein,_A).

:- begin_tests(namespace,[setup(my_setup),cleanup(my_teardown)]).

test(local_resolve,[true(A==fooval)]):-
    myns(NS),
    pdt_namespace_bind(NS,foo,fooval,NS1),
    pdt_namespace_bind(NS1,bar,barval,NS2),
    pdt_namespace_resolve(NS2,foo,A).

test(remote_bind_throw,[throws(error(no_local_name(bar:bau),bla))]):-
    myns(NS),
    pdt_namespace_bind(NS,foo,fooval,NS1),
   	pdt_namespace_bind(NS1,bar:bau,bauval,NS).


test(add_base_deterministic):-
    myns(NS),
    pdt_namespace_add_base(NS,bla,start,_).

test(remote_resolve_stack,[
		true(Foo-Bar-Bau==fooval-Remote1-bauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_bind(Local1,bar,Remote1,Local2),
    pdt_namespace_resolve(Local2,foo,Foo),
    pdt_namespace_resolve(Local2,bar,Bar),
    pdt_namespace_resolve(Local2,bar:bau,Bau).

test(remote_resolve_heap,[
		true(Foo-Bar-Bau==fooval-bar_nsid-bauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_store(Remote1),
    pdt_namespace_bind(Local1,bar,bar_nsid,Local2),
    pdt_namespace_resolve(Local2,foo,Foo),
    pdt_namespace_resolve(Local2,bar,Bar),
    pdt_namespace_resolve(Local2,bar:bau,Bau).

test(resolve_base_stack,[
		true(Foo-Bau==fooval-bauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_add_base(Local1,Remote1,end,Local2),
    pdt_namespace_resolve(Local2,foo,Foo),
    pdt_namespace_resolve(Local2,bau,Bau).

test(resolve_base_heap,[
		true(Foo-Bau==fooval-bauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_store(Remote1),
    pdt_namespace_add_base(Local1,bar_nsid,end,Local2),
    pdt_namespace_resolve(Local2,foo,Foo),
    pdt_namespace_resolve(Local2,bau,Bau).
    
test(resolve_base_two,[
		true(Foo-Bau-OtherBau==fooval-bauval-otherbauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_store(Remote1),
    pdt_namespace_bind(Remote,otherbau,otherbauval,Remote2),
    pdt_namespace_add_base(Local1,bar_nsid,end,Local2),
    pdt_namespace_add_base(Local2,Remote2,start,Local3),
    pdt_namespace_resolve(Local3,foo,Foo),    
    pdt_namespace_resolve(Local3,bau,Bau),
    pdt_namespace_resolve(Local3,otherbau,OtherBau).

test(resolve_base_override_two,[
		true(Foo-Bau==fooval-otherbauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_store(Remote1),
    pdt_namespace_bind(Remote1,bau,otherbauval,Remote2),
    pdt_namespace_add_base(Local1,bar_nsid,end,Local2),
    pdt_namespace_add_base(Local2,Remote2,start,Local3),
    pdt_namespace_resolve(Local3,foo,Foo),    
    pdt_namespace_resolve(Local3,bau,Bau).

test(resolve_base_override,[
		true(Foo-Bau==fooval-otherbauval)
	]):-
    myns(Local),
    pdt_namespace_bind(Local,foo,fooval,Local1),
    pdt_namespace_new(bar_nsid,Remote),
    pdt_namespace_bind(Remote,bau,bauval,Remote1),
    pdt_namespace_store(Remote1),
    pdt_namespace_add_base(Local1,bar_nsid,end,Local2),
    pdt_namespace_bind(Local2,bau,otherbauval,Local3),
    pdt_namespace_resolve(Local3,foo,Foo),    
    pdt_namespace_resolve(Local3,bau,Bau).


:- end_tests(namespace).