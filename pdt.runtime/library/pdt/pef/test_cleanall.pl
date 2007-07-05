:- use_module(pef_base).


setup:-
    pef_file_assert([id=0,path=the_file]),
    
    pef_toplevel_assert([id=1,file=0]),
    pef_variable_assert([id=2,ast=101,name='MyVar']),
    pef_term_assert([id=3,name=t,arity=1]),
    pef_variable_occurance_assert([id=4,variable=2]),
    pef_arg_assert([num=1,parent=3,child=4]),
    pef_singleton_assert([id=5,variable=2]),
    pef_ast_assert([id=101,toplevel=1,root=3]),
    
    pef_toplevel_assert([id=6,file=0]),
    pef_variable_assert([id=7,ast=106,name='MyVar2']),
    pef_term_assert([id=8,name=tt,arity=1]),
    pef_variable_occurance_assert([id=9,variable=7]),
    pef_arg_assert([num=1,parent=8,child=9]),
    pef_singleton_assert([id=10,variable=7]),
    pef_ast_assert([id=106,toplevel=6,root=8]).
teardown:-
    pef_file_retractall([id=0,path=the_file]),
    
    pef_toplevel_retractall([id=1,file=0]),
    pef_variable_retractall([id=2,ast=101,name='MyVar']),
    pef_term_retractall([id=3,name=t,arity=1]),
    pef_variable_occurance_retractall([id=4,variable=2]),
    pef_arg_retractall([num=1,parent=3,child=4]),
    pef_singleton_retractall([id=5,variable=2]),
    pef_ast_retractall([id=101,toplevel=1,root=3]),
    
    pef_toplevel_retractall([id=6,file=0]),
    pef_variable_retractall([id=7,ast=106,name='MyVar2']),
    pef_term_retractall([id=8,name=tt,arity=1]),
    pef_variable_occurance_retractall([id=9,variable=7]),
    pef_arg_retractall([num=1,parent=8,child=9]),
    pef_singleton_retractall([id=10,variable=7]),
    pef_ast_assert([id=106,toplevel=6,root=8]).

test1:-
    teardown,
    setup,
    call_cleanup( do_test1,true	).

do_test1:-
    assert_succeeds(pef_toplevel_cleanupall([id=1])),
    
	assert_succeeds(pef_file_query([id=0,path=the_file])),
    
    assert_fails(pef_toplevel_query([id=1])),
    assert_fails(pef_variable_query([id=2])),
    assert_fails(pef_term_query([id=3])),
    assert_fails(pef_variable_occurance_query([id=4])),
    assert_fails(pef_arg_query([num=1,parent=3,child=4])),
    assert_fails(pef_singleton_query([id=5])),
    assert_fails(pef_ast_query([id=101])),
    
    assert_succeeds(pef_toplevel_query([id=6])),
    assert_succeeds(pef_variable_query([id=7])),
    assert_succeeds(pef_term_query([id=8])),
    assert_succeeds(pef_variable_occurance_query([id=9])),
    assert_succeeds(pef_arg_query([num=1,parent=8,child=9])),
    assert_succeeds(pef_singleton_query([id=10])),
    assert_succeeds(pef_ast_query([id=106])).

assert_succeeds(Goal):-
	(	catch(Goal,E,writeln(throw(Goal,E)))
	->	writeln(pass(Goal))
	;	writeln(fail(Goal))
	).

assert_fails(Goal):-
	(	catch(Goal,E,writeln(throw(Goal,E)))
	->	writeln(fail(Goal))
	;	writeln(pass(Goal))
	).

	
exists([]).
exists([Id|Ids]):-
	!,
	exists(Id),
	exists(Ids).
exists(Id):-
    pef_type(Id,Type),
    pef_base:'$metapef_template'(Type,Tmpl),
    pef_base:find_id(Tmpl,IdNum),
    functor(Tmpl,Name,Arity),
    functor(Data,Name,Arity),
    arg(IdNum,Data,Id),
    pef_base:call(Data).
    
not_exists([]).
not_exists([Id|Ids]):-
	!,
	not_exists(Id),
	not_exists(Ids).
not_exists(Id):-
	\+ exists(Id).


	    