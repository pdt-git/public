:- use_module(library('org/cs3/pdt/util/pdt_util_rbtree')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).

:- pdt_define_context(bm_cx(method,initial_size,grow,task)).

bm_param(initial_size,0).	
bm_param(min_grow,0).	
bm_param(max_grow,100000).	
bm_param(step,10000).
bm_param(repetitions,4).
bm_param(key_max,10000000000000).

%--------------------

%% bm_method(-Method).
% generate all available Methods.
bm_method(assert_recursive).
%bm_method(assert_repeat_fail).
bm_method(rb_tree).
bm_method(assoc).



n_times(0,_Goal):-
    !.
n_times(N,_Goal):-
    N<0,
    !,
    throw(domain_error(positive_integer,N)).
n_times(N,Goal):-
    Goal,
    M is N -1,
    n_times(M,Goal).



bm_run:-
	bm_param(min_grow,N),
	bm_run(N).    

bm_run(N):-
    bm_param(max_grow,M),
    N>=M,
    !.
bm_run(N):-
    bm_param(repetitions,K),
    n_times(K,forall(bm_method(Method),bm_run(Method,N))),
    bm_param(step,D),
    M is N + D,
    bm_run(M).
    

bm_run(Method,N):-  

    bm_param(max_grow,Max),
    bm_param(initial_size,Size),
   	format("Method: ~w, Initial Size: ~w, Grow: ~w of ~w~n",[Method,Size,N,Max]),
   	
    bm_generate(Method,Size,Table),
	
    bm_generate_key_sets(N,UsedKeys,UnusedKeys), 
    bm_cx_new(Cx0),
	bm_cx_set(Cx0,[method=Method,initial_size=Size, grow=N,task=grow],Cx1),
	my_time(bm_grow(UsedKeys,Method,Table,Table1),Cx1),

	bm_cx_set_task(Cx1,lookup_success_pre,Cx2),
	my_time(bm_lookup_success(UsedKeys,Method,Table1),Cx2),
	
	bm_cx_set_task(Cx2,lookup_failure_pre,Cx3),
	my_time(bm_lookup_failure(UnusedKeys,Method,Table1),Cx3),

	bm_cx_set_task(Cx3,insert_and_delete,Cx4),
    (	Method==assoc
    ->	true,
    	bm_grow(UnusedKeys,Method,Table,Table2) % the rest should be equivalent
    ;  	my_time(bm_insert_and_delete(UsedKeys,UnusedKeys,Method,Table1,Table2),Cx4)
    ),

	bm_cx_set_task(Cx4,lookup_success_post,Cx5),
	my_time(bm_lookup_success(UnusedKeys,Method,Table2),Cx5),
	
	bm_cx_set_task(Cx5,lookup_failure_post,Cx6),
	my_time(bm_lookup_failure(UsedKeys,Method,Table2),Cx6),

    bm_reset(Method,Table1).	

bm_generate_key_sets(0,[],[]):-!.
bm_generate_key_sets(N,[I|Is],[J|Js]):-
    bm_gen_key(grow,I),
    bm_gen_key(unused,J),
    M is N -1,
    bm_generate_key_sets(M,Is,Js).


bm_grow([],_,Table,Table).
bm_grow([Key|Keys],Method,TableIn,TableOut):-
    bm_insert(Method,TableIn,Key,Key,TableNext),
    bm_grow(Keys,Method,TableNext,TableOut).

bm_insert_and_delete([],[],_,Table,Table).
bm_insert_and_delete([A|As],[B|Bs],Method,TableIn,TableOut):-
    bm_insert(Method,TableIn,B,B,Table1),
    bm_delete(Method,Table1,A,TableNext),
    bm_insert_and_delete(As,Bs,Method,TableNext,TableOut).


bm_lookup_success([],_,_).
bm_lookup_success([Key|Keys],Method,Table):-
    (	bm_lookup(Method,Table,Key,_)
    ;	throw(key_not_found(Key))
    ),
    !,
    bm_lookup_success(Keys,Method,Table).

bm_lookup_failure([],_,_).    
bm_lookup_failure([Key|Keys],Method,Table):-
    (	\+ bm_lookup(Method,Table,Key,_)
    ;	throw(key_found(Key))
    ),
    !,
    bm_lookup_failure(Keys,Method,Table).


bm_random_key(Multiplier,Offset,I):-
    bm_param(key_max,Len),
	I is Multiplier * random(floor(Len/Multiplier))+Offset.

bm_gen_key(start,I):-
    bm_random_key(3,0,I).
bm_gen_key(grow,I):-
    bm_random_key(3,1,I).
bm_gen_key(unused,I):-
    bm_random_key(3,2,I).    
    

bm_reserved_key(I):-
    bm_param(key_reserved_interval, From-To),
    I is From + random(To-From).
	

%% bm_generate(+Method, +Size, -Table)
% 
% generate a table with random entries.
%
% @param Method The table implementation to use. One of assert_recursive, assert_repeat_fail, 
%	rb_tree or assoc.
% @param Size number of random keys to add.
% @param Table Only used for non-destructive data strutures. It will be unified with the resulting table. 
%
% bla
bm_generate(assert_recursive, Size, bm_fact_recursive):-
    bm_assert(Size).
bm_generate(assert_repeat_fail, Size, bm_fact_repeat_fail):-
    bm_assert_2(Size).
bm_generate(rb_tree, Size, Table):-
    pdt_rbtree_empty(Table0),
    bm_rbtree(Size,Table0,Table).
bm_generate(assoc, Size, Table):-
	empty_assoc(Table0),
	bm_assoc(Size,Table0,Table).
	
%% bm_lookup(+Method, +Table, +Key, -Value)
% lookup an entry in a table.
% @param Method The table implementation to use. One of assert_recursive, assert_repeat_fail, 
%	rb_tree or assoc.
% @param Table Only used for non-destructive data strutures (assoc and rb_tree). 
%    It will be unified with the resulting table. 
% @param Key The key to look for.
% @param Value will be unified with the value associated to key.
bm_lookup(assert_recursive, _, Key, Value):-
    bm_fact_recursive(Key,Value).
bm_lookup(assert_repeat_fail, _, Key, Value):-
    bm_fact_repeat_fail(Key,Value).
bm_lookup(rb_tree, Table, Key, Value):-
    pdt_rbtree_lookup(Key,Value,Table).
bm_lookup(assoc, Table, Key, Value):-    
    get_assoc(Key,Table,Value).

%% bm_insert(+Method, +Table, +Key, +Value, -NewTable).	    
% Insert an entry into a table.
% @param Method The table implementation to use. 
% @param Table Only used for non-destructive data strutures (assoc and rb_tree). 
%    It will be unified with the resulting table. 
% @param Key The key of the new entry.
% @param Value the value of the new entry.
% @param NewTable the changed version of the table. Only used for non-destructive data structures.
bm_insert(assert_recursive, Table, Key, Value, Table):-
    assert(bm_fact_recursive(Key,Value)).
bm_insert(assert_repeat_fail, Table, Key, Value, Table):-
    assert(bm_fact_repeat_fail(Key,Value)).
bm_insert(rb_tree, TableIn, Key, Value, TableOut):-
    pdt_rbtree_insert(TableIn,Key,Value,TableOut).
bm_insert(assoc, TableIn, Key, Value, TableOut):-
    put_assoc(Key,TableIn,Value,TableOut).
    
%% bm_delete(+Method, +Table, +Key, -NewTable).	    
% Delete an entry from a table.
% @param Method The table implementation to use. 
% @param Table Only used for non-destructive data strutures (assoc and rb_tree). 
%    It will be unified with the resulting table. 
% @param Key The key of the entry that is to be deleted.
% @param NewTable the changed version of the table. Only used for non-destructive data structures.
bm_delete(assert_recursive, Table, Key, Table):-
    retract(bm_fact_recursive(Key,_)).
bm_delete(assert_repeat_fail, Table, Key, Table):-
    retract(bm_fact_repeat_fail(Key,_)).
bm_delete(rb_tree, TableIn, Key, TableOut):-    
    pdt_rbtree_delete(TableIn,Key,TableOut).
bm_delete(assoc, _TableIn, _Key, _TableOut):-    
    throw(not_implemented).

%% bm_reset(+Method, +Table).
% clean up after the benchmark.
% @param Method The table implementation to use. 
% @param Table Only used for non-destructive data strutures (assoc and rb_tree). 
bm_reset(assert_recursive, _):-
    retractall(bm_fact_recursive(_,_)).
bm_reset(assert_repeat_fail, _):-
    retractall(bm_fact_repeat_fail(_,_)).
bm_reset(rb_tree, _).
bm_reset(assoc, _).
    
    
%--------------------

:-dynamic bm_fact_recursive/1, bm_fact_repeat_fail/1.
bm_assert:-
    bm_param(size_max,N),
	bm_assert(N).
		
bm_assert(0):-!.
bm_assert(N):-
    bm_gen_key(start,Key),
	assert(bm_fact_recursive(Key)),
	M is N - 1,
	bm_assert(M).

%----------------------------	


bm_assert_2:-
    bm_param(size_max,N),
    bm_assert_2(N).

bm_assert_2(N):-
    flag(counter,_,0),
    repeat,
	    bm_gen_key(start,Key),
		assert(bm_fact_repeat_fail(Key)),
		flag(counter,J,J+1),
		J=N,
	!.

%----------------------------	
	
bm_rbtree:-
    bm_param(size_max,N),
    pdt_rbtree_empty(T0),
    bm_rbtree(N,T0,_T).
    
bm_rbtree(0,_,_):-!.
bm_rbtree(I,Tin,Tout):-
    bm_gen_key(start,Key),
	pdt_rbtree_insert(Tin,Key,Key,Tnext),
	J is I - 1,
	bm_rbtree(J,Tnext,Tout).
%----------------------	
bm_assoc:-
    bm_param(size_max,N),
    empty_assoc(T0),
    bm_assoc(N,T0,_T).
    
bm_assoc(0,_,_):-!.
bm_assoc(I,Tin,Tout):-
    bm_gen_key(start,Key),
	put_assoc(Key, Tin, Key, Tnext),
	J is I - 1,
	bm_assoc(J,Tnext,Tout).
%------------------------------

:- dynamic bm_result/5.

my_time(Goal0,Cx) :-
	expand_goal(Goal0, Goal),
	get_time(OldWall),
	statistics(cputime, OldTime), 
	statistics(inferences, OldInferences), 
	(   catch(Goal, E, true)
	->  Result = yes
	;   Result = no
	),
	statistics(inferences, NewInferences), 
	statistics(cputime, NewTime), 
	get_time(NewWall),
	UsedTime is NewTime - OldTime, 
	UsedInf  is NewInferences - OldInferences - 3, 
	Wall     is NewWall - OldWall,
	(   UsedTime =:= 0
	->  Lips = 'Infinite'
	;   Lips is integer(UsedInf / UsedTime)
	), 
	%print_message(informational, time(UsedInf, UsedTime, Wall, Lips)),
	assert(bm_result(Cx,UsedInf, UsedTime, Wall, Lips)),
	(   nonvar(E)
	->  throw(E)
	;   Result == yes
	).