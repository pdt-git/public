:- use_module(library('/org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_set')).

%maps integers to current functors 
create_functor_table(Tbl, Count):-
    flag(create_functor_table_counter,_,0),
    pdt_map_findall(Key,Name/Arity,
    	(	current_functor(Name,Arity),
    		flag(create_functor_table_counter,Key,Key+1)
    		
    	),Tbl
    ),
    flag(create_functor_table_counter,Count,0),
    recorda(functor_table,Tbl:Count).

create_random_term(Vars,_Tbl,_TblSize,0,Term):-
    length(Vars,Len),
    I is random(Len),
    nth0(I,Vars,Term),
	!.
create_random_term(Vars,Tbl,TblSize,MaxDepth,Term):-
    Key is random(TblSize),
    pdt_map_get(Tbl,Key,Name/Arity),
    functor(Term,Name,Arity),   
    create_random_args(Vars,Tbl,TblSize,MaxDepth,1,Arity,Term).
    
create_random_args(_Vars,_Tbl,_TblSize,_NextDepth,N,M,_Term):-
    N>M,
    !.
create_random_args(Vars,Tbl,TblSize,MaxDepth,N,M,Term):-
    ArgDepth is min(MaxDepth-1,random(max(1, MaxDepth))), %should be greater than 0!
    create_random_term(Vars,Tbl,TblSize,ArgDepth,Arg),
    arg(N,Term,Arg),
    O is N+1,
    create_random_args(Vars,Tbl,TblSize,MaxDepth,O,M,Term).

    
create_random_term(Vars,MaxDepth,Term):-
    recorded(functor_table,Tbl:TblSize),
    create_random_term(Vars,Tbl,TblSize,MaxDepth,Term).
    
    
create_random_clause(Vars,MaxDepth,Term):-
    create_random_clause([d,f,f,r,r,r],Vars,MaxDepth,Term).
create_random_clause(Types,Vars,MaxDepth,Term):-
    recorded(functor_table,Tbl:TblSize),
    !,
    length(Types,Len),
    I is random(Len),
    nth0(I,Types,Type),
	create_random_clause(Type,Vars,Tbl,TblSize,MaxDepth,Term).
	
create_random_clause(d,Vars,Tbl,TblSize,MaxDepth,(:-Term)):-
    repeat,
    	create_random_term(Vars,Tbl,TblSize,MaxDepth,Term),
    	not(functor(Term,op,3)),
    !.    
create_random_clause(f,Vars,Tbl,TblSize,_MaxDepth,(Term)):-
    create_random_term(Vars,Tbl,TblSize,2,Term).        
create_random_clause(r,Vars,Tbl,TblSize,MaxDepth,(Head:-Body)):-
    create_random_term(Vars,Tbl,TblSize,2,Head),
    create_random_body(Vars,Tbl,TblSize,MaxDepth,MaxDepth,Body).            

create_random_body(Vars,Tbl,TblSize,MaxDepth,MaxBodySize,Head):-
    MaxBodySize=< 1,
    !,
    create_random_term(Vars,Tbl,TblSize,MaxDepth,Head).    
 
create_random_body(Vars,Tbl,TblSize,MaxDepth,MaxBodySize,(Head,Tail)):-
    create_random_term(Vars,Tbl,TblSize,MaxDepth,Head),
    TailSize is min(MaxBodySize-1,random(max(1, MaxBodySize))), %should be greater than 1!
    create_random_body(Vars,Tbl,TblSize,MaxDepth,TailSize,Tail).

create_random_file(File):-
    N is random(1000),
    open(File,write,Stream),
    flag(clause_counter,_,0),
    repeat,
    	create_random_clause([_A,_B,_C,_A,_B,_C,a,b,c,2,3,4,'aj caramba'],5,Term),
    	nonvar(Term),
    	portray_clause(Stream,Term),
    	flag(clause_counter,C,C+1),
    	C>N,
    !,
    close(Stream).



gen_test_term(Term):-
    repeat,
   	create_random_clause([_A,_B,_C,_A,_B,_C,a,b,c,2,3,4,'aj caramba'],5,Term),
   	nonvar(Term),
   	!.

benchmark_assert(N):-
    flag(clause_counter,_,0),
    repeat,
    	gen_test_term(Term),
    	assert(benchmark(Term)),
    	flag(clause_counter,C,C+1),
    	C>N,
    !.
benchmark_record(N):-
    flag(clause_counter,_,0),
    repeat,
    	gen_test_term(Term),
       	recorda(benchmark,Term),
    	flag(clause_counter,C,C+1),
    	C>N,
    !.
benchmark_rbtree(O):-
    pdt_set_empty(S0),
    benchmark_rbtree(1,O,S0,_).    
benchmark_rbtree(N,O,S,S):-    
    N>O,
    !.
benchmark_rbtree(N,O,S0,S2):-        
   	gen_test_term(Term),
   	pdt_set_add(S0,Term,S1),
   	M is N +1,
   	benchmark_rbtree(M,O,S1,S2).
   	
   	
load_test:-
    flag(file_counter,_,0),
	repeat,
		flag(file_counter,C,C+1),	
		concat_atom(['/tmp/test_input_',C,'.pl'],File),
		(	exists_file(File)
		->	writeln(exists(File))
		;	writeln(creating(File)),
			create_random_file(File)
		),
		writeln(annotating(File)),
		pdt_ensure_annotated(File),
		writeln(done(File)),
		C>200,
	!.