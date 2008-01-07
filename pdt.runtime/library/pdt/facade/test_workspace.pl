:- ensure_loaded(pdt_facade).
:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).
:- ensure_loaded(library('builder/targets/literals')).
:- ensure_loaded(library('builder/targets/ast')).
:- ensure_loaded(library('builder/targets/newlines')).
:- ensure_loaded(library('builder/targets/delta')).
:- ensure_loaded(library('builder/depgraph')).
:- ensure_loaded(library('util/layout')).
:- ensure_loaded(library('util/ast_util')).
:- ensure_loaded(library('util/ast_transform')).
:- ensure_loaded(library('org/cs3/pdt/util/pdt_util_term_position')).


create_test_project(Path):-
    pef_reserve_id(pef_project,Project),
    pef_reserve_id(pef_source_path,SourcePath),
    pef_project_assert([id=Project,name=test_project]),
    pef_source_path_assert([id=SourcePath,path=Path,include_pattern='.*\.pl', exclude_pattern='']).


%use the pdt library as test source path
test_path(testproject,'testws').
    
:- prolog_load_context(directory,Dir),
	format("current directory: ~w~n",[Dir]),
	forall(test_path(PrjName,RelPath),
		(	format("resolving: ~w ...",[RelPath]),
			absolute_file_name(RelPath,[relative_to(Dir)],SrcPath),
			format("found ~w~n",[SrcPath]),
			pdt_add_source_path(PrjName,SrcPath,'.*\.pl','')
		)
	).


initial_request:-
    gensym(initial,Initial),
    thread_create(pdt_request_target(problems(workspace)),_,[alias(Initial)]),
    my_join(Initial,Status),
    format("initial: ~w~n",[Status]).
	
concurrent_requests(F):-
    initial_request,
    gensym(outline,Outline),
    gensym(problems,Problems),    
    format("invalidating: parse(~w)~n",[F]),
	pdt_invalidate_target(parse(F)),
	writeln('done.'),
	
	thread_create(pdt_request_target(outline(F)),_,[alias(Outline)]),
	format("started: outline(~w)~n",[F]),
	thread_create(pdt_request_target(problems(workspace)),_,[alias(Problems)]),
	format("started: problems(workspace)~n",[]).
	
	%my_join(Outline,OutlineStatus),
    %format("outline: ~w~n",[OutlineStatus]),
	%my_join(Problems,ProblemsStatus),
    %format("problems: ~w~n",[ProblemsStatus]).
    
my_join(Thread,Status):-
    format("joining ~w~n",[Thread]),
	call_with_time_limit(20,thread_join(Thread,Status)).
	
my_layout(Path):-
	get_pef_file(Path,File),
	pdt_request_targets([ast(file(Path)),newlines(file(Path)),literals(full,file(Path))]),
	pef_toplevel_query([file=File,id=Toplevel]),
	pef_ast_query([toplevel=Toplevel,root=Root]),
	layout_node(Root,current_output).
my_tokens(Path,Tokens):-
	get_pef_file(Path,File),
	pdt_request_targets([ast(file(Path)),newlines(file(Path)),literals(full,file(Path))]),
	pef_toplevel_query([file=File,id=Toplevel]),
	pef_ast_query([toplevel=Toplevel,root=Root]),
	repmac:ast_tokens(Root,Tokens).
my_delta(Path):-
	get_pef_file(Path,File),
	pdt_request_targets([ast(file(Path)),newlines(file(Path)),literals(full,file(Path))]),
	pef_toplevel_query([file=File,id=Toplevel,positions=Pos]),
	!,
	pef_ast_query([toplevel=Toplevel,root=Root]),	
	layout:get_memory_file(File,Memfile),
	open_memory_file(Memfile,read,In),
	call_cleanup(
		(	top_position(Pos,Offset,_),
			repmac:delta(Root,Offset,In,current_output)
		),
		close(In)
	).
	
ast_by_path(Start,Path0,Ast):-
    (	pef_type(Start,pef_file)
    ->	Path0=[N,Path],
    	nth_toplevel(N,Start,Tl)
    ;	Path0=Path,
    	Tl=Start
    ),
	pef_ast_query([toplevel=Tl,root=Root]),
	ast_by_path_X(Root,Path,Ast).

ast_by_path_X(N,[],N).
ast_by_path_X(N,[Num|Nums],M):-
	pef_arg_query([parent=N,num=Num,child=O]),
	ast_by_path_X(O,Nums,M).

nth_toplevel(N,File,Toplevel):-
    succeeds_n_times(
    	pef_toplevel_query([file=File,id=Toplevel]),
    	N
    ).

 :- module_transparent succeeds_n_times/2.

succeeds_n_times(Goal, Times) :-
    Counter = counter(0),
    (	var(Times)
	->  (   Goal,
	        arg(1, Counter, N0),
	        N is N0 + 1,
	        nb_setarg(1, Counter, N),
	        fail
	    ;   arg(1, Counter, Times)
	    )
	 ;	Goal,
	    arg(1, Counter, N0),
		N is N0 + 1,
	    nb_setarg(1, Counter, N),
        N == Times,
        !	    	    
	 ).	
/*
:- dynamic bla/2,bla/3. --> :- dynamic bla/3.
*/
transformation_1(Directive):-
    ast_root(Directive,Root),
    ast_simple_match((:-dynamic First,_),Root),
    ast_node(First,FirstNode),
    astseq_remove(FirstNode),
    fix_variables(Directive,merge),
    mark_toplevel_modified(Directive).
    
    