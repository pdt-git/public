:- ensure_loaded(pdt_facade).
:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).


create_test_project(Path):-
    pef_reserve_id(pef_project,Project),
    pef_reserve_id(pef_source_path,SourcePath),
    pef_project_assert([id=Project,name=test_project]),
    pef_source_path_assert([id=SourcePath,path=Path,include_pattern='.*\.pl', exclude_pattern='']).


%use the pdt library as test source path
test_path(testproject,'..').
    
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
	