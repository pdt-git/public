:- module(builder__test_deps,[dep_test/0]).

:- use_module(builder__graph).
:- use_module(library('util/concurrent_tests')).


% test cases for the algorithm that removes redundant dep edges.
% See PDT-305.
% See Wiki:
% https://sewiki.iai.uni-bonn.de/research/pdt/developers/architecture/subsystems/pdtcore/buildsystem/avoidredundantdependencies

% What is tested?
% For each test there is an input graph and an expected outcome graph.
% Edges of the input graph are added in every possible order. We use the concurrent_tests library
% and specify this as nondeterministic interleaving. 
% The resulting dependency graph is then compared with the expected outcome. 


dep_test:-
	(	current_thread(test_driver,_)
	->	thread_join(test_driver,_)
	;	true
	),
	thread_create(
		run_concurrent_test(dep_test),
		_,
		[alias(test_driver)]
	).

:- discontiguous input/3, input/4, test_case/2.

input2(N,A,B):-
	input(N,A,B).
input2(N,A,B):-
	input(N,A,B,_).

expected(N,A,B):-
	input(N,A,B).	


output_as_expected(N):-	
	% completeness:
	forall(expected(N,A,B),target_depends(A,B)),
	
	% correctness:
	forall(target_depends(A,B),expected(N,A,B)),
	
	% no duplicates:
	findall(A-B,expected(N,A,B),Expected),
	findall(A-B,target_depends(A,B),Output),
	length(Expected,ExpectedLen),
	length(Output,OutputLen),
	OutputLen==ExpectedLen.

test_case(0,"trivial graph").
input(0,a,b).
input(0,b,c).

test_case(1,"trivial shortcut").
input(1,a,b).
input(1,b,c).
input(1,c,d).	
input(1,a,d,red).  

test_case(2,"no redundant edge, but a cycle").
input(2,a,b).
input(2,b,c).
input(2,c,d).
input(2,c,e).
input(2,e,b).
	
test_case(3,"redundant bwd edge").	
input(3,a,b).
input(3,b,c).
input(3,c,d).
input(3,d,a).
input(3,b,a,red).

test_case(4,"cycle and redundant edge (not on cycle)").
input(4,a,b).
input(4,b,c).
input(4,d,c,red).
input(4,d,a).
input(4,b,a).

spyme.

fixture_setup(Num):-
	load_graph(Num),
	(	findall(A,(target_depends(a,b),A=1),As),
		length(As,2)
	->	spyme
	;	true
	).

fixture_store(Num):-
	(	findall(A,(target_depends(a,b),A=1),As),
		length(As,2)
	->	spyme
	;	true
	),
	store_graph(Num).
	
fixture_teardown(_).	

sequence(
	dep_test,
	builder__test_deps,
	meta(
		test_case(N,_),
		(?),
		sequence(dep_test(N)),
		share(_,_,_)
	)	
).

sequence(
	dep_test(N),
	builder__test_deps,
	(	meta(
			input2(N,A,B),
			(#),
			(	builder__graph:add_dependency(A,B)
			),
			share(_,_,_)
		)	
	~>	check(
			output_as_expected(N)
		)
	)
).
