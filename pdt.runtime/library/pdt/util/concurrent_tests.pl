/*
Goal:
- define a simple language for specifying concurrent, nondeterministic  sequences of atomic actions.
- define a simple language for special actions that constitute checks of some conditions.
- use term expansion to generate a set of all deterministic sequences that are represented by the nondeterministic one.
*/
:- module(concurrent_tests,
	[	op(1140, xfy, #), % interleave
		op(1135, xfy, ?), % nondet. choice
		op(1130, xfy, ~>), % concat
		run_concurrent_test/1
	]
).

:- module_transparent run_concurrent_test/1.
:- dynamic todo/2.
:- thread_local todo/2.
run_concurrent_test(Head):-
	context_module(Module),
	concurrent_tests:assert(todo(Module:sequence(Head),0)),
	repeat,
		step(Module,Head),
		\+ concurrent_tests:todo(_,_),
	!.

max_depth(0).



/*
Sequence Syntax

Atomic sequences / literals: 
 - Prolog goals,
 - sequence(Head),
 - check(Goal)
 
Complex sequences: If A and B are sequences then
 - A ~> B  is a sequence (concatenation)
 - A ? B is a sequence (nondeterministic choice)
 - A # B is a sequence (nondeterministic interleaving)
 
Meta sequence: If S is a sequence then
 - meta(Goal, Op, S) is a sequence. 

You can think of this "meta" sequence as a "static" forall. Goal is evaluated during sequence reduction (see below).
Any bindings resulting from this are used to instantiate Sequence. The meta/3 literal is replaced
by a sequence resulting from the combination of all instances with Operator.
*/


/*
Pre-process sequences: explicitly specify the context module for each literal.
*/

add_module_prefix(In,Module,Out):-
	(	In=nil
	->	Out=In
	;	In=(A~>B)	
	->	Out=(AA~>BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=(A?B)
	->	Out=(AA?BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=(A#B)		
	->	Out=(AA#BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In = meta(Goal, Op, S)
	->	Out = meta(Module:Goal,Op,SS),
		add_module_prefix(S,Module,SS)
	;	Out=Module:In
	).



/*
Reduction to deterministic sequences:

We use a set of rules to transform any sequence into a nondeterministic choice of 
deterministic sequences. The result looks like this:

  a ~> b ~> ... ~> c 
? d ~> e ~> ... ~> f 
? ... 
and so on. The a,b,c,... are atomic sequences (literals)
*/

/*
braindump 3.9.08, 2223,

platzeffizienz ist der knackpunkt, nicht zeiteffizienz. (vgl model checking) 

Idee: Eine nicht-deterministische Sequenz entspricht einer nicht deterministischen
Entscheidung zwischen mehreren deterministischen Sequenzen.

In allen für uns relevanten Fällen: endlicher Verzweigungsgrad. 

A ? B: 
det(A) U det(B)

A ~> B
{a~>b|a aus det(A), b aus det(B)}

A # B
{a1~>det(a # b1~>b), b1~>det(b # a1~>b) |a1~>a aus det(A), b1~>b aus det(B)}






*/

% eliminate empty sequences
reduce_rule( 	(A ? nil),				A										,r(nil,1,_)).
reduce_rule( 	(nil ? A),				A										,r(nil,2,_)).
reduce_rule( 	(A # nil),				A										,r(nil,3,_)).
reduce_rule( 	(nil # A),				A										,r(nil,4,_)).
reduce_rule( 	(A ~> nil),				A										,r(nil,5,_)).
reduce_rule( 	(nil ~> A),				A										,r(nil,6,_)).




% normalize order
reduce_rule( 	((A ? B) ? C),			(A ?B ? C)								,r(norm,1,_)).
reduce_rule( 	((A # B) # C),			(A #B # C)								,r(norm,2,_)).
reduce_rule( 	((A ~> B) ~> C),		(A ~>B ~> C)							,r(norm,2,_)).

% pull up disjunction
reduce_rule( 	((A?B)#C),				((A#C) ? (B#C))							,r(pull,1,_)). 
reduce_rule(	(A#(B?C)),				((A#B) ? (A#C))							,r(pull,2,_)). 
reduce_rule( 	((A?B)~>C),				((A~>C) ? (B~>C))						,r(pull,3,_)). 
reduce_rule(	(A~>(B?C)),				((A~>B) ? (A~>C))						,r(pull,4,_)).

% eliminate interleave operator
reduce_rule(	(A~>B#C~>D),			( (A~>(B # C~>D)) ? (C~>(D# A~>B )) )	,r(elm,1,_)).
reduce_rule(	(A~>B#C),				(A~>(B # C) ? C ~>A~>B )				,r(elm,2,_)). %assumes C is atomic.
reduce_rule(	(A#B~>C),				(A~>B~>C ? B~>(A#C))					,r(elm,3,_)). %assumes A is atomic.
reduce_rule(	(A#B),					(A~>B ? B~>A)							,r(elm,3,_)). %assumes A and B are atomic.



% expand meta literals
reduce_rule(	meta(Goal,Op,Template),	Sequence								,r(meta,1,_)):-
	findall(Template,Goal,Instances),
	combine(Instances,Op,Sequence).


combine([],_,nil).
combine([Seq|Seqs],Op,Out):-
	Out =.. [Op,Seq,Seqs2],
	combine(Seqs,Op,Seqs2).
	

subsequence0((A~>B),A,AA,(AA~>B)).
subsequence0((A~>B),B,BB,(A~>BB)).
subsequence0((A?B),A,AA,(AA?B)).
subsequence0((A?B),B,BB,(A?BB)).
subsequence0((A#B),A,AA,(AA#B)).
subsequence0((A#B),B,BB,(A#BB)).


subsequence(Seq0,Seq0,Seq,Seq).
subsequence(Seq0,Sub0,Sub,Seq):-
	subsequence0(Seq0,Sub1,Sub2,Seq),
	subsequence(Sub1,Sub0,Sub,Sub2).



reduce(In,Out):-
	(	subsequence(In,Sub1,Sub,Expanded),
		reduce_rule(Sub1,Sub,Name)
	->	%format("apply ~w on ~w yields ~w~n",[Name,Sub1,Expanded]),
		format("apply ~w~n",[Name]),
		reduce(Expanded,Out)		
	;	In=Out
	).


detseq(S,DS):-
	(	S = (A?B)
	->	(detseq(A,DS) ; detseq(B,DS))
	;	S = (A~>B)	
	->	detseq(A,DA),
		detseq(B,DB),
		concatseq(DA,DB,DS)
	;	S = (A # B)
	->	detseq(A,DA),
		detseq(B,DB),
		interleave(DA,DB,DS)
	;	S = meta(Goal,Op,Template)
	->	findall(Template,Goal,Instances),
		combine(Instances,Op,Sequence),
		detseq(Sequence,DS)
	;	DS = S
	).	

%In and B are deterministic
concatseq(In,B,Out):-
	(	In = (A~>As)
	->	Out = (A~>Tail),
		concatseq(As,B,Tail)
	;	Out = (A ~> B)
	).	

%InA and InB are deterministic
interleave(A,B,Out):-
	(	A==nil
	->	Out=B
	;	B==nil
	->	Out=A
	;	seq_first_tail(A,FirstA,TailA),
		Out=(FirstA ~> Tail), 
		interleave(TailA,B,Tail)
	;	seq_first_tail(B,FirstB,TailB),
		Out=(FirstB ~> Tail), 
		interleave(A,TailB,Tail)
	). 


seq_first_tail(nil,nil,nil):-!.
seq_first_tail((A~>B),A,B):-!.
seq_first_tail(A,A,nil).

/*

Semantic of deterministic sequences.

The literals of a deterministic sequence are executed one after another, possibly instantiating subsequent
literals. If the execution of a literal fails, the sequence is discarded as irrelevant.
If the execution raises an exception, the sequence is regarded as error case.

There are two special literals recognized by the meta-interpreter: 

- check(Goal)
This throws an exception if Goal fails. Similar to the assert() methods of JUnit.

- sequence(Head)
Refers to a subsequence. If the interpreter encounters it, it marks the sequence as "incomplete". Execution is 
deferred until after the next unfolding step. If the current unfolding depth exceeds a predefined maximum, an exception is raised.

*/



% non-recursive unfolding:
% simultaneously replace all occurrences of
% sequence/1 with the respective body.  
unfold_step(In,Out):-
	(	In=Cx0:sequence(Head)
	->	Out=meta(
			(	Cx0:sequence(Head,Cx, Body0),
				add_module_prefix(Body0,Cx,Body)
			),
			(?), 
			Body
		)
	;	In=(A~>B)	
	->	Out=(AA~>BB),
		unfold_step(A,AA),
		unfold_step(B,BB)
	;	In=(A?B)
	->	Out=(AA?BB),
		unfold_step(A,AA),
		unfold_step(B,BB)
	;	In=(A#B)		
	->	Out=(AA#BB),
		unfold_step(A,AA),
		unfold_step(B,BB)
	;	Out=In
	).


step(Module,Head):-
	retract(todo(In,Depth)),
	unfold_step(In,Unfolded),	
	NewDepth is Depth +1,
	forall(
		detseq(Unfolded,DetSeq),
		process_deterministic_sequence(DetSeq,Module,Head,NewDepth)
	).


	
process_deterministic_sequence(Seq,Module,Head,Depth):-
	%format("processing ~w (depth=~w) ~n",[Seq,Depth]),
	Module:ignore(setup(Head)),
	process_literals(Seq,Outcome),
	Module:ignore(teardown(Head)),
	process_outcome(Outcome,Seq,Depth).

process_outcome(Outcome,Seq,Depth):-
	(	Outcome = error(E)
	->	format("Sequence produced error: ~w~nFailing sequence:~w~n",[E,Seq])
	;	Outcome = incomplete
	->	max_depth(Max),
		(	Depth > Max	
		->	format("Sequence exceeds depth limit: ~w~n",[Seq])
		;	assert(todo(Seq,Depth))
		)
	;	true
	).


process_literals(Part,Outcome):-
	(	Part = (_:sequence(_) ~> B)
	->	Outcome = incomplete
	;	Part = (A ~> B)
	->	execute_literal(A,OutcomeA),
		(	OutcomeA == true
		->	process_literals(B,Outcome)
		;	Outcome=OutcomeA
		)
	;	Part = nil
	->	Outcome=true
	;	execute_literal(Part,Outcome)
	).

execute_literal(Lit0,Outcome):-
	Lit0 = Cx:Lit,
	(	Lit = check(Goal)
	->	(	Cx:catch(once(Goal),Error,fail)
		->	Outcome=true
		;	nonvar(Error)
		->	Outcome=Error
		;	Outcome=error(check_failed(Goal))
		)
	;	(	Cx:catch(once(Goal),Error,fail)
		->	Outcome=true
		;	nonvar(Error)
		->	Outcome=Error
		;	Outcome=fail
		)
	).


		
write_sequences((Seq?Seqs)):-
	!,
	writeln(Seq),
	write_sequences(Seqs).
write_sequences(Seq):-
	writeln(Seq).
	
	




	
