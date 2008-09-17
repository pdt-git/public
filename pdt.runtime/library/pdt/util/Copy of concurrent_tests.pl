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

:- module_transparent 
	run_concurrent_test/1.
:- dynamic todo/4.
%:- thread_local todo/4.

run_concurrent_test(Head):-
	tell('testresults.log'),
	context_module(Module),
	concurrent_tests:call_cleanup(run_concurrent_test_X(Module,Head),told).

run_concurrent_test_X(Module,Head):-
	flag(seq_counter,_,0),	
	assert(todo([],Module:sequence(Head),0,[])),
	repeat,
		step(Module,Head),
		\+ concurrent_tests:todo(_,_,_,_),
	!.

max_depth(10).




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
	;	In = meta(Goal, Op, S,Share)
	->	Out = meta(Module:Goal,Op,SS,Share),
		add_module_prefix(S,Module,SS)		
	;	Out=Module:In
	).







combine([],_,nil).
combine([Seq|Seqs],Op,Out):-
	Out =.. [Op,Seq,Seqs2],
	combine(Seqs,Op,Seqs2).
	
combine([],_,_,_,nil).
combine([Inst-Input-Output|More],Op,Input, Template,Sequence):-	
	(	More == []
	->	Inst=Template,
		Sequence=Inst
	;	Sequence =..[Op,Inst,MoreSequence],
		combine(More,Op,Output,Template,MoreSequence)
	).



detseq(S,DS):-
	(	S = (A?nil)
	->	detseq(A,DS)
	;	S = (nil?A)
	->	detseq(A,DS)
	; 	S = (A#nil)
	->	detseq(A,DS)
	;	S = (nil#A)
	->	detseq(A,DS)
	; 	S = (A->nil)
	->	detseq(A,DS)
	;	S = (nil->A)
	->	detseq(A,DS)
	;	S = (A?B)
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
	;	S = meta(Goal,Op,Template,share(In,Init,Out))
	->	findall(Template-In-Out,Goal,Instances),
		combine(Instances,Op,Init,Template,Sequence),
		detseq(Sequence,DS)
	;	DS = S
	).	

%In and B are deterministic
concatseq(In,B,Out):-
	(	In == nil
	->	Out = B
	;	B == nil
	->  Out = In
	;	In = (A~>As)
	->	Out = (A~>Tail),
		concatseq(As,B,Tail)
	;	Out = (In ~> B)
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
			Body,
			share(Head,Head,Head)
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
	retract(todo(OldSeq,In,Depth,_Parent)),
	%assert(done(OldSeq,In,Depth,Parent)),
	unfold_step(In,Unfolded),
	%format("unfolded: ~W~n",[Unfolded,[module(concurrent_tests)]]),	
	NewDepth is Depth +1,	
	forall(
		detseq(Unfolded,DetSeq),
		process_deterministic_sequence(DetSeq,Module,Head,NewDepth,OldSeq)
		%(	flag(seq_counter,SeqNum,SeqNum+1),
		%	format("~w:~n",[SeqNum]),
		%	write_deterministic_sequence(DetSeq)
		%)
	),
	!.
step(_,_):-
	debug(concurrent_tests,"no more steps.~n",[]).

	
process_deterministic_sequence(Seq,Module,Head,Depth,Parent):-
	flag(seq_counter,SeqNum,SeqNum+1),	
	%format("~n~nSequence: ~w, Depth: ~w, Parent: ~w~n",[SeqNum,Depth,Parent]),
	%write_deterministic_sequence(Seq),
	catch(
		Module:ignore(setup(Head)),
		SetupError,
		(	format("~n~nError during Setup: ~w~nFailing sequence: ~w Depth ~w, Parent ~w ~n",[SetupError,SeqNum,Depth,Parent]),
			write_deterministic_sequence(Seq)
		)
	),
	(	var(SetupError)
	->	process_literals(Seq,Outcome)
	;	true
	),	
	catch(
		Module:ignore(teardown(Head)),
		TeardownError,
		(	format("~n~nError during Teardown: ~w~nFailing sequence: ~w, Depth ~w, Parent ~w ~n",[TeardownError,SeqNum,Depth,Parent]),
			write_deterministic_sequence(Seq)
		)
	),
	(	var(SetupError)
	->	process_outcome(Outcome,Seq,SeqNum,Depth,Parent)
	;	true
	),
	(	var(TeardownError), 
		var(SetupError)
	->	true	
	;	throw(stop)
	),	
	!.
process_deterministic_sequence(Seq,Module,Head,Depth,Parent):-
	throw(failed(process_deterministic_sequence(Seq,Module,Head,Depth,Parent))).
	
process_outcome(Outcome,Seq,SeqNum,Depth,Parent):-
	(	Outcome == incomplete
	->	max_depth(Max),
		(	Depth > Max	
		->	format("~n~nSequence exceeds depth limit: ~w ~n",[SeqNum]),
			write_deterministic_sequence(Seq)
		;	assert(todo(SeqNum,Seq,Depth,Parent))
			%format("Sequence defered: ~w, Depth: ~w, Parent: ~w~n",[SeqNum,Depth,Parent])
		)
	;	Outcome == true
	->	format("~n~nSequence succeeded: ~w, Depth: ~w, Parent: ~w. ~n",[SeqNum,Depth,Parent]),
		write_deterministic_sequence(Seq)
	;	Outcome == fail
	->	format("~n~nSequence discarded: ~w, Depth: ~w, Parent: ~w. ~n",[SeqNum,Depth,Parent]),
		write_deterministic_sequence(Seq)
	;	format("~n~nSequence produced error: ~w~nFailing sequence: ~w, Depth: ~w, Parent: ~w~n",[Outcome,SeqNum,Depth,Parent]),
		write_deterministic_sequence(Seq)
	).


process_literals(Part,Outcome):-
	(	Part = (Cx:sequence(Head) ~> B)
	->	check_existence(Cx,Head,Outcome)
	;	Part = (Cx:sequence(Head))
	->	check_existence(Cx,Head,Outcome)
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

check_existence(Cx,Head,Outcome):-
	(	Cx:clause(sequence(Head,_,_),_,_)
	->	Outcome=incomplete
	;	Outcome=error(existence_error(sequence, Head), _)
	).

execute_literal(Lit0,Outcome):-
	debug(concurrent_tests,"literal: ~t~W ",[Lit0,[module(concurrent_tests)]]),
	Lit0 = Cx:Lit,
	execute_literalX(Lit,Cx,Outcome),
	debug(concurrent_tests,"outcome: ~t~w~n",[Outcome]).

execute_literalX(check(Goal),Cx,Outcome):-
	!,
	(	Cx:catch(Goal,Error,true)
	->	(	var(Error)
		->	Outcome=true
		;	Outcome=Error
		)
	;	Outcome=error(check_failed(Goal))
	).
execute_literalX(Goal,Cx,Outcome):-	
	(	Cx:catch(Goal,Error,true)
	->	(	var(Error)
		->	Outcome=true
		;	Outcome=Error
		)
	;	Outcome=fail
	).

write_deterministic_sequence(Seq):-
	copy_term(Seq,Cpy),
	numbervars(Cpy,0,_),
	write_deterministic_sequence_X(Cpy).
		
write_deterministic_sequence_X((Seq~>Seqs)):-
	!,
	format("~t~W~n ",[Seq,[module(concurrent_tests),portray(true)]]),
	write_deterministic_sequence_X(Seqs).
write_deterministic_sequence_X(Seq):-
	format("~t~W~n ",[Seq,[module(concurrent_tests),portray(true)]]).
	
	




	
