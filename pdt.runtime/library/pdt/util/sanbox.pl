
% non-recursive unfolding:
% simultaneously replace all meta-literals
unfold_step(In,Out):-
	(	is_meta_literal(In)
	->	unfold_meta(In,Out)
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



/******************************************************************************* 
 * create deterministic paths.
 * used by the old meta interpreter.
 * Input is an arbitrary (non-deterministic) sequence.
 * Output is a sequence conataining only ~> junctors.
 * Since the output may contain meta-literals, it is not necessarily deterministic,
 * so the name is actually misleading.
 */

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
	->	(detseq(A,DS) ; detseq(B,DS)) %here
	;	S = (A~>B)	
	->	detseq(A,DA),
		detseq(B,DB),
		concatseq(DA,DB,DS)
	;	S = (A # B)
	->	detseq(A,DA),
		detseq(B,DB),
		interleave(DA,DB,DS)
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

/******************************************************************
 * old meta interpreter
 *  - retracts a sequence from the queue
 *  - performs unfolding of meta literals
 *  - iterates all deterministic paths through the sequence
 *    and processes the literal by literal.
 */

step(Module,Head):-
	retract(todo(OldSeq,In,Depth,_Parent)),
	%assert(done(OldSeq,In,Depth,Parent)),
	%format("~n~ndequeued: ~n",[]),
	%write_sequence(In),
	unfold_step(In,Unfolded),
	%format("~n~nunfolded: ~n",[]),
	%write_sequence(Unfolded),	
	%format("unfolded: ~W~n",[Unfolded,[module(concurrent_tests)]]),	
	NewDepth is Depth +1,	
	forall(
		detseq(Unfolded,DetSeq),
		process_deterministic_sequence(DetSeq,Module,Head,NewDepth,OldSeq)
		%(	flag(seq_counter,SeqNum,SeqNum+1),
		%	format("~w:~n",[SeqNum]),
		%	write_sequence(DetSeq)
		%)
	),
	!.
step(_,_):-
	debug(concurrent_tests,"no more steps.~n",[]).


	
process_deterministic_sequence(Seq,Module,Head,Depth,Parent):-
	flag(seq_counter,SeqNum,SeqNum+1),	
	%format("~n~nSequence: ~w, Depth: ~w, Parent: ~w~n",[SeqNum,Depth,Parent]),
	%write_sequence(Seq),
	catch(
		Module:ignore(setup(Head)),
		SetupError,
		(	format("~n~nError during Setup: ~w~nFailing sequence: ~w Depth ~w, Parent ~w ~n",[SetupError,SeqNum,Depth,Parent]),
			write_sequence(Seq)
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
			write_sequence(Seq)
		)
	),
	(	var(SetupError)
	->	process_outcome(Outcome,Module,Seq,SeqNum,Depth,Parent)
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
	
process_outcome(Outcome,Module,Seq,SeqNum,Depth,Parent):-
	(	Outcome == incomplete
	->	max_depth(Module,Max),
		(	Depth > Max	
		->	format("~n~nSequence exceeds depth limit: ~w ~n",[SeqNum]),
			write_sequence(Seq)
		;	assert(todo(SeqNum,Seq,Depth,Parent))%,
			%format("~n~nSequence defered: ~w, Depth: ~w, Parent: ~w~n",[SeqNum,Depth,Parent]),
			%write_sequence(Seq)
		)
	;	Outcome == true
	->	format("~n~nSequence succeeded: ~w, Depth: ~w, Parent: ~w. ~n",[SeqNum,Depth,Parent]),
		write_sequence(Seq)
	;	Outcome == fail
	->	true%format("~n~nSequence discarded: ~w, Depth: ~w, Parent: ~w. ~n",[SeqNum,Depth,Parent]),
		%write_sequence(Seq)
	;	format("~n~nSequence produced error: ~w~nFailing sequence: ~w, Depth: ~w, Parent: ~w~n",[Outcome,SeqNum,Depth,Parent]),
		write_sequence(Seq)
	).


process_literals(Part,Outcome):-
	(	Part = (A ~> _), is_meta_literal(A)
	->	Outcome = incomplete
	;	is_meta_literal(Part)
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

check_existence(Cx,Head,Outcome):-
	(	Cx:clause(sequence(Head,_,_),_,_)
	->	Outcome=incomplete
	;	Outcome=error(existence_error(sequence, Head), _)
	).
		