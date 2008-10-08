/*
http://butterbur03.iai.uni-bonn.de:8080/jira/browse/PDT-302
https://sewiki.iai.uni-bonn.de/research/pdt/developers/architecture/subsystems/pdtcore/buildsystem/start
*/
:- module(concurrent_tests,
	[	op(1145, xfx, else), % comitted choice
		op(1140, xfy, #), % interleave
		op(1135, xfy, ?), % nondet. choice		
		op(1130, xfy, ~>), % concat
		run_concurrent_test/1
	]
).

:- module_transparent 
	run_concurrent_test/1.

/*
The todo queue stores deferred branches.

todo(Num,Seq,TotalDepth,Stack,Marks)

Num: number of the deferred branch
Seq: the remaining sequence
TotalDepth: the depth of the branch.
Stack: the path from the branch up to the root.
Marks: a list of marked ancestors.

*/
:- dynamic todo/5.

/*
The dynamic predicate branch_marked(Num) succeeds if
Num is the number of a marked branch. There shouldn't be more
than one fact for the same branch.
*/
:- dynamic branch_marked/1.

/*
The dynamic predicate branch_deferred(Num) succeeds if a sub-branch
of a marked branch is deferred. 
There may be more than one facts for each marked branch -- we use them
as a counter.
*/
:- dynamic branch_deferred/1.

run_concurrent_test(Head):-
	tell('testresults.log'),
	context_module(Module),
	concurrent_tests:call_cleanup(run_concurrent_test_X(Module,Head),told).

run_concurrent_test_X(Module,Head):-
	flag(seq_counter,_,0),	
	add_module_prefix(sequence(Head),Module,Seq),	
	assert(todo([],Seq,0,[],[])),
	process_queue(Module).



max_total_depth(Module,D):-
	(	Module:clause(max_total_depth(_),_,_)
	->	Module:max_total_depth(D)
	;	D=50
	).
max_step_depth(Module,D):-
	(	Module:clause(max_step_depth(_),_,_)
	->	Module:max_step_depth(D)
	;	D=10
	).


/*
 * preprocessing: 
 *   - push down context module into literals
 *   - replace sequence references with equivalent meta literals
 */
add_module_prefix(In,Module,Out):-
	(	In=nil
	->	Out=In
	;	member(In,[nop(_),mark_branch(_),unmark_branch(_),store(_),recall(_)])
	->	Out=In
	;	In=(A~>B)	
	->	Out=(AA~>BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=(A,B)	
	->	Out=(AA~>BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=(A?B)
	->	Out=(AA?BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=(A else B)
	->	Out=(AA else BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=(A#B)		
	->	Out=(AA#BB),
		add_module_prefix(A,Module,AA),
		add_module_prefix(B,Module,BB)
	;	In=wait(Cond,Seq)		
	->	my_strip_module(Module:Cond,Module1,Cond1),
		Out=wait(Module1:Cond1,Seq1),		
		add_module_prefix(Seq,Module,Seq1)
	;	In = meta(Goal0, Op, S)
	->	my_strip_module(Module:Goal0,Module1,Goal),
		Out = meta(Module1:Goal,Op,S)
	;	In = meta(Goal0, Op, S,Share)
	->	my_strip_module(Module:Goal0,Module1,Goal),
		Out = meta(Module1:Goal,Op,S,Share)		
	;	In = sequence(Head0)
	->	my_strip_module(Module:Head0,Module1,Head),
		% make sure the sequence exists. If it does not, 
		% insert an error in the sequence.
		(	Module1:clause(sequence(Head,_,_),_,_)
		->	Out = meta(
				Module1:sequence(Head,Cx, Body),
				(?), 
				Cx:Body,
				share(Head,Head,Head)
			)
		;	Out=(Module1:throw(error(existence_error(sequence, Module1:Head), _)))
		)
	;	In = (_:_)
	->	my_strip_module(In,SubSeqMod,SubSeq),
		add_module_prefix(SubSeq,SubSeqMod,Out)			
	;	my_strip_module(Module:In,Module1,In1),
		Out=(Module1:In1)
	).



is_meta_literal(meta(_,_,_)).
is_meta_literal(meta(_,_,_,_)).
is_meta_literal(sequence(_)).


unfold_meta(meta(Goal0,Op,Template),Seq):-
	my_strip_module(Goal0,GoalCx,Goal),	
	GoalCx:findall(Template,Goal,Instances),
	combine(Instances,Op,Seq0),
	add_module_prefix(Seq0,GoalCx,Seq).
unfold_meta(meta(Goal0,Op,Template,share(In,Init,Out)),Seq):-
	my_strip_module(Goal0,GoalCx,Goal),	
	GoalCx:findall(Template-In-Out,Goal,Instances),
	combine(Instances,Op,Init,Out,Seq0),
	add_module_prefix(Seq0,GoalCx,Seq).
		
combine([],_,nil).
combine([Seq|Seqs],Op,Out):-
	(	Seqs==[]
	->	Out=Seq
	;	Out =.. [Op,Seq,Seqs2],
		combine(Seqs,Op,Seqs2)
	).
	
combine([],_,InOut,InOut,nil).
combine([Inst-Input-Next|More],Op,Input, Output,Sequence):-	
	(	More == []
	->	Output=Next, 
		Sequence=Inst
	;	Sequence =..[Op,Inst,MoreSequence],
		combine(More,Op,Next,Output,MoreSequence)
	).







/******************************
 *	new meta-interpreter.
 * does unfolding/exploration on the fly.
 * still uses iterative deepening.
 */
 
seq_step_tail( (A,nil), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (nil,A), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).	
seq_step_tail( (A~>nil), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (nil~>A), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).	
seq_step_tail( (A#nil), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (nil#A), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (A?nil), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (nil?A), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (A else nil), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).
seq_step_tail( (nil else A), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,Tail).

%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	

seq_step_tail((A,B),Step,Tail):-
	!,
 	seq_step_tail(A,AStep,ATail),
	seq_step_tail(B,BStep,BTail),
	( 	AStep==A, 
		ATail==nil %A is atomic
	-> 	Step=(A,BStep),
		Tail=BTail
	;	BStep==B, 
		BTail==nil %B is atomic
	->	Step = AStep,
		Tail = (ATail,B)
	;	throw(error(not_a_sequence))
	). 
seq_step_tail( (A~>B), Step, Tail):-
	!,	
	seq_step_tail(	A, Step,TailA ),
	seq_concat(TailA, B,Tail).		
seq_step_tail( (A?B), Step, Tail):-
	!,
	next_seqnum(Num),
	(	Step = (store(Num),Step0),
		seq_step_tail(A,Step0,Tail)		
	;	Step = (recall(Num),Step0),
		seq_step_tail(B,Step0,Tail)		
	).
seq_step_tail( (A else B), Step, Tail):-
	!,	
	next_seqnum(Num),	
	(	Rewritten=(store(Num),mark_branch(Num),A,unmark_branch(Num))		
	;	Rewritten = (add_mark(Num),recall(Num),join_branch(Num),unmark_branch(Num),B)		
	),
	seq_step_tail(Rewritten,Step,Tail).	
seq_step_tail( (A#B), Step, Tail):-
	!,
	% Interleaving: we can
	% nondeterministically choose decompositions 
	% for both A and B.
	% Then we can nondeterministically choose whether we want to
	% continue with A or B.
	% Instead of doing this manually, we can rewrite the
	% the formula using meta-unfolding.	
	Rewritten=
		meta(
			(	concurrent_tests:seq_step_tail(A,TStep,TTail), TOther=B
			;	concurrent_tests:seq_step_tail(B,TStep,TTail), TOther=A
			),
			(?),
			(TStep~>(TTail#TOther)),
			share(_,_,_)
		), 	
	
	seq_step_tail(Rewritten,Step,Tail).
		
seq_step_tail(wait(Condition,Seq),Step,Tail):-
	!,
	Condition,
	seq_step_tail(Seq,Step,Tail).
seq_step_tail( Literal, Step, Tail):-
	(	is_meta_literal(Literal)
	->	unfold_meta(Literal,Unfolded),
		% FIXME: this recursion does not check depth limit.
		% This may cause inifinite recursion. See PDT-314
		seq_step_tail(Unfolded,Step,Tail)
	;	Step=Literal, 
		Tail = nil
	).
 

seq_concat(In,B,Out):-
	(	In == nil
	->	Out = B
	;	B == nil
	->  Out = In
	;	In = (A~>As)
	->	Out = (A~>Tail),
		seq_concat(As,B,Tail)
	;	Out = (In ~> B)
	).	

% only used for debugging

detseq(Seq,FilteredSeq):-
	detseq2(Seq,DetSeq),
	filter_seq(DetSeq,FilteredSeq).
	
detseq2(Seq,DetSeq):-
	(	Seq == nil
	->	DetSeq=Seq
	;	DetSeq=(Step~>DetTail),
		seq_step_tail(Seq,Step,Tail),
		detseq2(Tail,DetTail)
	).

process_queue(Module):-
	repeat,
		(	next_sequence(Seq,Num,Stack,TotalDepth,Marks)
		->	process(Num,Seq,Module,Stack,TotalDepth,Marks),
			fail
		;	true
		),	
	!.

process(Num,Seq,Module,Stack,TotalDepth,Marks0):-
	filter_marks__resume(Marks0,Marks),
	catch(
		(	Module:fixture_setup(Num)
		->	true
		;	SetupError=error(failed(Module:fixture_setup(Num)))
		),
		SetupError,
		report_setup_error(SetupError,Seq,Stack)		
	),
	(	var(SetupError)
	->	process_sequence(Seq,Module,Stack,0,TotalDepth,Marks)
	;	true
	),		
	catch(
		(	Module:fixture_teardown(Num)
		->	true
		;	TeardownError=error(failed(Module:fixture_teardown(Num)))
		),
		TeardownError,
		report_teardown_error(TeardownError,Seq,Stack)
	),	
	(	var(TeardownError), 
		var(SetupError)
	->	true	
	;	throw(stop)
	).
	



process_sequence(Seq,Module,Stack,StepDepth,TotalDepth,Marks):-	
	max_total_depth(Module,MaxTotalDepth),
	max_step_depth(Module,MaxStepDepth),
	(	Seq == nil
	->	report_sequence_succeeded(Stack)
	;	TotalDepth > MaxTotalDepth
	->	report_depth_limit_exceeded(Seq,Stack)
	;	StepDepth > MaxStepDepth
	->	defer_sequence(Seq,Module,Stack,TotalDepth,Marks,Num),
		report_sequence_defered(Num,Seq,Stack)
	;	process_sequence_X(Seq,Module,Stack,StepDepth,TotalDepth,Marks)
	).

process_sequence_X(Seq,Module,Stack,StepDepth,TotalDepth,Marks):-	
	(	seq_step_tail( Seq,Step,Tail)
	*-> process_sequence_XX(Step,Tail,Seq,Module,Stack,StepDepth,TotalDepth,Marks)
	;	report_dead_lock(Seq,Stack)
	).
	
process_sequence_XX(Step,Tail,Seq,Module,Stack,StepDepth,TotalDepth,Marks):-	
	process_step(Step,Module,Marks,NextMarks,Outcome),
	(	Outcome == fail
	->	report_sequence_discarded(Seq,Module,TotalDepth,Stack)		
	;	Outcome == true
	->	NextStepDepth is StepDepth + 1,
		NextTotalDepth is TotalDepth + 1,
		process_sequence(Tail,Module,[Step|Stack],NextStepDepth,NextTotalDepth,NextMarks)
	;	Outcome == defer
	->	defer_sequence(Seq,Module,Stack,TotalDepth,Marks,StoreNum),
		report_sequence_defered(StoreNum,Seq,Stack)
	;	report_sequence_produced_error(Seq,Module,TotalDepth,Stack,Outcome)
	).

process_step(Step,Module,Marks,NextMarks,Outcome):-
	(	Step = (A,B)
	->	process_step(A,Module,Marks,TmpMarks,TmpOutcome),
		(	TmpOutcome==true
		->	process_step(B,Module,TmpMarks,NextMarks,Outcome)
		;	NextMarks=TmpMarks,
			Outcome=TmpOutcome
		)
	;	Step = nop(_)
	->	NextMarks=Marks,
		Outcome = true
	;	Step == nil
	->	NextMarks=Marks,
		Outcome = true
	;	Step = store(Num)
	->	NextMarks=Marks,
		catch(
			(	Module:fixture_store(Num)
			->	Outcome= true
			;	Outcome= error(failed(Module:fixture_store(Num)))
			),
			Outcome,
			true
		)
	;	Step = recall(Num)
	->	debug(concurrent_tests, "~n~nRetrying at ~w, with Marks=~w~n",[Num,Marks]),
		filter_marks__cp(Marks,NextMarks),		
		catch(
			(	Module:fixture_setup(Num)
			->	Outcome= true
			;	Outcome= error(failed(Module:fixture_setup(Num)))
			),
			Outcome,
			true
		)
	;	Step = mark_branch(Num)
	->  (	branch_marked(Num)
		->	NextMarks=Marks
		;	NextMarks=[Num|Marks],
			assert(branch_marked(Num))
		),
		Outcome = true
	; 	Step = add_mark(Num)
	->	NextMarks=[Num|Marks],
		Outcome=true			
	;	Step = unmark_branch(Num)
	->  retractall(branch_marked(Num)),
		retractall(branch_deferred(Num)),
		debug(concurrent_tests, "unmark_branch(~w): removing all branch_marked(~w) and branch_deferred(~w) facts.~n",[Num,Num,Num]),
		filter_marks__cp(Marks,NextMarks),
		Outcome = true		
	;	Step = join_branch(Num)
	->	NextMarks = Marks,
		(	branch_marked(Num)
		->	debug(concurrent_tests, "join_branch(~w): The branch is still marked. ~n",[Num]),
			(	branch_deferred(Num)
			->	debug(concurrent_tests, "join_branch(~w): some subbranch is still in the queue, deferring the current branch. ~n",[Num]),
				Outcome= defer
			;	Outcome= true,
				debug(concurrent_tests, "join_branch(~w): no subbranch queued, continuing on current branch. ~n",[Num])
			)
		;	Outcome=fail,
			debug(concurrent_tests, "join_branch(~w): The branch is not marked (any more). Discarding current branch.~n",[Num])
		)
	;	NextMarks=Marks,
		execute_literal(Step,Outcome)
	).


defer_sequence(Seq,Module,Stack,TotalDepth,Marks,Num):-
	next_seqnum(Num),
	debug(concurrent_tests, "defer_sequence: Num=~w, Marks=~w~n",[Num,Marks]),
	forall(
		member(Mark,Marks),
		(	assert(branch_deferred(Mark)),
			count(branch_deferred(Mark),Count),
			debug(concurrent_tests, "Added one branch_deferred(~w) fact. There are now ~w.~n",[Mark,Count])
		)
	),
	Module:fixture_store(Num),
	assert(todo(Num,Seq,TotalDepth,Stack,Marks)).

filter_marks__resume([],[]).
filter_marks__resume([Mark|Marks],FilteredMarks):-
	(	branch_marked(Mark)
	->	debug(concurrent_tests, "filter_marks__resume: The branch ~w is still marked: keeping the mark.~n",[Mark]),
		count(branch_deferred(Mark),Count),
		once(retract(branch_deferred(Mark))),
		debug(concurrent_tests, "filter_marks__resume: Removed one of ~w branch_deferred(~w) facts.~n",[Count,Mark]),			
		FilteredMarks=[Mark|MoreFilteredMarks]
	;	debug(concurrent_tests, "filter_marks__resume: The branch ~w is not marked any more: dropping the mark.~n",[Mark]),
		FilteredMarks=MoreFilteredMarks	
	),
	filter_marks__resume(Marks,MoreFilteredMarks).

filter_marks__cp([],[]).
filter_marks__cp([Mark|Marks],FilteredMarks):-
	(	branch_marked(Mark)
	->	debug(concurrent_tests, "filter_marks__cp: The branch ~w is still marked: keeping the mark.~n",[Mark]),
		FilteredMarks=[Mark|MoreFilteredMarks]		
	;	debug(concurrent_tests, "filter_marks__cp: The branch ~w is not marked any more: dropping the mark.~n",[Mark]),
		FilteredMarks=MoreFilteredMarks	
	),
	filter_marks__cp(Marks,MoreFilteredMarks).



next_sequence(Seq,Num,Stack,TotalDepth,Marks):-
	retract(todo(Num,Seq,TotalDepth,Stack,Marks)),
	report_sequence_resumed(Num,Marks).
	
next_seqnum(Num):-	
	flag('$concurrent_tests__defered_sequence_counter',Num,Num+1).	


report_dead_lock(Seq,Stack):-
	format("~n~nSequence locked, no more executable steps: ~nFailing Path:~n", []),
	reverse(Stack,Path),
	write_path(Path),	
	format("~nNothing executable in remaining sequence: ~n",[]),
	write_sequence(Seq).
report_sequence_produced_error(Seq,_Module,_Depth,Stack,Error):-
	format("~n~nSequence produced error: ~w~nFailing Path:~n", [Error]),
	reverse(Stack,Path),
	write_path(Path),	
	format("~nRemainder of the failing sequence: ~n",[]),
	write_sequence(Seq).
report_sequence_discarded(_Seq,_Module,_Depth,_Stack).%:-	
	%debug(concurrent_tests, "~n~nSequence discarded: ~nPath:~n", []),
	%reverse(Stack,Path),
	%write_path(Path),
	%debug(concurrent_tests, "~nDiscarded remainder:~n",[]),
	%write_sequence(Seq).
report_sequence_defered(_Num,_Seq,_Stack).%:-
	%debug(concurrent_tests, "~n~nSequence defered: ~w~nPath:~n", [Num]),
	%reverse(Stack,Path),
	%write_path(Path),
	%debug(concurrent_tests, "~nRemaining sequence:~n",[]),
	%write_sequence(Seq).
report_sequence_succeeded(Stack):-
	format("~n~nSequence succeeded:~n",[]),
	reverse(Stack,Path),
	write_path(Path).
report_depth_limit_exceeded(Seq,Stack):-
	format("~n~nSequence exceeds depth limit: ~nPath:~n", []),
	reverse(Stack,Path),
	write_path(Path),
	format("~nRemaining sequence:~n",[]),
	write_sequence(Seq).
report_setup_error(Error,Seq,Stack):-
	format("~n~nError during fixture setup: ~w~nFailing Path:~n", [Error]),
	reverse(Stack,Path),
	write_path(Path),
	format("~nRemainder of the failing sequence:~n",[]),
	write_sequence(Seq).
report_teardown_error(Error,Seq,Stack):-
	format("~n~nError during fixture teardown: ~w~nFailing Path:~n", [Error]),
	reverse(Stack,Path),
	write_path(Path),
	format("~nRemainder of the failing sequence:~n",[]),
	write_sequence(Seq).	
report_sequence_resumed(_Num,_Marks).%:-
	%debug(concurrent_tests, "~n~nResuming Sequence ~w, Marks=~w~n",[Num,Marks]).	

execute_literal(Lit0,Outcome):-
	debug(concurrent_tests,"literal: ~t~W ",[Lit0,[module(concurrent_tests)]]),
	Lit0 = Cx:Lit,
	execute_literalX(Lit,Cx,Outcome),
	debug(concurrent_tests,"outcome: ~t~w~n",[Outcome]),
	!.
execute_literal(Lit0,Outcome):-
	throw(failed(execute_literal(Lit0,Outcome))).	

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

:- module_transparent my_strip_module/3.
	
my_strip_module(A,Mod,Goal):-	
	context_module(Candidate),
	concurrent_tests:my_strip_module(A,Candidate,Mod,Goal).
my_strip_module(A,Cand,Mod,Goal):-
	(	var(A)
	->	Mod=Cand,Goal=A
	;	A=NextCand:NextA
	->	my_strip_module(NextA,NextCand,Mod,Goal)
	;	Mod=Cand,Goal=A
	).

write_sequence(Seq):-
	copy_term(Seq,Seq1),
	numbervars(Seq1,0,_),
	filter_seq(Seq1,Seq2),
	format("   ",[]),
	write_sequence(Seq2,100000,""),
	nl,
	!.  	
write_sequence(Seq):-
	throw(failed(write_sequence(Seq))).
	
write_sequence(Seq,P0,I):-
	(	is_literal(Seq)
	->	write_literal(Seq)
	;	write_complex(Seq,P0,I)
	),
	!.
write_sequence(Seq,P0,I):-
	throw(failed(write_sequence(Seq,P0,I))).
is_literal(Seq):-
	(	var(Seq)
	->	true
	;	memberchk(Seq,[(A,B),(A~>B),(A#B),(A?B),(A else B),wait(A,B)])
	->	fail
	;	true
	).

write_literal(Seq):-
	format("~W",[Seq,[module(concurrent_tests),numbervars(true)]]).
		
write_complex(Seq,P0,I0):-
	Seq =..[ Junctor,Left,Right],
	(	current_op(P,_,concurrent_tests:Junctor)
	->	pad_junctor(Junctor,Pad),
		LeftP is P - 1,	
		(	P > P0
		->	append(I0,"   ",I),
			format("(  ",[]),
			write_sequence(Left,LeftP,I),
			format("~n~s~w~s",[I,Junctor,Pad]),
			write_sequence(Right,P,I),
			format("~n~s)",[I])
		;	write_sequence(Left,LeftP,I0),
			format("~n~s~w~s",[I0,Junctor,Pad]),
			write_sequence(Right,P,I0)
		)
	;	LeftP = 999, 
		RightP = 999,
		append(I0,"   ",IMe),
		append(IMe,"   ",IKids),
		format("~w(~n",[Junctor]),
		format("~s",[IKids]),
		write_sequence(Left,LeftP,IMe),
		format(",~n",[]),
		format("~s",[IKids]),
		write_sequence(Right,RightP,IMe),
		format("~n~s)",[IMe])	
	).

pad_junctor((,),"  ").		
pad_junctor((#),"  ").
pad_junctor((?),"  ").
pad_junctor((else)," ").
pad_junctor((~>)," ").

write_path(Path):-
	combine(Path,(~>),PathSeq),
	write_sequence(PathSeq).

	
filter_seq(In,Out):-
	(	is_literal(In)
	->	(	member(In,[nop(_),mark_branch(_),unmark_branch(_),store(_),recall(_)])
		->	Out = nil
		;	Out = In
		)
	;	In =.. [Fun,LH,RH],
		filter_seq(LH,LOut),
		filter_seq(RH,ROut),
		(	ROut==nil
		->	Out = LOut
		;	LOut==nil
		->	Out = ROut
		;	Out =.. [Fun,LOut,ROut]
		)
	).
%write_path(Path):-
%	copy_term(Path,Path1),
%	numbervars(Path1,0,_),
%	write_path_X(Path1).
		
%write_path_X([]).
%write_path_X([Literal|Literals]):-
%	write_literal(Literal),
%	nl,
%	write_path_X(Literals).



% code copied from the occurs lib.
count(Goal, Count) :-
	State = count(0),
	(   Goal,
	    arg(1, State, N0),
	    N is N0 + 1,
	    nb_setarg(1, State, N),
	    fail
	;   arg(1, State, Count)
	).

count_paths(Seq,Module,Depth,Count):-
	max_total_depth(Module,MaxDepth),
	count(truncate_path(Seq,Depth,MaxDepth,_Path),Count).

	
truncate_path(Seq,CurrentDepth,MaxDepth,Path):-
	(	CurrentDepth>MaxDepth
	->	Path=[truncate]
	;	Seq=nil
	->	Path=[]
	;	seq_step_tail(Seq,Step,Tail),
		NextDepth is CurrentDepth+1,
		Path=[Step|TailPath],
		truncate_path(Tail,NextDepth,MaxDepth,TailPath)
	).
		
spyme.		