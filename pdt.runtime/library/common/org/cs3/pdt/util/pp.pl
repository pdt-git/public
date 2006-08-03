:- use_module(pdt_util).
:- use_module(pdt_util_aterm).
:- use_module(pdt_util_map).
:- use_module(pdt_util_term_position).
comment_positions([],[]).
comment_positions([CPos-_|Cs],[Position|Positions]):-
    stream_position_data(char_count,CPos,Position),
    comment_positions(Cs,Positions).


comments_map(Comments,Map):-
    pdt_map_empty(Map0),
    comments_map(Map0,Comments,Map).
    
comments_map(In,[],In).
comments_map(In,[CPos-Comment|Cs],Out):-
    stream_position_data(char_count,CPos,Position),
    pdt_map_put(In,Position,Comment,Next),
    comments_map(Next,Cs,Out).

embed_comments(ATerm,[],ATerm).
embed_comments(ATermIn,CPositions,ATermOut) :-
	pdt_term_annotation(ATermIn,SubTerm,Annos),
	nonvar(SubTerm),
	!,
	pdt_member(position(TPosition-_),Annos),
	pdt_chop_before(TPosition,CPositions,Before,After),
	
	SubTerm=..[Functor|ArgsIn],
	embed_comments_X(ArgsIn,After,ArgsOut),
	SubTermOut=..[Functor|ArgsOut],
	pdt_term_annotation(ATermOut,SubTermOut,[comments(Before)|Annos]).
embed_comments(ATerm,_,ATerm).	
	

%comment before term
embed_comments_X([],_,[]).
embed_comments_X([ArgIn|ArgsIn],CPositions,[ArgOut|ArgsOut]):-
    embed_comments(ArgIn,CPositions,ArgOut),
    embed_comments_X(ArgsIn,_,ArgsOut).


    

		
	

reformat_comment(Comment) :-
	atom_chars(Comment, Chars),
	Chars = [First|_],
	( First = '%' ->
		write(Comment), nl
	;
		append([_,_|Nub], [_,_], Chars),
		format("\n\n/*\n  "),
		reformat_comment_(Nub),
		format("\n*/\n\n")
	).

reformat_comment_([]).
reformat_comment_([C|Cs]) :-
	( C = '\n' ->
		( Cs \== [] -> format("\n  ") ; true )
	;
		format(C)
	),
	reformat_comment_(Cs).


plpp(File) :-
    pdt_file_ref(File,FileRef),
	open(File, read, In),
	read_term(In, Term, [subterm_positions(Pos),variable_names(Names),comments(Comments)]),
%	embed_comments(Comments),
	term_predicate(Term, Pred),
	(	Term==end_of_file
	->	true
	;	wrap_term(Term,Pos,FileRef,0,ATerm0,_),
		comments_map(Comments,Map),
	    b_setval(comments,Map),
	    comment_positions(Comments,Positions),
		embed_comments(ATerm0,Positions,ATerm),
		call_cleanup(pp(ATerm, Names, Pred, In), close(In))
	).

term_predicate(ATerm, Pred) :-
    pdt_aterm(ATerm),
    !,
    pdt_strip_annotation(ATerm,Term,_),
    term_predicate(Term,Pred).
term_predicate(Term, Pred) :-
	functor(Term, Functor, Arity),
	( Functor == (:-) ->
		% rule or declaration
		arg(1, Term, Head),
		functor(Head, F, A),
		( F == module, A =:= 2 ->
			arg(2, Head, PublicList),
			module_ops(PublicList)
		; F == op, A =:= 3 ->
			call(Head)
		; F == use_module, A =:= 1 ->
			call(Head)
		;
			true
		),
		Pred = F/A
	;
		% fact
		Pred = Functor/Arity
	).

module_ops([]).
module_ops([P|Ps]) :-
	( functor(P, op, _) -> call(P) ; true ),
	module_ops(Ps).


%pp(end_of_file, _, _, _) :- !.
pp(ATerm, Names, Previous, In) :-
	term_predicate(ATerm, Pred),
	( Pred \== Previous -> nl, nl ; true ),
	bind_names(Names),
	writeln(ATerm),
	portray_clause(ATerm),
	read_term(In, Term2, [subterm_positions(Pos),variable_names(Names2),comments(Comments)]),
	( Comments \== [] -> nl ; true ),
	(	Term2==end_of_file
	->	true
	;	pdt_term_annotation(ATerm,_,Annos),
		pdt_member(file_ref(FileRef),Annos),
		pdt_member(last_n(N),Annos),
		M is N+1,
		wrap_term(Term2,Pos,file_ref(FileRef),M,ATerm0,_),
		b_getval(comments,Map0),
		comments_map(Map0,Comments,Map),
		b_setval(comments,Map),
		comment_positions(Comments,Positions),
		embed_comments(ATerm0,Positions,ATerm2),
		
		call_cleanup(pp(ATerm2, Names2, Pred, In), close(In))
	).
	

bind_names([]).
bind_names([Name = Var|T]) :-
	Var = '$VAR'(Name),
	bind_names(T).

portray(ATerm):-
    pdt_aterm(ATerm),
    pdt_term_annotation(ATerm,SubTerm,Annos),
    (	pdt_member(comments(Cs),Annos)
    ->	print_comments(Cs)
    ;	true
    ),    
    print(SubTerm),nl.

print_comments([]).
print_comments([C|Cs]):-
	b_getval(comments,Map),
	pdt_map_get(Map,C,Comment),
	reformat_comment(Comment),
	print_comments(Cs).
    


