:-module(op_anotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/anotate/pdt_anotator')).



term_pre_anotation_hook(_,OpModule,InTerm,OutTerm):-
    pdt_strip_anotation(InTerm,':-'(op(Precedence,Type,Name)),(TopAnot,ArgAnot)),
    pdt_splice_anotation(':-'(op(Precedence,Type,Name)),([declares_op(op(Precedence,Type,Name))|TopAnot],ArgAnot),OutTerm),
    op(Precedence,Type,OpModule:Name).
file_anotation_hook(_,_,Terms,InAnos,[declares_ops(Ops)|InAnos]):-
    collect_ops(Terms,Ops).

term_post_anotation_hook(_,_,_,_,_).

collect_ops([],[]).
collect_ops([H|T],[op(Precedence,Type,Name)|TOps]):-
    pdt_strip_anotation(H,':-'(op(Precedence,Type,Name)),_),
	!,
	collect_ops(T,TOps).
collect_ops([_|T],TOps):-
	collect_ops(T,TOps).

