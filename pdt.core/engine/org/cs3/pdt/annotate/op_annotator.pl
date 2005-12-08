:-module(op_annotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).



term_pre_annotation_hook(_,OpModule,InTerm,OutTerm):-
    pdt_strip_annotation(InTerm,':-'(op(Precedence,Type,Name)),(TopAnot,ArgAnot)),
    pdt_splice_annotation(':-'(op(Precedence,Type,Name)),([declares_op(op(Precedence,Type,Name))|TopAnot],ArgAnot),OutTerm),
    op(Precedence,Type,OpModule:Name).
file_pre_annotation_hook(_,_,Terms,InAnos,[declares_ops(Ops)|InAnos]):-
    collect_ops(Terms,Ops).


collect_ops([],[]).
collect_ops([H|T],[op(Precedence,Type,Name)|TOps]):-
    pdt_strip_annotation(H,':-'(op(Precedence,Type,Name)),_),
	!,
	collect_ops(T,TOps).
collect_ops([_|T],TOps):-
	collect_ops(T,TOps).

