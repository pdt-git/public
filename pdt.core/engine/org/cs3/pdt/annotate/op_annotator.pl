/* $LICENSE_MSG$(ld) */

:-module(op_annotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).

:-pdt_annotator([interleaved,file],[]).

interleaved_annotation_hook(_,OpModule,InTerm,OutTerm):-
    pdt_aterm_strip_annotation(InTerm,Term,(TopAnot,ArgAnot)),
    nonvar(Term),
    Term=':-'(op(Precedence,Type,Name)),
    pdt_aterm_splice_annotation(':-'(op(Precedence,Type,Name)),([declares_op(op(Precedence,Type,Name))|TopAnot],ArgAnot),OutTerm),
    op(Precedence,Type,OpModule:Name).
file_annotation_hook([File|_],_,InAnos,OutAnos):-
    pdt_file_record_key(term,File,Key),
    collect_ops(Key,Ops),
    (	Ops==[]
    ->	OutAnos=InAnos
    ;	OutAnos=[declares_ops(Ops)|InAnos]
    ).


collect_ops(Key,Ops):-
    findall(op(Precedence,Type,Name),
    	(	pdt_file_record(Key,ATerm),
    		pdt_aterm_strip_annotation(ATerm,':-'(op(Precedence,Type,Name)),_)
    	),Ops
    ).
    

