:- dynamic foo/3.
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- register_annotator(library('/org/cs3/pdt/annotate/op_annotator')).
:- register_annotator(library('/org/cs3/pdt/annotate/fileref_annotator')).
:- register_annotator(library('/org/cs3/pdt/annotate/export_annotator')).
:- register_annotator(library('/org/cs3/pdt/annotate/member_annotator')).

some_clause(A,B):-
    do(this), 
    
    now(do(that)).