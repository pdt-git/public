:-module(export_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
file_pre_annotation_hook(_,_,[H|_],In,[defines_module(Module), exports(Exports)|In]):-
    pdt_strip_annotation(H,':-'(module(Module,Exports)),_).
