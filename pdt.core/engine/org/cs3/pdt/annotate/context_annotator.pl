:-module(context_annotator,[]).

:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).

%term_post_annotation_hook(_,_,FileAnnos,InTerm,OutTerm):-
%    pdt_member(defines_module(Module),FileAnnos),
%    pdt_strip_annotation(InTerm,Term,(TopAnn,ArgAnn)),
%    pdt_member(clause_for(Pred),TopAnn),
%    pdt_member(defines_module_transparent
    