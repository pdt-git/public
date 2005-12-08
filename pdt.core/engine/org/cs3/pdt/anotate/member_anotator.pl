module(member_anotator,[]).

term_pre_anotation_hook(_,_,_,_).

file_anotation_hook(_,_,[H|_],In,[defines_module(Module), exports(Exports)|In]):-
    pdt_strip_anotation(H,':-'(module(Module,Exports)),_).
file_anotation_hook(_,_,_,_,_).

term_post_anotation_hook(_,_,_,_,_).