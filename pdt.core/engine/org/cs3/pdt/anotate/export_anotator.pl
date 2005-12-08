:-module(export_anotator,[]).


file_anotation_hook(_,_,[H|_],In,[defines_module(Module), exports(Exports)|In]):-
    pdt_strip_anotation(H,':-'(module(Module,Exports)),_).
