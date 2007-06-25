:- module(problems,[]).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/interpreter')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).

pdt_builder:build_hook(problems):-
    program_interpreter:my_build_hook(AbsFile).

pdt_builder:invalidate_hook(interprete(_)):-
    pdt_invalidate_target(problems).