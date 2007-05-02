:- module(toplevel_processor,
	[	pdt_process_toplevels/1,
		pdt_forget_toplevels/1
	]
).

:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('spike/pef_base')).

pdt_process_toplevels(Spec):-
    pdt_file_ref(Spec,Ref)