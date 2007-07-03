:-module(pdt_workspace,[
	pdt_file_added/1,
	pdt_file_changed/1,
	pdt_file_removed/1
	]
).

:-use_module(library('builder/builder')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).

update_file(Abs):-
	(	exists_file(Abs)
    ->	get_pef_file(Abs,_)
    ;	pef_file_retractall([path=Abs])
    ).

pdt_file_added(Abs):-
    update_file(Abs),    
    pdt_invalidate_target(file(Abs)).
    
pdt_file_changed(Abs):-
    update_file(Abs),
	pdt_invalidate_target(file(Abs)).
	
pdt_file_removed(Abs):-
    update_file(Abs),
    pdt_invalidate_target(file(Abs)).
    
pdt_builder:invalidate_hook(file(_)):-
	pdt_invalidate_target(workspace).    