:-module(pdt_workspace,[
	pdt_file_added/1,
	pdt_file_changed/1,
	pdt_file_removed/1
	]
).

:-use_module(library('builder/builder')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).


	

pdt_file_added(Abs):-
    pdt_invalidate_target(file(Abs)).    
    
pdt_file_changed(Abs):-
    pdt_invalidate_target(file(Abs)).
	
pdt_file_removed(Abs):-
    pdt_invalidate_target(file(Abs)).
    
    