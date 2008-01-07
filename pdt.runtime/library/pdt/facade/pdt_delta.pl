:- module(pdt_delta,
	[	pdt_modified_file/2,
		pdt_text_delta/4
	]
).

:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).
:- ensure_loaded(library('builder/targets/delta')).

pdt_modified_file(File,Path):-
    pef_modified_file_query([file=File]),
    get_pef_file(Path,File).
    
pdt_text_delta(File,Start,End,String):-
	pdt_with_targets(
		[delta],
		pef_text_delta_query([file=File,start=Start,end=End,text=String])
	).     