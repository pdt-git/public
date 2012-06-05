:- module(pdt_delta,
	[	pdt_modified_file/2,
		pdt_text_delta/4,
		pdt_resource_delta/3,
		mark_file_renamed/2
	]
).

:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).
:- ensure_loaded(library('builder/targets/delta')).
:- use_module(library('builder/builder')).

pdt_modified_file(File,Path):-
    pef_modified_file_query([file=File]),
    get_pef_file(Path,File).
    
pdt_text_delta(File,Start,End,String):-
	pdt_with_targets(
		[delta],
		pef_text_delta_query([file=File,start=Start,end=End,text=String])
	).
pdt_resource_delta(OldPath,NewPath,Type):-
    pdt_with_targets(
		[delta],
		pef_resource_delta_query([old_path=OldPath,new_path=NewPath,type=Type])		
	).

	
mark_file_renamed(File,_OldPath):-
    pef_renamed_file_query([file=File]),
    !.
mark_file_renamed(File,OldPath):-    
    pef_renamed_file_assert([file=File,old_path=OldPath]).    