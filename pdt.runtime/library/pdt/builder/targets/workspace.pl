:- module(workspace,[]).
:-use_module(library('builder/builder')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).

pdt_builder:build_hook(directory(Abs,Include,Exclude)):-
    (	exists_directory(Abs)
    ->	pef_reserve_id(pef_directory,Dir),
    	pef_directory_assert([id=Dir,path=Abs, include_pattern=Include, exclude_pattern=Exclude]),
    	find_entries(Abs,Include,Exclude,Deps),
    	process_entries(Deps,Dir,0,Count),
    	pef_property_assert([pef=Dir,key=file_count,value=Count])
    ;	throw(no_such_dir(Abs))
    ).



process_entries([],C,C).
process_entries([file(Abs)|Entries],C,C2):-
    C1 is C + 1,
    pdt_request_target(file(Abs)),
    process_entries(Entries,C1,C2).
    	
find_entries(Abs,IP,EP,Deps):-
	atom_concat(Dir,'/*',LsPattern),
	expand_file_name(LsPattern,Files),
	filter(Files,IP,EP,Deps).
	
filter([],_IP,_EP,[]).
filter([File|Files],IP,EP,[directory(File,IP,EP)|FilteredFiles]):-
    exists_directory(File),
    !,
    filter(Files,IP,EP,FilteredFiles).
filter([File|Files],IP,EP,[file(File)|FilteredFiles]):-
    atom_codes(File,Codes),
    pdt_regex_match(IP,Codes,[],_),
    \+ pdt_regex_match(EP,Codes,[],_),
    !,
    filter(Files,IP,EP,FilteredFiles).
filter([_|Files],IP,EP,[FilteredFiles]):-
	filter(Files,IP,EP,FilteredFiles).
	    	
    
