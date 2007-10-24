:- module(workspace,[]).
:-use_module(library('builder/builder')).
:-use_module(library('pef/pef_base')).
:-use_module(library('util/pdt_regex')).

pdt_builder:build_hook(directory(Abs,Include,Exclude)):-
    (	exists_directory(Abs)
    ->	pef_reserve_id(pef_directory,Dir),
    	pef_directory_assert([id=Dir,path=Abs, include_pattern=Include, exclude_pattern=Exclude]),
    	find_entries(Abs,Include,Exclude,Deps),
    	process_entries(Deps,Dir,0,Count),
    	pef_property_assert([pef=Dir,key=file_count,value=Count])
    ;	throw(no_such_dir(Abs))
    ).
pdt_builder:build_hook(file(Abs)):-
	(	exists_file(Abs)
    ->	pef_reserve_id(pef_file,FID),
    	pef_file_assert([id=FID,path=Abs])    	
    ;	throw(no_such_file(Abs))
    ).
pdt_builder:build_hook(project(Name)):-
    (	pef_project_query([name=Name, id=Project])
    ->	forall(
    		pef_source_path_query([project=Project, 
    								include_pattern=IP,
    								exclude_pattern=EP,
    								path=Path]
    							),
    	    pdt_request_target(directory(Path,IP,EP))
    	)
    ;	throw(no_such_project(Name))
    ).
pdt_builder:build_hook(workspace):-
    forall(pef_project_query([name=Name]),pdt_request_target(project(Name))).
    
    


process_entries([],_,C,C).
process_entries([file(Abs)|Entries],Dir,C,C2):-
    !,
    C1 is C + 1,  
    pdt_request_target(file(Abs)),  
    pef_file_query([path=Abs,id=FID]),
    pef_directory_entry_assert([parent=Dir,child=FID]),
    process_entries(Entries,Dir,C1,C2).
process_entries([directory(Abs,IP,EP)|Entries],Dir,C,C2):-
    pdt_request_target(directory(Abs,IP,EP)),
    pef_directory_query([path=Abs,include_pattern=IP,exclude_pattern=EP,id=SubDir]),
    pef_property_query([pef=SubDir,key=file_count,value=SubC]),
    C1 is C + SubC,
    pef_directory_entry_assert([parent=Dir,child=SubDir]),    
    process_entries(Entries,Dir,C1,C2).
    	
find_entries(Abs,IP,EP,Deps):-
	atom_concat(Abs,'/*',LsPattern),
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
filter([_|Files],IP,EP,FilteredFiles):-
	filter(Files,IP,EP,FilteredFiles).
	    	
    
