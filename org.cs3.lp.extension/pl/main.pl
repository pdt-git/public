extension_point_location_fact.

add_extension_point_file_search_paths :-
    clause(extension_point_location_fact,_,Ref),
    clause_property(Ref,file(File)),
	file_directory_name(File,Dir),
	assert(file_search_path(org_cs3_lp_extension,Dir)).
	
:- add_extension_point_file_search_paths.
		
:- ['extension_point'].
