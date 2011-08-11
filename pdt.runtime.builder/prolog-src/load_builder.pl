:- ensure_loaded('parse_util').
:- use_module('analyzer/metapred_finder').


generate_factbase:-
    with_mutex(prolog_factbase,
    	(	find_all_loaded_files(Project),
    		parse_util:generate_facts(Project)
    	)
    ).       


generate_factbase(File):-
    with_mutex(prolog_factbase,
    	(	find_all_loaded_files(Project),
    		filter_already_known_files(Project,MissingFiles),
    		flatten(MissingFiles,FlatMissingFiles),
    		parse_util:update_facts(File,FlatMissingFiles)
    	)
    ).
    	       

    
find_all_loaded_files(Project):-
    current_prolog_flag(home, PrologHome),
    findall(
    	File,					%the following removes the files from prolog itself - maybe this should be changed back
    	(	source_file(File),
    		\+(string_concat(PrologHome, _, File))
    	), 
    	Project
    ).	
 
filter_already_known_files([],[]).
filter_already_known_files([File|Tail],[MissingTail]):-
    fileT_ri(File,_),
  	%format('!!!!Already there: ~w~n',[File]),
    !,
    filter_already_known_files(Tail,MissingTail).
filter_already_known_files([File|Tail],[File|MissingTail]):-
    filter_already_known_files(Tail,MissingTail).    
    
	 
 
pl_test(Project):-
	parse_util:generate_facts(Project).
	
	
pl_test_fix:-	
    pl_test(['C:/Data/Git-Data/pdt.git/pdt.runtime.builder/prolog-src']). 