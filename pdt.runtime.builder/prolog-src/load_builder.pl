:- use_module('prolog_file_reader').
:- use_module('analyzer/metapred_finder').



%undeclared_metapredicate(Head):-
	 


generate_factbase:-
    find_all_loaded_files(Project),
    plparser_quick:generate_facts(Project).       
    
find_all_loaded_files(Project):-
    current_prolog_flag(home, PrologHome),
    findall(
    	File,					%the following removes the files from prolog itself - maybe this should be changed back
    	(	source_file(File),
    		\+(string_concat(PrologHome, _, File))
    	), 
    	Project
    ).	
 
pl_test(Project):-
	plparser_quick:generate_facts(Project).
	
	
pl_test_fix:-	
    pl_test(['C:/Data/Git-Data/pdt.git/pdt.runtime.builder/prolog-src']). 