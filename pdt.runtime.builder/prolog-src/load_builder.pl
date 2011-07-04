:- use_module('prolog_file_reader').
:- use_module('analyzer/metapred_finder').



pl_test:-
    find_all_loaded_files(Project),
    plparser_quick:generate_facts(Project).       
    
find_all_loaded_files(Project):-
    findall(
    	File,
    	source_file(File),
    	Project
    ).	
 
pl_test(Project):-
	plparser_quick:generate_facts(Project).
	
	
pl_test_fix:-	
    pl_test(['C:/Data/Git-Data/pdt.git/pdt.runtime.builder/prolog-src']). 