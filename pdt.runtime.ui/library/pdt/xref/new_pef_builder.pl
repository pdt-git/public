%:- module(new_builder,[consult_entry_point_and_parse/2]).

:- use_module(prolog_file_reader_quick).
%:- consult(pl_ast_to_abba).

%:- assert(library_directory('z:/workspacepdt/pdt.runtime/library/pdt/xref')).
%:- assert(library_directory('z:/workspacepdt/pdt.runtime/library/pdt/xref/util')).

/**
 * consult_entry_point_and_parse(+File, +Project)
 *    - loades the file represented by Arg1 and everything that has to 
 *   	be loaded together with it.
 *    - after that parses every file that was loaded in the first step inside 
 *      of the directory represented by Arg2 and builds the PEF-AST together
 *      with the edge informations 
 *      (see generate_facts/1 from prolog_file_reader_quick.pl).
 **/
consult_entry_point_and_parse(File, Project):-
    load_files(File,silent(true)),         
    %consult(File),
    findall(ToParse, 
    		(	source_file(ToParse),
% 				downcase_atom(Project,Lc_Project),
				Project = Lc_Project,
      			atom_concat(Lc_Project,_,ToParse)
      		),
      		ParseList),
 %   	not(library_directory(Directory)),
    writeln(ParseList),
    generate_facts(ParseList).
%consult_entry_point_and_parse(_,_).


