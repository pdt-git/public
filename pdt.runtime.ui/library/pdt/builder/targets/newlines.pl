:- module(newlines,[]).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/workspace')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).

pdt_builder:build_hook(newlines(Resource)):-
    pdt_request_target(Resource),
    (	Resource = file(Path)
    ->	find_newlines(Path)
    ;	forall(pdt_contains(Resource,Element),pdt_request_target(newlines(Element)))
    ).

find_newlines(Path):-
	get_pef_file(Path,File),
	!,
	open(Path,read,In),
	call_cleanup(find_newlines(In,File),close(In)).
find_newlines(_).
	    
find_newlines(In,File):-
    pdt_map_empty(Assoc0),
	find_newlines(In,Assoc0,Assoc),
	pef_property_assert([pef=File,key=newlines,value=Assoc]).
	   
find_newlines(In,Assoc0,Assoc):-
	(	at_end_of_stream(In)
	->	Assoc=Assoc0
	;	check_code(In,Assoc0,Assoc1),
		find_newlines(In,Assoc1,Assoc)
	).
	
check_code(In,Assoc0,Assoc):-
    character_count(In,CharCount),
    stream_property(In,position(Pos)), 
    get_code(In,Code),      
    (	code_type(Code,end_of_line)	
	->	% rb tree traversal is currently only implemnted from left
		% to right. We need it the other way arround. 
		% Work arround: negate keys.
		Key is -1 * CharCount,
		pdt_map_put(Assoc0,Key,Pos,Assoc)
	;	Assoc=Assoc0
	).