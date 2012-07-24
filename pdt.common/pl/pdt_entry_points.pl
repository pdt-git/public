/* $LICENSE_MSG$ */


:- module(pdt_entry_points,[
	pdt_entry_point/1,
	add_entry_point/1,
	remove_entry_points/1
]).

:- dynamic unfiltered_entry_point/1.

pdt_entry_point(File) :-
    unfiltered_entry_point(File),
    source_file(File).

add_entry_point(File) :-
    unfiltered_entry_point(File),
    !.
    
add_entry_point(File) :-
    assert(unfiltered_entry_point(File)).
    
remove_entry_points(File) :-
    retractall(unfiltered_entry_point(File)).


