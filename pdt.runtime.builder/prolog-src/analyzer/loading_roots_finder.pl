:- module(loading_roots_finder,[find_loading_roots/0]).

:- ensure_loaded('../parse_util').

find_loading_roots:-
    fileT(FileId,FileName,_),
    	not(load_edge(_,FileId,_,_)),
    	format('Loading root: ~w~n',[FileName]).
    
find_standalone_files:-
    fileT(FileId,FileName,_),
    	not(load_edge(FileId,_,_,_)),
    	not(load_edge(_,FileId,_,_)),
    	format('Loading root: ~w~n',[FileName]).
    
find_loading_leafs:-
    fileT(FileId,FileName,_),
    	not(load_edge(FileId,_,_,_)),
    	format('Loading root: ~w~n',[FileName]).
    
:- dynamic load_level/2. %load_level(FileId,Level).
:- dynamic load_level_ri/2. %load_level_ri(Level,FileId).    
    	
compute_loading_levels:-
    remove_old_loading_levels,
    assign_level0_to_leaves.
   
remove_old_loading_levels:-
    retractall(load_level(_,_)),
    retractall(load_level_ri(_,_)).
    
    
:- dynamic next_level/1. 

assign_level0_to_leaves:-
    fileT(FileId,_,_),
    	not(load_edge(FileId,_,_,_)),
    	assert(load_level(FileId,0)),
    	assert(load_level_ri(0,FileId)),
    	fail.
assign_level0_to_leaves:-
	retractall(next_level(_)),
	assert(next_level(1)).	   

compute_further_levels:-
    repeat,
    	next_level(CurrentLevel),
    	compute_next_loading_level,
    not(load_level(_,CurrentLevel)).

compute_next_loading_level:-
    next_level(CurrentLevel),
    format('currentLevel: ~w~n',[CurrentLevel]),
    LastLevel is CurrentLevel - 1,
    writeln('a'),
    load_level_ri(LastLevel,LoadedFileId),
    	load_edge(FileId,LoadedFileId,_,_),
    		(	load_level(FileId,OldLevel)
    		->	(	writeln('b'),
    				(	OldLevel < CurrentLevel
    				->	format('resetting load level of ~w from ~w to ~w - loads ~w~n',[FileId,OldLevel,CurrentLevel,LoadedFileId])
    				;	true
    				),
    				retract(load_level(FileId,_))
    			)
    		;	true
    		),
    		writeln('c'),
    		assert(load_level(FileId,CurrentLevel)),
    		assert(load_level_ri(CurrentLevel,FileId)),
    		writeln('d'),
    fail.
compute_next_loading_level:-
    writeln('e'),
    next_level(CurrentLevel),
    retractall(next_level(_)),
    NextLevel is CurrentLevel + 1,
    assert(next_level(NextLevel)).