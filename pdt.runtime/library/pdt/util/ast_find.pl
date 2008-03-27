% helper predicates for adressing program elements through a source position.
:- module(ast_find,
	[	path_at/4,
		topleval_at/4,
		between_toplevels/6
	]
).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).

%%
% toplevel_at(File,From-To,Toplevel) nondet
%
% Find toplevels at a given source location.
%
% Succeeds if Toplevel is a toplevel in File associated to a source code interval that overlapps
% [From,To[.
toplevel_at(File,From,To,Toplevel):-
    pef_toplevel_query([file=File,id=Toplevel]),
    toplevel_source_position(Toplevel,File,Start,End),
   	overlapping(Start,End,From,To).

%%
% ast_path_at(File,From,To,Toplevel,Path) nondet
%
% Finds a path to a maximal included AST element for a given source interval.
%
% A node is called "maximal included" for a given interval if it represents a 
% source interval which is included in the given interval, and if none of its children are
% included.
%
% There may be more than one maximal included node, so this predicate is non deterministic.
%
% @param Path will be unified with a list. The first element is the toplevel. All remaining 
% elements are a list of AST node IDs starting from the root down to the maximal element. 
%
% Succeeds if Toplevel is a toplevel in File associated to a source code interval that overlapps
% [From,To[.    
path_at(File,From,To,[Toplevel|Path]):-    
	toplevel_at(File,From,To,Toplevel),
	path_at2(Toplevel,From,To,Path).
	
path_at2(Toplevel,From,To,Path):-
	(	pef_ast_query([toplevel=Toplevel,root=Root])
	->	path_at3(Root,From,To,Path)
	;	Path=[]
	).
	
path_at3(Node,From,To,Path):-
	node_position(Node,Start,End),
	(	included(Start,End,From,To)
	->	Path=[Node]
	;	overlapping(Start,End,From,To)
	->	pef_arg_query([parent=Node,child=Arg]),
		Path=[Node|Tail],
		path_at3(Arg,From,To,Tail)
	;	fail
	).

node_position(Node,Start,End):-
    pef_property_query([pef=Node,key=start,value=Start]),
    pef_property_query([pef=Node,key=end,value=End]).

overlapping(Start,End,From,To):-
	Start < To, 
    End > From.

included(Start,End,From,To):-
	Start >= From,
	End =< To.    
	
	

between_toplevels(File,Offset,Left,LeftEnd,Right,RightStart):-	
	setof(c(Start,End,Toplevel),
		File^End^(	
			pef_toplevel_query([file=File,id=Toplevel]),
			toplevel_source_position(Toplevel,File,Start,End)
		),
		Toplevels
	),
	between2(Toplevels,Offset,[],[],Left,LeftEnd,Right,RightStart).

between2([],Offset,Last,LastEnd,Last,LastEnd,[],[]).
between2([c(Start,End,Toplevel)|Tupels],Offset,Last,LastEnd,Left,LeftEnd,Right,RightStart):-
	(	Start > Offset
	->	RightStart = Start,
		Right = Toplevel,
		Left= LastLeft,
		End=LastEnd		
	;	between2(Tupels,Offset,Toplevel,End,Left,LeftEnd,Right,RightStart)
	).
	