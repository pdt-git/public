%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
some predicate definitions for queries frequently used by the pdt.core 
*/

:-module(pdt_meta_info,[
	pdt_file_problem/3,
	pdt_find_clauses/4,
	pdt_find_first_clause/4,
	pdt_find_first_clause_position/4,
	pdt_file_includes/2,
	pdt_file_depends/2,
	pdt_predicate_completion/4,
	pdt_predicate_completion/5,
	pdt_file_module/2,
	pdt_lookup_aterm/3,
	pdt_lookup_aterm/4,
	pdt_render_term/4,
	pdt_file_directive/2,
	pdt_predicate_clause/3,
	pdt_builtin_help/3,
	pdt_builtin_help/4
]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_dfs')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:-use_module(library('/org/cs3/pdt/model/pdt_index')).
:-use_module(library('/org/cs3/pdt/model/pdt_handle')).

:- use_module(library(help)).
:- use_module(library(helpidx)).

%TODO: most of the file_<something> predicates should be replaced by an index/factory combi

%pdt_file_contains_clause(File,DefModule,Name,Arity,aterm(Anns,Term)):-
%    pdt_file_spec(File,Abs),
%	current_file_annotation(Abs,_,Terms),
%	member(aterm(Anns,Term),Terms),
%	pdt_member(clause_of(DefModule:Name/Arity),Anns).
  
pdt_lookup_aterm(FileSpec,N,ATerm,Member):-
	pdt_file_spec(FileSpec,Abs),
    current_file_annotation(Abs,_,Terms),
    lookup_aterm(Terms,N,ATerm,Member).

pdt_lookup_aterm(FileSpec,N,ATerm):-
	pdt_lookup_aterm(FileSpec,N,ATerm,_).

  
lookup_aterm([Member|_],N,ATerm,Member):-
    lookup_aterm_X(Member,N,ATerm),
    !.
lookup_aterm([Member|Members],N,ATerm,Container):-
    pdt_term_annotation(Member,_,Annos),
    pdt_member(last_n(LastN),Annos),
    LastN<N,
    lookup_aterm(Members,N,ATerm,Container).
  
lookup_aterm_X(ATerm,N,ATerm):-
	pdt_term_annotation(ATerm,_,Annos),
	pdt_member(n(N),Annos),
	!.
lookup_aterm_X(Container,N,ATerm):-
	pdt_term_annotation(Container,_,Annos),
	pdt_member(n(Left),Annos),
	pdt_member(last_n(Right),Annos),
	Left < N,
	N =< Right,
	pdt_subterm(Container,[_|_],SubContainer),
	lookup_aterm_X(SubContainer,N,ATerm).
    
pdt_file_directive(FileSpec,[label(Label)|Annos]):-
	current_file_annotation(FileSpec,_,Terms),
	member(Term,Terms),
	pdt_term_annotation(Term,:-(_),Annos),
	get_op_module(FileSpec,OpModule),
	pdt_subterm(Term,[1],Body),
	render_term(Body,Term,OpModule,4,Label).

pdt_predicate_clause(FileSpec,Module:Name/Arity,[label(Label),type(Type)|Annos]):-
	current_file_annotation(FileSpec,_,Terms),
	member(Term,Terms),	
	pdt_term_annotation(Term,TTerm,Annos),
	pdt_member(clause_of(Module:Name/Arity),Annos),
	(	functor(TTerm,:-,2)
	->	pdt_subterm(Term,[1],Head),
		Type=rule
	;	Head=Term,
		Type=fact
	),
	
	get_op_module(FileSpec,OpModule),
	render_term(Head,Term,OpModule,4,Label).
	
  
pdt_file_problem(FileSpec,Problem,Pos):-
    pdt_file_spec(FileSpec,Abs),
    current_file_annotation(Abs,_,Terms),
    pdt_subterm(Terms,_,ATerm),
    pdt_term_annotation(ATerm,_,Annos),
    pdt_member(problem(Problem),Annos),
    pdt_member(position(Pos),Annos).

pdt_file_includes(FileSpec,IncludeSpec):-
    nonvar(FileSpec),
    !,
    get_includes(FileSpec,IncludeSpec).
pdt_file_includes(FileSpec,IncludeSpec):-
    nonvar(IncludeSpec),
    get_including(IncludeSpec,FileSpec).

get_includes(FileSpec,IncludeSpec):-  
    pdt_file_spec(FileSpec,Abs),
    current_file_annotation(Abs,Terms,_),
    member(references_files(Refs),Terms),
    member(IncludeSpec,Refs).
    
get_including(IncludeSpec,File):-
	pdt_file_spec(IncludeSpec,Abs),
    current_file_annotation(File,Terms,_),
    member(references_files(Refs),Terms),
    member(Abs,Refs).

pdt_file_depends(DependentFile,DependencyFile):-
    nonvar(DependentFile),
    !,
    get_file_dependency(DependentFile,DependencyFile).
pdt_file_depends(DependentFile,DependencyFile):-
    nonvar(DependencyFile),
    get_dependent_file(DependencyFile,DependentFile).

get_file_dependency(DependentFile,DependencyFile):-
    pdt_dfs(DependentFile,get_includes(From,To),From,To,DependencyFile).
get_dependent_file(DependencyFile,DependentFile):-
    pdt_dfs(DependencyFile,get_including(From,To),From,To,DependentFile).
	
pdt_file_module(FileSpec,Module):-
    nonvar(FileSpec),
    !,
    get_module(FileSpec,Module).

pdt_file_module(FileSpec,Module):-
	nonvar(Module),
	!,
	get_file(Module,FileSpec).    

get_module(FileSpec,Module):-    
    pdt_file_spec(FileSpec,Abs),
    current_file_annotation(Abs,Ann,_),
    memberchk(defines_module(Module),Ann),
    !.
get_module(_,user).    

get_file(Module,File):-
    pdt_index_load(module_definitions,Ix),
    pdt_index_get(Ix,Module,H),
    pdt_property(H,file,File).

pdt_predicate_completion(ContextModule,Prefix,P,Properties):-
	pdt_predicate_completion(predicate_definitions,ContextModule,Prefix,P,Properties).
pdt_predicate_completion(ContextModule,Prefix,P,Properties):-	
	pdt_predicate_completion(builtin_predicates,ContextModule,Prefix,P,Properties).

pdt_predicate_completion(IxName,ContextModule,Prefix,P,Properties):-
   pdt_index_load(IxName,IX),
   pdt_index_after(IX,Prefix,P,H),
   (		atom_concat(Prefix,_,P)
   ->	true
   ;		!,fail
   ),
   visible_in_context(ContextModule,H),
   pdt_properties(H,Properties).    
   
pdt_find_clauses(Context,Name/Arity,DefFile,Clauses):-
    pdt_index_load(predicate_definitions,IX),
    pdt_index_get(IX,Name,H),
    pdt_property(H,arity,Arity),
    visible_in_context(Context,H),
    pdt_property(H,file,DefFile),
    pdt_file_module(ContextFile,Context),
    pdt_file_depends(ContextFile,DefFile),
    pdt_property(H,clauses,Clauses).

pdt_find_first_clause(Context,Name/Arity,DefFile,Clause):-
	pdt_find_clauses(Context,Name/Arity,DefFile,Clauses),
	arg(1,Clauses,Clause).

pdt_find_first_clause_position(Context,Name/Arity,DefFile,Position):-
	pdt_find_first_clause(Context,Name/Arity,DefFile,Clause),
	pdt_top_annotation(Clause,Anno),
	memberchk(position(Position),Anno).


visible_in_context(ContextModule,H):-
    pdt_property(H,module,ContextModule),
    !.
visible_in_context(_,H):-	
	pdt_property(H,module,user),
	!.
visible_in_context(_,H):-	
	pdt_property(H,module,system),
	!.
visible_in_context(_,H):-	
	pdt_property(H,exported,true),
	!.	
	
pdt_render_term(FileSpec,N,Depth,Out):-
	pdt_lookup_aterm(FileSpec,N,ATerm,Clause),
	get_op_module(FileSpec,OpModule),
	render_term(ATerm,Clause,OpModule,Depth,Out).
	
render_term(ATerm,Clause,OpModule,Depth,Out):-
	new_memory_file(MemFile),
	open_memory_file(MemFile,write,Stream),	
	pdt_term_annotation(Clause,_,Annos),
	pdt_member(variable_names(VarNames),Annos),
	execute_elms(VarNames),
	term_variables(ATerm,Vars),
	unify_with_underscores(Vars),
	pdt_strip_annotation(ATerm,Term,_),
	write_term(Stream,Term,[max_depth(Depth),module(OpModule),portray(true)]),
	close(Stream),
	memory_file_to_atom(MemFile,Out),
	free_memory_file(MemFile).
	
execute_elms([]).
execute_elms([Goal|Goals]):-
	Goal,
	execute_elms(Goals).

unify_with_underscores([]).
unify_with_underscores([Var|Vars]):-
	Var='_',
	unify_with_underscores(Vars).
	
	
pdt_builtin_help(Name,Arity,Summary):-
		predicate(Name,Arity,Summary,_,_),
		!.
pdt_builtin_help(_Name,_Arity,'').
		
pdt_builtin_help(Name,Arity,Summary,Help):-
	find_manual(ManPath),
	open(ManPath,read,ManStream),
	predicate(Name,Arity,Summary,From,To),
	!,
	new_memory_file(MemFile),
	open_memory_file(MemFile,write,MemStream),
	show_ranges([From-To],ManStream,MemStream),
	close(MemStream),
	memory_file_to_atom(MemFile,Help),
	free_memory_file(MemFile).
pdt_builtin_help(_Name,_Arity,'', '').
	
%
% the following predicates where copied from the library help.pl
% which is distributed with swi-prolog. 
%

find_manual(Path) :-
	absolute_file_name(library('MANUAL'), [access(read)], Path).

show_ranges([], _, _) :- !.
show_ranges([From-To|Rest], Manual, Pager) :-
	seek(Manual, From, bof, _),
	Range is To - From,
	copy_chars(Range, Manual, Pager),
	nl(Pager),
	show_ranges(Rest, Manual, Pager).

copy_chars(N, From, To) :-
	get0(From, C0),
	copy_chars(N, From, To, C0).

copy_chars(N, _, _, _) :-
	N =< 0, !.
copy_chars(N, _, To, _) :-
	0 =:= N mod 4096,
	flush_output(To),
	fail.
%copy_chars(N, From, To, C) :-
%	get0(From, C1),
%	(   C1 == 8,			% backspace
%	    \+ current_prolog_flag(write_help_with_overstrike, true)
%	->  get0(From, C2),
%	    NN is N - 2,
%	    copy_chars(NN, From, To, C2)
%	;   put_printable(To, C),
%	    NN is N - 1,
%	    copy_chars(NN, From, To, C1)
%	).

copy_chars(N, From, To, C) :-
	get0(From, C1),
	(   C1 == 8			% backspace
	    %\+ current_prolog_flag(write_help_with_overstrike, true)
	->  get0(From, C2),
	    NN is N - 2,
	    copy_chars(NN, From, To, C2)
	;   put_printable(To, C),
	    NN is N - 1,
	    copy_chars(NN, From, To, C1)
	).

put_printable(_, 12) :- !.
put_printable(_, -1) :- !.
put_printable(To, C) :-
	put(To, C).
	