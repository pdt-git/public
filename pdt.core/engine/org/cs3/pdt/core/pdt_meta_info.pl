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
	pdt_find_clauses/4,
	pdt_find_first_clause/4,
	pdt_find_first_clause_position/4,
	pdt_file_includes/2,
	pdt_file_depends/2,
	pdt_predicate_completion/4,
	pdt_predicate_completion/5,
	pdt_file_module/2
]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_dfs')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:-use_module(library('/org/cs3/pdt/model/pdt_index')).
:-use_module(library('/org/cs3/pdt/model/pdt_handle')).

%TODO: most of the file_<something> predicates should be replaced by an index/factory combi

%pdt_file_contains_clause(File,DefModule,Name,Arity,aterm(Anns,Term)):-
%    pdt_file_spec(File,Abs),
%	current_file_annotation(Abs,_,Terms),
%	member(aterm(Anns,Term),Terms),
%	pdt_member(clause_of(DefModule:Name/Arity),Anns).
  

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