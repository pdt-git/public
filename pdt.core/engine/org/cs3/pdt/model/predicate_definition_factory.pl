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

:-module(predicate_definition_factory,[]).

%
% this factory uses the index predicate_definition. This index is not maintained here, but
% by the annotator module clause_annotator, which is defined in ../annotate/indexer.pl
% things will probably be moved around a bit in the future, i have not yet decided on the
% final modul boundaries.
%

:- use_module(library('org/cs3/pdt/model/pdt_index')).
:- use_module(library('org/cs3/pdt/model/pdt_handle')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_util_hashtable')).

%This module registers itself as a property factory for handles of type 'predicate_definition'. See pdt_handle.
:- pdt_add_property_factory(predicate_definition,predicate_definition_factory).


lookup_handle(handle(id(File,Module:Name/Arity), predicate_definition,Cache)):-
    pdt_index_load(builtin_predicates,Ix),
    pdt_index_get(Ix,Name,handle(id(File,Module:Name/Arity), predicate_definition,Cache)).


%
% atm, all properties are cached except the indexed property clauses
%
get_property(handle(id(File,Module:Name/Arity), predicate_definition,_),clauses,Value):-
    current_file_annotation(File,_,Terms),
	filter_clauses(Terms,Module:Name/Arity,Clauses),
	Value=..[array|Clauses].


get_property(handle(id(File,Module:Name/Arity), predicate_definition,_),comments,Value):-
    current_file_annotation(File,_,Terms),
    current_file_comments(File,CommentsMap),
	collect_comments(CommentsMap,Terms,Module:Name/Arity,CommentsTexts),
	flatten(CommentsTexts,Value).
	
    
collect_comments(_,[],_,[]).
collect_comments(CommentsMap,[Term|Terms],Sig,[CommentTexts|Comments]):-
    Term=aterm(Anns,_),
    pdt_memberchk(clause_of(Sig),Anns),
    pdt_memberchk(comments_left(CommentPositions),Anns),
    !,
	comment_texts(CommentsMap,CommentPositions,CommentTexts),
    collect_comments(CommentsMap,Terms,Sig,Comments).
collect_comments(CommentsMap,[_|Terms],Sig,Comments):-
    collect_comments(CommentsMap,Terms,Sig,Comments).
    

		
filter_clauses([],_,[]).
filter_clauses([Term|Terms],Sig,[Term|Clauses]):-
    Term=aterm(Anns,_),
    pdt_memberchk(clause_of(Sig),Anns),
    !,
    filter_clauses(Terms,Sig,Clauses).
filter_clauses([_|Terms],Sig,Clauses):-
    filter_clauses(Terms,Sig,Clauses).


comment_texts(_CommentsMap,[],[]).
comment_texts(CommentsMap,[Position|Positions],[Comment|Comments]):-
    pdt_map_get(CommentsMap,Position,Comment),
    comment_texts(CommentsMap,Positions,Comments).
    