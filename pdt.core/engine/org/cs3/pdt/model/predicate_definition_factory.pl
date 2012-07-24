/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:-module(predicate_definition_factory,[]).

%
% this factory uses the index predicate_definition. This index is not maintained here, but
% by the annotator module clause_annotator, which is defined in ../annotate/indexer.pl
% things will probably be moved around a bit in the future, i have not yet decided on the
% final modul boundaries.
%

:- use_module(library('org/cs3/pdt/model/pdt_index')).
:- use_module(library('org/cs3/pdt/model/pdt_handle')).
:- use_module(library('org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
%:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('org/cs3/pdt/util/pdt_source_term')).


%This module registers itself as a property factory for handles of type 'predicate_definition'. See pdt_handle.
:- pdt_add_property_factory(predicate_definition,predicate_definition_factory).


lookup_handle(handle(id(File,Module:Name/Arity), predicate_definition,Cache)):-
    pdt_index_load(builtin_predicates,Ix),
    pdt_index_get(Ix,Name,handle(id(File,Module:Name/Arity), predicate_definition,Cache)).


%
% atm, all properties are cached except the indexed property clauses
%
get_property(handle(id(File,Module:Name/Arity), predicate_definition,_),clauses,Value):-
    pdt_file_record_key(term,File,Key),
    findall(Clause,
    	(	pdt_file_record(Key,Clause),
    		%pdt_aterm_term_annotation(Clause,_,Annos),
    		%pdt_member(clause_of(Module:Name/Arity),Annos)
    		source_term_property(Clause,clause_of,Module:Name/Arity)
    	),
    	Clauses
    ),
	Value=..[array|Clauses].


get_property(handle(id(File,Module:Name/Arity), predicate_definition,_),comments,Value):-
    pdt_file_comments(File,CommentsMap),
    pdt_file_record_key(term,File,Key),
	collect_comments(CommentsMap,Key,Module:Name/Arity,CommentsTexts),
	flatten(CommentsTexts,Value).
	
    
collect_comments(CommentsMap,Key,Sig,Comments):-
    findall(CommentTexts,
    	(	pdt_file_record(Key,Term),
    		%pdt_aterm_term_annotation(Term,_,Anns),
		    %pdt_memberchk(clause_of(Sig),Anns),
    		%pdt_memberchk(comments_left(CommentPositions),Anns),
    		source_term_property(Term,clause_of,Sig),
    		source_term_property(Term,comments_left,CommentPositions),
			comment_texts(CommentsMap,CommentPositions,CommentTexts)
		), Comments
	).
    	
collect_comments(CommentsMap,[_|Terms],Sig,Comments):-
    collect_comments(CommentsMap,Terms,Sig,Comments).
    

		


comment_texts(_CommentsMap,[],[]).
comment_texts(CommentsMap,[Position|Positions],[Comment|Comments]):-
    pdt_map_get(CommentsMap,Position,Comment),
    comment_texts(CommentsMap,Positions,Comments).
    


