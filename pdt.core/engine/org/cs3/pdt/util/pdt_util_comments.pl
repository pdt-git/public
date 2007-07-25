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


/* maybe this should be moved to the core module? Not sure.
*/

:- module(pdt_util_comments,[
	pdt_attach_comments/6,
	pdt_comment_dom/4,
	pdt_comment_summary/5,
	pdt_dom_html/3
	]).
	
:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_source_term')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_term_position')).


%FIXME: Jan said this probably all subject to changes, but that the predicates
% with relevance to the PDT will eventually become public api.
%:- ensure_loaded(library('test_wiki')). 
:- use_module(library('pldoc/doc_html')).
%:- use_module(my_pldoc_html). %temporary workaround. I need an extra pred not in the public api
:- use_module(library('pldoc/doc_process')).
:- use_module(library('pldoc/doc_wiki')).
:- use_module(library('pldoc/doc_modes')).
:- use_module(library('http/html_write')).

:- doc_collect(false).
	
%attach_comments(+ATermIn,+InputStream,+CPositions, -RemainingPositions, -ATermOut)
%
%Attaches comment positions to the correct subterms of a term read with read_term.
%
%ATermIn should be an annotated term, as produced by pdt_aterm_wrap_term/6. It should at least 
%contain position annotations.
%
% CommentsMap is a pdt_map with character offsets as key and comment strings as value.
%
%InputStream should be a stream providing the data from which the term was originally read. 
%NOTE: do not use the same stream for interleaved reading of terms! This predicate may
%seek in this stream and not revert it to its original position.
%
%CPositions should be a list containing the character offsets of all comments that were read together with
%the term. The list should be in ascending order.
%
%ATermOut will be unified with an annotated term that results from ATermIn with additional comments_left and
%comments_right annotations. The values of these annotations are lists of character offsets of comments.

pdt_attach_comments(ATerm,_,_,[],[],ATerm):-
    !.
pdt_attach_comments(ATermIn,CommentsMap,Stream,CPositions,RemainingPositions,ATermOut):-
	% read character start and end offsets of the term from the term annotations.	
	/*
	pdt_aterm_term_annotation(ATermIn,_,AnnosIn),
	pdt_member(position(Start-End),AnnosIn),
	*/
	source_term_property(ATermIn,position,Start-End),
	
	% rough separation of comments in three intervals left, mid and right.
	% Left positions are left of the term, mid within abd right positions are at the right of the term.
	pdt_chop_before(Start,CPositions,LeftPositions,TmpPositions),
	pdt_chop_before(End,TmpPositions,MidPositions,RightPositions),
	
	% left is easy: all comments that were not claimed by any preceeding term are attached
	% to this term.
	do_attach_left(ATermIn,LeftPositions,ATerm1),
	
	% the middle must belong to the arguments of the term. Recurse on arguments.
	/*	
	pdt_aterm_term_annotation(ATerm1,Subterm1,Annos1),
	(	nonvar(Subterm1)
	->	Subterm1=..[Functor|Args1],
		attach_arg_comments(Args1,CommentsMap,Stream,MidPositions,Args2),
		Subterm2=..[Functor|Args2],
		pdt_aterm_term_annotation(ATerm2,Subterm2,Annos1)
	;	true
	),
	*/
	(	source_term_var(ATerm1)
	->	ATerm2=ATerm1
	;	do_attach_middle(ATerm1,MidPositions,CommentsMap,Stream,ATerm2)
	),
	
	% from the comments reamining to the right, we claim all comments that "directly" follow
	% the current term, i.e., there is no non-whitespace token between the end of the term and the beginning
	% of the comment.
	% First we scan forward and find the position of the next non-whitespace char in the input stream.
	next_token_position(Stream,End,CommentsMap,TPos),
	
	% chop the list of positions at the found position. The positions left of TPos are claimed by this term
	% for the other positoins are unified with RemainingPositions.
	pdt_chop_before(TPos,RightPositions,ClaimedPositions,RemainingPositions),
	do_attach_right(ATerm2,ClaimedPositions,ATermOut).


do_attach_middle(Term,[],_CommentsMap,_Stream,Term):-
    !.
do_attach_middle(TermIn,Positions,CommentsMap,Stream,TermOut):-
	source_term(TermIn,Module),
    source_term_create(Module,_,Term1),
	source_term_functor(TermIn,Name,Arity),
	source_term_functor(Term1,Name,Arity),
    source_term_copy_properties(TermIn,Term1,TermOut),
    do_attach_args(TermIn,Positions,CommentsMap,Stream,1,Arity,TermOut).
    
do_attach_args(_TermIn,_Positions,_CommentsMap,_Stream,N,M,_TermOut):-
    N>M,
    !.
do_attach_args(TermIn,Positions,CommentsMap,Stream,N,M,TermOut):-
	source_term_arg(N,TermIn,ArgIn),
    pdt_attach_comments(ArgIn,CommentsMap,Stream,Positions,RemainingPositions,ArgOut),
    source_term_arg(N,TermOut,ArgOut),
    O is N + 1,
    do_attach_args(TermIn,RemainingPositions,CommentsMap,Stream,O,M,TermOut).
    
do_attach_left(ATermIn,Positions,ATermOut):-
	source_term_set_property(ATermIn,comments_left,Positions,ATermOut).
/*    
	pdt_aterm_term_annotation(ATermIn,Term,Annos),
    pdt_aterm_term_annotation(ATermOut,Term,[comments_left(Positions)|Annos]).
*/

do_attach_right(ATermIn,Positions,ATermOut):-
	source_term_set_property(ATermIn,comments_right,Positions,ATermOut).
/*
    pdt_aterm_term_annotation(ATermIn,Term,Annos),
    pdt_aterm_term_annotation(ATermOut,Term,[comments_right(Positions)|Annos]).
*/

next_token_position(Stream,End,CommentsMap,TPos):-
    /*FIXME: since end positions are exclusive, it can happen that
      end>eof. There is probably a better way to handle this, but this is how i do it:
      
      seek to end-1, check if it is eof, if not seek +1
     */
    BeforeEnd is End - 1,    
    catch(seek(Stream,BeforeEnd,bof,BeforeEnd),_,seek(Stream,0,eof,_)),
    (	at_end_of_stream(Stream)
    ->	TPos=End
    ;   get_code(Stream,_), %seek +1
    	next_token_position_X(Stream,CommentsMap,TPos)
	).
	
next_token_position_X(Stream,CommentsMap,TPos):-
    repeat,
    	peek_code(Stream,C),
    	( C >= 0, code_type(C,space)
	   	->	get_code(Stream,C),
    		fail
    	;	% the character may be the start of a comment. Skip over it.
    		\+ skip_comment(Stream,CommentsMap)
    	),
    !,
	character_count(Stream,TPos).


skip_comment(Stream,CommentsMap):-
    character_count(Stream,Pos),
    pdt_map_get(CommentsMap,Pos,_-Comment),
    string_length(Comment,Len),
    seek(Stream,Len,current,Len).
    
    
/*    
attach_arg_comments([],_,_,_,[]):-
	!.
attach_arg_comments(Args,_,_,[],Args):-
    !.
attach_arg_comments([ArgIn|ArgsIn],CommentsMap,Stream,Positions,[ArgOut|ArgsOut]):-
    pdt_attach_comments(ArgIn,CommentsMap,Stream,Positions,RemainingPositions,ArgOut),
    attach_arg_comments(ArgsIn,CommentsMap,Stream,RemainingPositions,ArgsOut).
*/  

%% pdt_comment_dom(+File, +Pos, +CommentString, -Dom)
% Parse raw comment data into a DOM
%
% This predicate is used as an interface to 
% functionality from the pldoc package, which 
% is still under development.
% 
% @param FileSpec 		The file containing the comment
% @param Pos 			The comment position. this is a stream_position_data term.
% @param CommentString	The raw comment string,
% @param Dom			Will be unified with a DOM term that can for instance be used with pdt_dom_html/3.
pdt_comment_dom(FileSpec,Pos,CommentString,Dom):-
    pdt_file_spec(FileSpec,File),
    process_comment(File, Pos-CommentString, Dom).

process_comment(File, Pos-String, DOM) :-
	stream_position_data(line_count, Pos, Line),
	FilePos = File:Line,
	is_structured_comment(String, Prefixes),
	indented_lines(String, Prefixes, Lines),
	(   section_comment_header(Lines, Header, Lines1)
	->  DOM = [Header|DOM1],
	    Args = []
	;   process_modes(Lines, FilePos, Modes, Args, Lines1)
	->  DOM = [\pred_dt(Modes,pred,[]), dd(class=defbody, DOM1)]
	),
	wiki_lines_to_dom(Lines1, Args, DOM0),
	strip_leading_par(DOM0, DOM1).
	
%% pdt_comment_summary(+CommentString, -Summary)
% Extract summary line from a source comment.
%
% This predicate is used as an interface to functionality from the pldoc package, which is still under 
% development.
% 
% @param FileSpec 		The file containing the comment
% @param Pos 			The comment position. this is a stream_position_data term.
% @param CommentString	The raw comment string,
% @param Summary			Will be unified with a summary String.
pdt_comment_summary(FileSpec,Pos,String,Head,Summary):-    
    pdt_file_spec(FileSpec,File),
	stream_position_data(line_count, Pos, Line),
	FilePos = File:Line,
	is_structured_comment(String, Prefixes),
	indented_lines(String, Prefixes, Lines),
	process_modes(Lines, FilePos, Modes0, _Args0, Lines1),
	copy_term(Modes0,Modes),
	member(mode(Head,Args),Modes),
	execute_elms(Args),	
	summary_from_lines(Lines1,SummaryString),
	string_to_atom(SummaryString,Summary).


execute_elms([]).
execute_elms([Goal|Goals]):-
	Goal,
	execute_elms(Goals).


%% pdt_dom_html(+File, +Dom, -Html)
% Render a DOM term to HTML text.
%
% This predicate is used as an interface to functionality from the the pldoc package, which is still under 
% development.
% 
% @param FileSpec 		The file containing the comment.
% @param Dom			A DOM term as created by pdt_comment_dom/4.    
% @param Html			Will be unified with an atom containing HTML data.
pdt_dom_html(_File,Dom,HTML):-
    new_memory_file(MF),
    open_memory_file(MF,write,Out),
    call_cleanup(
	    doc_write_html(Out,  Dom),
    	close(Out)
    ),
    memory_file_to_atom(MF,HTML).       
    
doc_write_html(Out, DOM) :-
	pldoc_html:phrase(html(DOM), Tokens),
	print_html(Out, Tokens).
     