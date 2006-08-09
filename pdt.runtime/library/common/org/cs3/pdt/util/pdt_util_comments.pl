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

:- module(pdt_util_comments,[
	pdt_attach_comments/6
	]).
	
:- use_module(pdt_util).
:- use_module(pdt_util_aterm).
:- use_module(pdt_util_map).
:- use_module(pdt_util_term_position).
	
%attach_comments(+ATermIn,+InputStream,+CPositions, -RemainingPositions, -ATermOut)
%
%Attaches comment positions to the correct subterms of a term read with read_term.
%
%ATermIn should be an annotated term, as produced by wrap_term/6. It should at least 
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

pdt_attach_comments(ATerm,_,_,[],[],ATerm).
pdt_attach_comments(ATermIn,CommentsMap,Stream,CPositions,RemainingPositions,ATermOut):-
	% read character start and end offsets of the term from the term annotations.	
	pdt_term_annotation(ATermIn,_,AnnosIn),
	pdt_member(position(Start-End),AnnosIn),
	
	% rough separation of comments in three intervals left, mid and right.
	% Left positions are left of the term, mid within abd right positions are at the right of the term.
	pdt_chop_before(Start,CPositions,LeftPositions,TmpPositions),
	pdt_chop_before(End,TmpPositions,MidPositions,RightPositions),
	
	
	% left is easy: all comments that were not claimed by any preceeding term are attached
	% to this term.
	do_attach_left(ATermIn,LeftPositions,ATerm1),
	
	% the middle must belong to the arguments of the term. Recurse on arguments.
	pdt_term_annotation(ATerm1,Subterm1,Annos1),
	(	nonvar(Subterm1)
	->	Subterm1=..[Functor|Args1],
		attach_arg_comments(Args1,CommentsMap,Stream,MidPositions,Args2),
		Subterm2=..[Functor|Args2],
		pdt_term_annotation(ATerm2,Subterm2,Annos1)
	;	true
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
	
do_attach_left(ATermIn,Positions,ATermOut):-
    pdt_term_annotation(ATermIn,Term,Annos),
    pdt_term_annotation(ATermOut,Term,[comments_left(Positions)|Annos]).

do_attach_right(ATermIn,Positions,ATermOut):-
    pdt_term_annotation(ATermIn,Term,Annos),
    pdt_term_annotation(ATermOut,Term,[comments_right(Positions)|Annos]).

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
    
attach_arg_comments([],_,_,_,[]).
attach_arg_comments(Args,_,_,[],Args).
attach_arg_comments([ArgIn|ArgsIn],CommentsMap,Stream,Positions,[ArgOut|ArgsOut]):-
    pdt_attach_comments(ArgIn,CommentsMap,Stream,Positions,RemainingPositions,ArgOut),
    attach_arg_comments(ArgsIn,CommentsMap,Stream,RemainingPositions,ArgsOut).