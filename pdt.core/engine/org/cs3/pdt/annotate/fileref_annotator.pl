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

:-module(fileref_annotator,[]).

:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:-use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).



term_pre_annotation_hook(Stack,_,InTerm,OutTerm):-
	file_refs(Stack,InTerm,OutTerm).

file_pre_annotation_hook(_,_,Terms,InAnos,[references_files(Refs)|InAnos]):-
    collect_refs(Terms,Refs).


file_refs(Stack,InTerm,OutTerm):-
    pdt_strip_annotation(InTerm,Term,(Head,Tail)),
    find_file_refs(Term,Refs),   
    resolve_file_Refs(Refs,ResRefs),
    annotate_refered_files(Stack,ResRefs),
    pdt_splice_annotation(Term,([file_refs(ResRefs)|Head],Tail),OutTerm).

    
find_file_refs(:-[H,T],[H,T]).
find_file_refs(:-load_files([H,T],_),[H,T]).
find_file_refs(:-load_files(H,_),[H]):-
    \+ is_list(H).
find_file_refs(:-consult([H,T]),[H,T]).
find_file_refs(:-consult(H),[H]):-
    \+ is_list(H).
find_file_refs(:-ensure_loaded([H,T]),[H,T]).
find_file_refs(:-ensure_loaded(H),[H]):-
    \+ is_list(H).
find_file_refs(:-include(H),[H]):-
    \+ is_list(H).    
find_file_refs(:-use_module([H,T]),[H,T]).
find_file_refs(:-use_module(H),[H]):-
    \+ is_list(H).
find_file_refs(:-use_module([H,T],_),[H,T]).
find_file_refs(:-use_module(H),_,[H]):-
    \+ is_list(H).

resolve_file_Refs([],[]).
resolve_file_Refs([H|T],[RH|RT]):-
    pdt_file_spec(H,RH),
    resolve_file_Refs(T,RT).

annotate_refered_files(_,[]).
annotate_refered_files(Stack,[H|T]):-
%    the_debug(Stack,H),
    annotate_refered_file([H|Stack]),
%    this_debug(Stack),
    annotate_refered_files(Stack,T).

annotate_refered_file([H|Stack]):-
	member(H,Stack),!.    
annotate_refered_file(Stack):-
	ensure_annotated(Stack).    	

%the_debug([Cur|_],H):-
%    format("~w refers to ~w~n",[Cur,H]).
%
%this_debug([Cur|_]):-
%    format("back to ~w~n",[Cur]).

collect_refs([],[]).
collect_refs([H|T],Out):-
    pdt_strip_annotation(H,_,(Head,_)),
    (	pdt_member(file_refs(Refs),Head)
    ->  sort(Refs,SortedRefs)
    ;	SortedRefs=[]
    ),
    collect_refs(T,Rest),
    merge_set(SortedRefs,Rest,Out).
    
    