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

:- module(pdt_util_aterm,[
	pdt_strip_annotation/3,
	pdt_splice_annotation/3,
	pdt_top_annotation/2,
	pdt_term_annotation/3
]).

%@deprecated: use pdt_term_annotation/3
pdt_top_annotation(aterm(A,_),A).

%pdt_term_annotation(?AnnotatedTerm,?Term,?Annotation).
% succeeds if AnnotatedTerm is an annotated term, Annotation is the top annotation, and
% Term is the unwrapped toplevel (its arguments are still annotated).
pdt_term_annotation(aterm(A,T),T,A).

pdt_strip_annotation(AnotatedTerm,Term,Anotation):-
    nonvar(AnotatedTerm),check_aterm(AnotatedTerm),
    !,
    strip(AnotatedTerm,Term,Anotation).

pdt_splice_annotation(Term,Anotation,AnotatedTerm):-
    check_annotation(Term,Anotation),
    !,
    splice(Term,Anotation,AnotatedTerm).

check_annotation(Term,(TopAn,ArgsAn)):-
    nonvar(ArgsAn),    
    unbound_tail(TopAn),
 	(   var(Term)
 	->	ArgsAn=[]
	;	Term=..[_|Args],
		functor(Term,_,Arity),
    	length(ArgsAn,Arity),
    	check_arg_annotations(Args,ArgsAn)
    ),!.
check_annotation(Term,Anotation):-
    throw(invalid_annotation(term(Term),annotation(Anotation))).
    
check_arg_annotations([],[]).
check_arg_annotations([H|T],[AnH|AnT]):-
    check_annotation(H,AnH),
    check_arg_annotations(T,AnT).

check_aterm(aterm(Top,AnotatedTerm)):-
    unbound_tail(Top),
    (	var(AnotatedTerm)
    ->	true
    ;	AnotatedTerm=..[_|Args],
    	check_arg_aterms(Args)
    ),!.
check_aterm(Term):-
	throw(invalid_aterm(Term)).    

check_arg_aterms([]).
check_arg_aterms([H|T]):-
	check_aterm(H),
	check_arg_aterms(T).
	
	    
unbound_tail(Term):-
    var(Term),!.
unbound_tail('.'(_,Tail)):-
    unbound_tail(Tail).

strip(aterm(HeadAnotation,AnotatedTerm),AnotatedTerm,(HeadAnotation,[])):-
	var(AnotatedTerm),
	!.
strip(aterm(HeadAnotation,AnotatedTerm),Term,(HeadAnotation,ArgAnotations)):-
	AnotatedTerm=..[Functor|AnotatedArgs],
	strip_args(AnotatedArgs,Args,ArgAnotations),
	Term=..[Functor|Args].

strip_args([],[],[]).
strip_args(	[AnotatedArgsH|AnotatedArgsT],
			[ArgsH|ArgsT],
			[ArgAnotationsH|ArgAnotationsT]):-
	strip(AnotatedArgsH,ArgsH,ArgAnotationsH),
	strip_args(AnotatedArgsT,ArgsT,ArgAnotationsT).
	
splice(Term,(HeadAnotation,[]),aterm(HeadAnotation,Term)):-
	var(Term),
	!.	
splice(Term,(HeadAnotation,ArgAnotations),aterm(HeadAnotation,AnotatedTerm)):-
    Term=..[Functor|Args],
    splice_args(Args,ArgAnotations,AnotatedArgs),
    AnotatedTerm=..[Functor|AnotatedArgs].

splice_args([],[],[]).
splice_args([ArgsH|ArgsT],
			[ArgAnotationsH|ArgAnotationsT],
			[AnotatedArgsH|AnotatedArgsT]):-
    splice(ArgsH,ArgAnotationsH,AnotatedArgsH),
    splice_args(ArgsT,ArgAnotationsT,AnotatedArgsT).
