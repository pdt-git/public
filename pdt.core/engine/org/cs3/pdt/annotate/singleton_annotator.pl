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

:-module(singleton_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).


:- pdt_annotator([term],[/*library('/org/cs3/pdt/annotate/variable_name_annotator')*/]).


term_annotation_hook(_,_,_,InTerm,OutTerm):-
	pdt_term_annotation(InTerm,_,Annos),
    pdt_member(singletons(Singletons),Annos),
    pdt_member(variable_names(Variables),Annos),

	%% check the exports list 
	check_singletons(InTerm,Singletons,TmpTerm),
	check_no_singletons(TmpTerm,Variables,OutTerm).

    
check_singletons(In,Singletons,Out):-

	%% find ill-formed elements in the exports list
	findall(singleton(Path,Singleton,TVs),
		(	pdt_subterm(In,Path,Singleton),
			pdt_term_annotation(Singleton,Var,_),
			member(Name=Value,Singletons),
			Var==Value,
			\+atom_concat('_',_,Name),
			term_variables(In,TVs)
		),
		Occurances
	),
	add_singleton_annos(In,Occurances,Out).

add_singleton_annos(In,[],In).
add_singleton_annos(In,[singleton(Path,InExport,TVs)|IFEs],Out):-
    term_variables(In,TVs),
    pdt_term_annotation(InExport,Term,Annotation),
    pdt_term_annotation(OutExport,Term,[problem(warning(singleton))|Annotation]),
    pdt_subst(In,Path,OutExport,Next),
    add_singleton_annos(Next,IFEs,Out).

check_no_singletons(In,[],In).
check_no_singletons(In,[Variable|Variables],Out):-
	check_no_singleton(In,Variable,Tmp),
	check_no_singletons(Tmp,Variables,Out).

check_no_singleton(In,Name=Variable,Out):-
    atom_concat('_',_,Name),
    !,
	findall(no_singleton(Path,Singleton,TVs),
		(	pdt_subterm(In,Path,Singleton),
			pdt_term_annotation(Singleton,Var,_),
			Variable==Var,
			term_variables(In,TVs)
		),
		Occurances
	),
	length(Occurances,Count),
	(	Count>1
	->	add_no_singleton_annos(In,Occurances,Out)
	;	In=Out
	).
check_no_singleton(In,_,In).

add_no_singleton_annos(In,[],In).
add_no_singleton_annos(In,[no_singleton(Path,InExport,TVs)|IFEs],Out):-
    term_variables(In,TVs),
    pdt_term_annotation(InExport,Term,Annotation),
    pdt_term_annotation(OutExport,Term,[problem(warning(no_singleton))|Annotation]),
    pdt_subst(In,Path,OutExport,Next),
    add_no_singleton_annos(Next,IFEs,Out).
    
