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

:-module(runtime,
	[predicates_defined_in/2,
	 predicates_defined_in/5,
	 predicates_defined_in/4,
	 clauses_defined_in/2,
	 clause_ref/1,
	 clause_of/4,
	 clause_of/3,
	 term_type/2]).



predicates_defined_in(M, Head) :-
	current_predicate(_, M:Head),
	\+ predicate_property(M:Head, imported_from(_)).


predicates_defined_in(M,Head, Name, Arity) :-
	(	nonvar(Name),
		nonvar(Arity)
	-> 	functor(Head,Name,Arity),
	    predicates_defined_in(M,Head)
	;	predicates_defined_in(M,Head),
		functor(Head,Name,Arity)
	).

    
predicates_defined_in(M, Head, Name, Arity, Exported) :-
    predicates_defined_in(M,Head, Name,Arity),
	(	predicate_property(M:Head,exported)
	->	Exported=true
	;	Exported=false
	).


clause_ref(Ref):-
    (	nonvar(Ref)
    ->	catch(nth_clause(_,_,Ref),_,fail)
    ;	current_predicate(_,_:Pred),
		nth_clause(Pred,_,Ref)
	).

clause_of(Module,Name,Arity,Index):-
    (	ground(Name/Arity)
    ->	functor(Head,Name,Arity),
    	clause_of(Module,Head,Index)
    ;	clause_of(Module,Head,Index),
    	functor(Head,Name,Arity)
    ).

clause_of(Module,Head,Index):-
    predicates_defined_in(Module,Head),
    Module:nth_clause(Head,Index,_).
    
clauses_defined_in(File,ClauseRef):-
	current_predicate(_,_:Pred),
	nth_clause(Pred,_,ClauseRef),
	clause_property(ClauseRef,file(File)).	

term_type(Term, var):-
    var(Term),!.
	
term_type(Term, string):-
	string(Term),!.	

term_type(Term, integer):-
	integer(Term),!.    

term_type(Term, float):-
	float(Term),!.    

term_type(Term, number):-
	number(Term),!.    

term_type(Term, atom):-
    atom(Term),!.

term_type(Term, list):-
	is_list(Term),!.

term_type(Term, compound):-
    %\+ is_list(Term), eigentlich sind listen auch compounds. (ld)
    compound(Term),!.
    