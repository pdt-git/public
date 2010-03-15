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

:-module(mi_meta_ops,[mi_unify/2,mi_apply_subst/2]).

% unify(+SubstBefore,?T1,?T2,-SubstAfter)
%
% called by the meta interpreter when a unification should accur.
% SubstBefore is a list of variable=substitution terms
% should succeed when T1 and T2 are unifyable when applying SubstBefore 
% in praxis, variables in T1 or T2 will already be substituted according to 
% SubstBefore.
% should find out which additional substitutions WOULD BE required to unify
% T1 and T2.
% should unify SubstAfter with the union of SubstBefore and those additional 
% bindings.
% Should *NOT* change instantiation state of ISubst, T1 or T2.
% In particular, shoul *NOT* unify T1 and T2.
% 
mi_unify(T1,T2):-
  mi_apply_subst(T1,ST1),
  mi_apply_subst(T2,ST2),
  unifyable(ST1,ST2,Subst),
  attribute_subst_vars(Subst).
  
attribute_subst_vars([]).
attribute_subst_vars([Var=Val|T]):-
    put_attr(Var,meta_ops,Val),
    %format("~w->~w;~n",[Val,Var]),
    writeln(unify(Var=Val)),
    attribute_subst_vars(T).

% when this one get's called, we have an error in the 
% meta interpreter: substituted variables should never
% actualy be unified.
attr_unify_hook(AttValue,VarValue):-
    writeln('bug in meta int'),
    throw(exception(should_not_happen(AttValue,VarValue))).

  
% apply_subst(+Subst, +InTerm,-OutTerm)
%
% called by the meta interpreter when a variable substitution should be applied
% to a given term.
% should always succeed.
% should unify OutTerm with a *COPY* of InTerm that reflects the result of the substitution.
% should *NOT* alter the instantiation state of Subst or InTerm
%
% the current implementation uses attributed variables and does not
% need the subst.
mi_apply_subst(T1,T2):-
  subst(T1,T2).
  
subst(GroundTerm,GroundTerm):-
    ground(GroundTerm),
    !.
subst(Var,Subst):-
	var(Var),
	!,
	(	attvar(Var)
	->	get_attr(Var,meta_ops,Value),		
		subst(Value,Subst)
		%writeln(subst(Var=Subst))
	;	Var=Subst
	).	
subst(InTerm,OutTerm):-
	InTerm=..[Functor|Args],
	!,
	subst_args(Args,OutArgs),
	OutTerm=..[Functor|OutArgs].

subst_args([],[]).
subst_args([InTerm|InTail],[OutTerm|OutTail]):-
    subst(InTerm,OutTerm),
    subst_args(InTail,OutTail).