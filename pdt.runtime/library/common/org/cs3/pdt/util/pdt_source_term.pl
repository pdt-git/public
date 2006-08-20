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


:- module(pdt_source_term,[
	source_term/2,
	source_term_arg/3,
	source_term_expand/2,
	source_term_functor/3,
	source_term_property/3,
	source_term_set_property/4
]).


:- dynamic source_term_hook/2.
:- multifile source_term_hook/2.

%% source_term(?SourceTerm)  
% verify that the argument a source term.
%
% I.e. _somehow_ refering to a character sequence read 
% from an actual source file.
source_term(SourceTerm,Module):-
    source_term_hook(SourceTerm,Module),
    !.
source_term(_,pdt_source_term).

%% source_term_expand(+SourceTerm, -Term) 
% Expand source term
%
% if the term is - as in e.g. with "annotated terms" - wrapped 
% in some data structure, unify the "actual" 
% (plain, unwrapped, ...) term with the second argument.
source_term_expand(SourceTerm,Term):-
    source_term(SourceTerm,Module),
    Module:current_predicate(source_term_expand_hook/2),
    !,
    Module:source_term_expand_hook(SourceTerm,Term).
source_term_expand(Term,Term).


%% source_term_functor(+SourceTerm, Name,Arity) 
% access the principal functor of a source term.
source_term_functor(SourceTerm,Name,Arity):-
    source_term(SourceTerm,Module),
    Module:current_predicate(source_term_functor_hook/3),
    !,
    Module:source_term_functor_hook(SourceTerm,Name,Arity).
source_term_functor(SourceTerm,Name,Arity):-
    functor(SourceTerm,Name,Arity).

%% source_term_arg(?ArgNum, +SourceTerm, ?ArgValue) 
% like arg/3, only that second and third arguments are source terms.
source_term_arg(ArgNum,SourceTerm,ArgVal):-
    source_term(SourceTerm,Module),
    Module:current_predicate(source_term_arg_hook/3),
    !,
    Module:source_term_arg_hook(ArgNum,SourceTerm,ArgVal).
source_term_arg(ArgNum,SourceTerm,ArgVal):-
    arg(ArgNum,SourceTerm,ArgVal).
    

%% source_term_property(+SourceTerm, +Key, -Value) 
% access term annotation.
source_term_property(SourceTerm,Key,Property):-
    source_term(SourceTerm,Module),
    Module:current_predicate(source_term_property_hook/3),
    !,
    Module:source_term_property_hook(SourceTerm,Key,Property).



%% source_term_set_property(+SourceTerm, +Key, -Value, -NewSourceTerm) 
% modify term annotation.
source_term_set_property(SourceTerm,Key,Value,NewSourceTerm):-
    source_term(SourceTerm,Module),
    Module:current_predicate(source_term_set_property_hook/4),
    !,
    Module:source_term_set_property_hook(SourceTerm,Key,Value,NewSourceTerm).


