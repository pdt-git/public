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


% implements a simple mutable, non-destructive, sets
% currently uses rbtrees, but client code should not rely on this.
% no set operations are currently impplemented


:- module(pdt_util_set,[
	pdt_set_empty/1,
	pdt_set_add/3,
	pdt_set_remove/3,
	pdt_set_element/2,
	pdt_set_to_list/2,
	pdt_set_findall/3,
	pdt_set_addall/4
]).

:- use_module(library('/org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).

:-module_transparent pdt_set_findall/3,pdt_set_addall/4.

pdt_set_findall(Elm,Goal,Set):-
	pdt_set_empty(Set0),
	pdt_util:pdt_unique(pdt_set_findall,U),
	nb_setval(U,Set0),
	forall(Goal,
		(	nb_getval(U,SetIn),
			pdt_set_add(SetIn,Elm,SetOut),
			nb_setval(U,SetOut)
		)
	),
	nb_getval(U,Set),
	nb_delete(U).

pdt_set_addall(Set0,Elm,Goal,Set):-
	pdt_util:pdt_unique(pdt_set_addall,U),
	nb_setval(U,Set0),
	forall(Goal,
		(	nb_getval(U,SetIn),
			pdt_set_add(SetIn,Elm,SetOut),
			nb_setval(U,SetOut)
		)
	),
	nb_getval(U,Set),
	nb_delete(U).


pdt_set_empty(Set):-
    pdt_map_empty(Set).

pdt_set_to_list(Set,List):-
    findall(Elm,pdt_set_element(Set,Elm),List).

pdt_set_add(In,Elm,Out):-
    pdt_map_put(In,Elm,true,Out).
pdt_set_remove(In,Elm,Out):-
    pdt_map_remove(In,Elm,Out).
pdt_set_element(In,Elm):-
    pdt_map_get(In,Elm,_).

