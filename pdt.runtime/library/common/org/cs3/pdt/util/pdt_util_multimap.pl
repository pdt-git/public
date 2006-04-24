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


% implements mutable, non-destructive, multi-maps 
% currently uses rbtrees, but client code should not rely on this.
%


:- module(pdt_util_multimap,[
	pdt_multimap_empty/1,
	pdt_multimap_add/4,
	pdt_multimap_remove/4,
	pdt_multimap_remove_all/3,
	pdt_multimap_get/3,
	pdt_multimap_get_set/3,
	pdt_multimap_get_list/3,
	pdt_multimap_next/4,	
	pdt_multimap_to_list/2
]).

:- use_module(library('/org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_set')).


pdt_multimap_get_set(M,Key,Set):-
    get_values(M,Key,Set).

pdt_multimap_get_list(M,Key,List):-
    get_values(M,Key,Set),
    pdt_set_to_list(Set,List).



pdt_multimap_to_list(M,L):-
    findall(Term,
    	(
    		pdt_map_get(M,Key,Values),
    		pdt_set_to_list(Values,ValueList),
    		values_to_property(Key,ValueList,Term)    		
    	),
    	L
    ).
    
values_to_property(_,[],_):-
    !, fail.
values_to_property(Key,[Value],Property):-
    !,
	functor(Property,Key,1),
	arg(1,Property,Value).
values_to_property(Key,Values,Property):-
	functor(Property,Key,1),
	arg(1,Property,Values).

pdt_multimap_empty(M):-
    pdt_map_empty(M).
pdt_multimap_get(In,Key,Value):-
    get_values(In,Key,Values),
    pdt_set_element(Values,Value).

pdt_multimap_next(M,Start,Key,Value):-
    next_values(M,Start,Key,Values),
    pdt_set_element(Values,Value).
    
pdt_multimap_add(In,Key,Value,Out):-
	get_values(In,Key,Values),
	pdt_set_add(Values,Value,OutValues),
	pdt_map_put(In,Key,OutValues,Out).

get_values(In,Key,Values):-
    pdt_map_get(In,Key,Values),
    !.
get_values(_,_,Values):-
	pdt_set_empty(Values).

next_values(In,Start,Key,Values):-
    pdt_map_next(In,Start,Key,Values).

    
pdt_multimap_remove(In,Key,Value,Out):-
	pdt_map_get(In,Key,Values),
    pdt_set_remove(Values,Value,OutValues),
    	update_or_delete(In,Key,OutValues,Out).

update_or_delete(In,Key,Values,Out):-
    pdt_set_empty(Values),
    !,
    pdt_map_remove(In,Key,Out).
update_or_delete(In,Key,Values,Out):-
    pdt_map_put(In,Key,Values,Out).

pdt_multimap_remove_all(In,Key,Out):-
    pdt_map_remove(In,Key,Out),
    !.
pdt_multimap_remove_all(_,_,_).
	
