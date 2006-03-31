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

:- module(plast,
	[	plast_node/1,
		plast_prop/2,
		plast_new_node/2,
		plast_del_tree/1,
		plast_set_prop/2,
		plast_del_prop/2
	]
).

:-dynamic node_id/1,node_attr/2.


plast_prop(compilation_unit_node(P),child(C)):-
    plast_prop(compilation_unit_node(P),member(C)).
plast_prop(brace_node(P),child(C)):-
    plast_prop(brace_node(P),argument(C)).    
plast_prop(list_node(P),child(C)):-
    plast_prop(list_node(P),elements(L)),
    member(C,L).            
plast_prop(compound_node(P),child(C)):-
    plast_prop(compound_node(P),arguments(L)),
    member(C,L).  
plast_prop(Id,Prop):-
    node_attr(Id,Prop).
    
plast_node(Id):-
	node_id(Id).
	    

unused_id(Type,Id):-
    atom_concat(Type,'_node',Fun),
	Num is random(1073741824),
	Try=..[Fun,Num],
    ( node_id(Try)
    ->unused_id(Type,Id)
    ; Id=Try
    ).	

% plast_new_node(+Type,-Id)
% creates a new node of a given type
plast_new_node(Type,Id):-
    unused_id(Type,Id),
    my_assert(node_id(Id)).
    

% plast_del_tree(+Id)
% completely removes an entire subtree.
% Will throw an exception if the node is still 
% referenced by its parent. (remove this link first)
plast_del_tree(Id):-
    (	plast_prop(Id,parent(P)),
    	plast_prop(P,child(Id))
    ->	throw(error(plast_del_tree(Id),referenced_by_parent))
    ;	do_del_tree(Id)
    ).

do_del_tree(Id):-
	forall(plast_prop(Id,child(C)),do_del_tree(C)),
	do_del_node(Id).

do_del_node(Id):-
    retractall(node_attr(Id,_)),
    retractall(node_id(Id)).
	    
plast_set_prop(Id,Prop):-
	my_assert(node_attr(Id,Prop)).

plast_del_prop(Id,Prop):-
	retractall(node_attr(Id,Prop)).	 
	
my_assert(node_id(Id)):-
    ( 	plast_node(Id)
    ->	true
	;   assert(node_id(Id))
	).

my_assert(node_attr(Id,Attr)):-
    (	plast_prop(Id,Attr)
    ->	true
    ;	assert(node_attr(Id,Attr))
    ).
    
    
	   