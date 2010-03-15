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


/**
 this module is used to maintain a topological ordering of nodes in an acyclic directed graph.
 it is designed with relatively small graphs in mind, that do not change often.
 E.g. the pdt uses it to determine the order in which annotator modules should be processed.
*/
:- module(pdt_util_dependency,[
	pdt_add_dependency/3, 
	pdt_process_order/2,
	pdt_add_node/2]).

:- dynamic process_order_next/3.
:- dynamic node/2.
process_order_first(Graph,Node,First):-
    process_order_next(Graph,Prev,Node),
    !,
    process_order_first(Graph,Prev,First).
process_order_first(Graph,Node,Node):-
    node(Graph,Node).

process_order_last(Graph,Node,Last):-
    process_order_next(Graph,Node,Next),
    !,
    process_order_last(Graph,Next,Last).
process_order_last(Graph,Node,Node):-
    node(Graph,Node).

%% pdt_add_dependency(+Graph,+Node,+Dependency)
% add an edge to the dependency graph.
% 
% @param Graph the graph identifier
% @param Node the dependend node. It will be created if it does not exist.
% @param Dependency the node on which Node depends. It will be created if it does not exist.
pdt_add_dependency(Graph,Node,Dependency):-
    add_node(Graph,Node),
    add_node(Graph,Dependency),
    add_dependency(Graph,Node,Dependency).
%% pdt_add_node(+Graph,+Node)
% add a node to a dependency graph.
%
% If the node exists already, nothing happens.
% @param Graph the graph identifier.
% @param Node the node to be added.
pdt_add_node(Graph,Node):-
    add_node(Graph,Node).
    
add_node(Graph,Node):-
    (	node(Graph,Node)
    ->	true
    ;	assert(node(Graph,Node))
    ).
    
add_dependency(Graph,Node,Dependency):-
    process_order_before(Graph,Node,Dependency),
    !,
    throw(error(dependency_cycle(Node,Dependency))).
add_dependency(Graph,Node,Dependency):-
    process_order_before(Graph,Dependency,Node),
    !. %nothing to do.
add_dependency(Graph,Node,Dependency):-    
	process_order_last(Graph,Dependency,A),
	process_order_first(Graph,Node,B),
	(	process_order_next(Graph,A,B)
	->	true
	;	assert(process_order_next(Graph,A,B))
	).

%process_order_before(Node,Node). %TODO: not sure. do we allow trivial circles?
process_order_before(Graph,Before,After):-
    process_order_next(Graph,Before,After).
 process_order_before(Graph,Before,After):-
    process_order_next(Graph,Before,Next),
    process_order_before(Graph,Next,After).
      
    
    

process_order_root(Graph,Root):-
    node(Graph,Root),
    process_order_first(Graph,Root,Root).
    
process_order(Graph,Root,[Root|Tail]):-
    process_order_next(Graph,Root,Next),
    !,
    process_order(Graph,Next,Tail).
process_order(_Graph,Root,[Root]).

%% pdt_process_order(+Graph,-Order)
% calculate a process order for a dependency graph.
%
% @param Graph is the graph identifier
% @param Order is a list of nodes in which each node is preceeded by the nodes it depends on.
pdt_process_order(Graph,Order):-
    process_order(Graph,Order).

process_order(Graph,Order):-
	findall(SubOrder,(process_order_root(Graph,Root),process_order(Graph,Root,SubOrder)),SubOrders),
	flatten(SubOrders,Order).
