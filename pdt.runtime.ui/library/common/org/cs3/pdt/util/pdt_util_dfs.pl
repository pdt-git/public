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


:- module(pdt_util_dfs,[pdt_dfs/5]).

:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_rbtree')).


:-module_transparent pdt_dfs/5.
%% pdt_dfs(+Start,+EdgeGoal,+EdgeFrom,+EdgeTo,-Found).
% perform a (cycle-safe) depth-first search on an arbitrary graph.
%
% @param EdgeGoal should be a Goal that unifies EdgeFrom and EdgeTo with two nodes
%        that are connected by an edge. 
% @param Start should the node at which the search is started.
% @param Found will successfully be unified with all found nodes.
%
% $Note:if a node can be reached along different paths, it may be reported more than once.
% Along a single path, each node is only considered once, so this predicate may be used on
% cyclic graphs.

pdt_dfs(Start,EdgeGoal,From,To,Found):-
	pdt_rbtree_new(Visited0),
	context_module(Module),
	pdt_util_dfs:dfs(Visited0,Start,Module:EdgeGoal,From,To,Found).
	
dfs(Visited,Start,_,_,_,_):-
    pdt_rbtree_lookup(Start,_,Visited),
    !,
    fail.
dfs(_,Node,_,_,_,Node).
dfs(Visited,Node,EdgeGoal,From,To,Found):-
    pdt_rbtree_insert(Visited,Node,visited,NextVisited),
    copy_term((EdgeGoal,From,To),(NextEdgeGoal,NextFrom,NextTo)),
    From=Node,
    EdgeGoal,
    dfs(NextVisited,To,NextEdgeGoal,NextFrom,NextTo,Found).
    