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

:- module(model_utils,[
	node/2,
	parse_property/2,
	parse_property_list/2,
	write_node/1,
	write_tree/1,
	delete_tree/1
]).

:- use_module(model).


node(Id,List):-
    var(List),
    !,
    setof(Att,node_property(Id,Att),List).
node(Id,[]):-
    node(Id).
node(Id,[HeadAttr|TailAttr]):-
    nonvar(HeadAttr),
    nonvar(TailAttr),
    !,
    node_property(Id,HeadAttr),
   	node(Id,TailAttr).
   	       
write_node(Id):-
    format("<<~w>>~n",Id),
    forall(node_property(Id,P),format(" --> ~w~n",[P])).
   	

write_tree(Id):-
	write_tree(Id,'','').
	   	
write_tree(Id,Indent,Arrow):-
    format("~w~w<<~w>>~n",[Indent,Arrow,Id]),
    atom_concat(Indent,'|    ',ChildIndent),
    forall(post(Id,Child),write_tree(Child,ChildIndent,'|--')).    

delete_tree(Node):-
    is_source(Node),
    delete_tree_rec(Node).

delete_tree_rec(Node):-
	forall(post(Node,Child),delete_tree_rec(Child)),
	retractall(node_attr(Node,_)),
	retractall(node_id(Node)).
			
    
%TODO find a better name for these predicates.
parse_property(In,Out):-
	In=..[Functor|Args],
	parse_property(Functor,Args,Out).
parse_property(Functor,[],Functor->true).
parse_property(Functor,[Arg|[]],Functor->Arg).		    

parse_property_list(In,Out):-
    maplist(parse_property,In,Out).

post(Node,Child):-
	adjacent(Node,outgoing,_,Child).