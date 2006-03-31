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

:-module(metamodule,
[
	meta_node/1,
	meta_node_property/2,
	meta_edge/1,
	meta_edge_property/2
]).

meta_node(module).
meta_node(predicate).
meta_node(clause).
meta_node(source_folder).
meta_node(compilation_unit).
meta_node(atom).
meta_node(variable).
meta_node(string).
meta_node(brace).
meta_node(list).
meta_node(compound).

meta_node_property(variable,leaf).
meta_node_property(string,leaf).
meta_node_property(brace,leaf).


%% argument syntax is <node_type>(<child_property_name>)
meta_edge(module(predicate)).
meta_edge(predicate(clause)).
meta_edge(source_folder(compilation_unit)).
meta_edge(compilation_unit(clause)).
meta_edge(brace(argument)).
meta_edge(list(elements)).
meta_edge(list(tail)).
meta_edge(compound(arguments)).


implicit_edge(module,predicate).

simple_edge(predicate,clause).
simple_edge(source_folder,compilation_unit).
simple_edge(compilation_unit,clause).
simple_edge(brace,argument).
simple_edge(list,tail).

list_edge(list,elements).
list_edge(compound,arguments).

type_property(atom,leaf).
type_property(variable,leaf).
type_property(string,leaf).

edge_property(predicate,clause,explicit).
edge_property(predicate,clause,multiplicity(*)).
