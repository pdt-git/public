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

:- module(source,
[
	source_node/1,
	source_node_property/2,
	create_source_node/2,
	delete_source_node/1,
	add_source_node_property/2,
	delete_source_node_property/2
]).

:- dynamic parsed_node_attr/2.
:- dynamic parsed_node_id/1.

create_source_node(Type,Id):-
    unused_id(Type,Id),
    my_assert(parsed_node_id(Id)).
    
delete_source_node(Id):-
	retractall(parsed_node_id(Id)),
	retractall(parsed_node_property(Id,_)).

add_source_node_property(Id,Prop):-
    my_assert(parsed_node_attr(Id,Prop)).

delete_source_node_property(Id,Prop):-
    retractall(parsed_node_attr(Id,Prop)).
    
source_node(source_folder_node(N)):-
    parsed_node_id(source_folder_node(N)).
source_node(compilation_unit_node(N)):-
    parsed_node_id(compilation_unit_node(N)).
source_node(atom_node(N)):-
    parsed_node_id(atom_node(N)).
source_node(variable_node(N)):-
    parsed_node_id(variable_node(N)).
source_node(string_node(N)):-
    parsed_node_id(string_node(N)).
source_node(brace_node(N)):-
    parsed_node_id(brace_node(N)).
source_node(list_node(N)):-
    parsed_node_id(list_node(N)).
source_node(compound_node(N)):-
    parsed_node_id(compound_node(N)).        
    

source_node_property(source_folder_node(Num),type(source_folder)):-
    source_node(source_folder_node(Num)).
source_node_property(compilation_unit_node(Num),type(compilation_unit)):-
    source_node(compilation_unit_node(Num)).
source_node_property(atom_node(Num),type(atom)):-
    source_node(atom_node(Num)).
source_node_property(variable_node(Num),type(variable)):-
    source_node(variable_node(Num)).
source_node_property(string_node(Num),type(string)):-
    source_node(string_node(Num)).
source_node_property(brace_node(Num),type(brace)):-
    source_node(brace_node(Num)).
source_node_property(list_node(Num),type(list)):-
    source_node(list_node(Num)).
source_node_property(compound_node(Num),type(compound)):-
    source_node(compound_node(Num)).    
source_node_property(Id,Prop):-
    parsed_node_attr(Id,Prop).       
source_node_property(compound_node(Num),arguments([])):-
    source_node(compound_node(Num)),
    \+ parsed_node_attr(compound_node(Num),arguments(_)).       
source_node_property(list_node(Num),elements([])):-
    source_node(list_node(Num)),
    \+ parsed_node_attr(list_node(Num),elements(_)).       
    
    
my_assert(parsed_node_id(Id)):-
    %format("assert(~w)~n",[Term]),
    ( 	parsed_node_id(Id)
    ->	true
	;   assert(parsed_node_id(Id))
	). 

my_assert(parsed_node_attr(Id,Attr)):-
    %format("assert(~w)~n",[Term]),
    my_assert(parsed_node_id(Id)),
    (	parsed_node_attr(Id,Attr)
    ->	true
    ;	assert(parsed_node_attr(Id,Attr))
    ).

unused_id(Type,Id):-
    atom_concat(Type,'_node',Fun),
	Num is random(1073741824),
	Try=..[Fun,Num],
    ( parsed_node_id(Try)
    ->unused_id(Type,Id)
    ; Id=Try
    ).	
     