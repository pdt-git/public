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


% abstraction layer arround annotation.
% this resambles the subset of the interface of multimaps, because i want to eventualy switch 
% from flat lists to multi maps. I won't do this, however, before all depending code has been
% refactored to use this layer.


:- module(pdt_util_annotation,[
	pdt_annotation_empty/1,
	pdt_annotation_add/4,
	pdt_annotation_remove/4,
	pdt_annotation_remove_all/3,
	pdt_annotation_get/3,
	pdt_annotation_get_set/3,
	pdt_annotation_get_list/3,
	pdt_annotation_to_list/2
]).


pdt_annotation_get_set(M,Key,Set):-
    get_values(M,Key,Set).

pdt_annotation_get_list(M,Key,List):-
    get_values(M,Key,Set),
    pdt_set_to_list(Set,List).



pdt_annotation_to_list(M,M).
    
pdt_annotation_empty([]).

pdt_annotation_get(In,Key,Value):-
    Prop=..[Key,Value],
    member(Prop,In).

    
pdt_annotation_add(In,Key,Value,[Prop|In]):-
    Prop=..[Key,Value].
	


get_values(Prop,Key,Values):-
    Prop=..[Key,Value],
	findall(Value,member(Key,Value),Values).    

    
pdt_annotation_remove(In,Key,Value,Out):-
	Prop=..[Key,Value],
	findall(
		P,
		(	member(P,In),
			\+ P=Prop
		),
		Out
	).
		
pdt_annotation_remove_all(In,Key,Out):-
	pdt_annotation_remove(In,Key,_,Out).
