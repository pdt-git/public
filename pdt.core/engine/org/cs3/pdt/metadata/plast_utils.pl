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

:- module(plast_utils,
	[	contains/2,
		in_toplevel/2,
		in_compilation_unit/2,
		variable_symbol/3,
		toplevel_term/5
	]).
	
:- use_module(plast).
contains(P,P).
contains(P,Q):-
    ground(P),
    !,
    plast_prop(P,child(C)),
    contains(C,Q).
contains(P,Q):-
    ground(Q),
    !,
    plast_prop(Q,parent(C)),
    contains(P,C).

in_toplevel(Node,TLT):-
	contains(TLT,Node),
	plast_prop(TLT,toplevel_term).	    
	
in_compilation_unit(Node,compilation_unit_node(C)):-
	contains(compilation_unit_node(C),Node).

variable_symbol(variable_node(V),Frame,varsym(T,Frame,Term)):-
    in_toplevel(variable_node(V),T),
    plast_prop(variable_node(V),term(Term)).

%operator_type(compound_node(V),prefix):-
%    plast_prop(compound_node(V),functor(F/1)),
%    in_toplevel(compound_node(V),TL),
%    plast_prop(TL,module(Module)),    
%    plast_prop(compound_node(V),functor_position(From-_)),
%    plast_prop(compound_node(V),From-_).


toplevel_term(Id,Type,Module,Head,Body):-
    plast_prop(Id,toplevel_term),
    do_toplevel_term(Id,Type,M,Head,Body),
    (	M==[]
    ->	plast_prop(Id,module(Module))
    ;	Module=M
    ).
    
do_toplevel_term(Id,directive,[],[],Body):-
    plast_prop(Id,functor((:-)/1)),
    plast_prop(Id,arguments([Body])).
do_toplevel_term(Id,rule,Module,Head,Body):-
    plast_prop(Id,functor((:-)/2)),
    plast_prop(Id,arguments([Left,Body])),
    (	plast_prop(Left,functor((:)/2))
    ->	plast_prop(Left,arguments([Module,Head]))	
    ;	Module=[],
	   	Head=Left
    ).
do_toplevel_term(Id,Type,Module,Head,Body):-
    plast_prop(Id,functor((:)/2)),
    plast_prop(Id,arguments([Module,TL])),
    do_toplevel_term(TL,Type,[],Head,Body).
do_toplevel_term(Id,fact,Module,Head,[]):-
    (	plast_prop(Id,functor((:)/2))
    ->	plast_prop(Id,arguments([Module,Head]))	
    ;	Module=[],
    	Head=Id
    ).


	