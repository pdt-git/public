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

:-module(c_plast,[
	c_plast_node/1,
	c_plast_prop/2,
	c_variable_symbol/2
]).

:- use_module(plast).
:- use_module(plast_utils).

c_plast_node(c_node(A,_,_)):-
    plast_node(A).

c_plast_prop(c_node(A,Links,Path),Prop):-
    (	variable_symbol(A,Sym),
        link(Sym,Links,Path,Value)
   	->	c_plast_prop(c_node(Value,Links,[Sym|Path]),Prop)
   	;	plast_prop(A,PProp),
	   	wrap_prop(A,Links,Path,PProp,Prop)
   	).

c_variable_symbol(c_node(Var,_,Path),var_sym(Sym,Path)):-
	variable_symbol(Var,Sym).
   	
link(Sym,Links,Path,Value):-
    memberchk([Sym|Path]=Value,Links).


    
wrap_prop(A,Links,Path,parent(P),parent(PW)):-
    (	Path=[Var|VPath],
    	link(Sym,Links,VPath,A),
    	variable_symbol(Var,Sym)
    ->	c_plast_prop(c_node(Var,Links,VPath),PW)
    ;	PW=c_node(P,Links,[Var|Path])
    ).
    
wrap_prop(_,Links,Path,child(C),child(c_node(C,Links,Path))).    
wrap_prop(_,Links,Path,argument(C),argument(c_node(C,Links,Path))).
wrap_prop(_,Links,Path,tail(C),tail(c_node(C,Links,Path))).
wrap_prop(_,Links,Path,member(C),member(c_node(C,Links,Path))).
wrap_prop(_,Links,Path,elements(C),elements(WC)):-
    wrap_elements(Links,Path,C,WC).
wrap_prop(_,Links,Path,arguments(C),arguments(WC)):-
    wrap_elements(Links,Path,C,WC).

wrap_elements(_,_,[],[]).
wrap_elements(Links,Path,[Node|Tail],[c_node(Node,Links,Path)|WTail]):-
    wrap_elements(Links,Path,Tail,WTail).