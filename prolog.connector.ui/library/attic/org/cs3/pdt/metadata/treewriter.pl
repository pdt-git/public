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

:- module(treewriter,[write_tree/2,write_tree/1]).
:- use_module(plast).
:- use_module(plast_utils).

write_tree(Node):-
    write_tree(current_output,Node).
    
write_tree(Stream,compilation_unit_node(I)):-
	forall(
		plast_prop(compilation_unit_node(I),child(J)),    
		(	write_tree(Stream,J),
			put(Stream,'.'),
			nl(Stream)
		)
	).
	
write_tree(Stream,atom_node(I)):-
	plast_prop(atom_node(I),term(T)),
	write(Stream,T).

write_tree(Stream,variable_node(I)):-
	plast_prop(variable_node(I),name(T)),
	write(Stream,T).
	
	
write_tree(Stream,string_node(I)):-
	plast_prop(string_node(I),term(T)),
	write(Stream,T).	
	
write_tree(Stream,compound_node(I)):-
	plast_prop(compound_node(I),functor(N/_)),	
	plast_prop(compound_node(I),arguments(Args)),
	write(Stream,N),
	put(Stream,'('),
	write_sequence(Stream,Args),
	put(Stream,')').
	
write_tree(Stream,list_node(I)):-
    put(Stream,'['),
	(	plast_prop(list_node(I),elements(Elms))
	->	write_sequence(Stream,Elms),
		(	plast_prop(list_node(I),tail(Tail))
		->	put(Stream,'|'),
			write_tree(Stream,Tail)
		;	true
		)
	;	true
	),
	put(Stream,']').
	
write_tree(Stream,brace_node(I)):-
	put(Stream,'{'),
	plast_prop(brace_node(I),argument(Arg)),
	write_tree(Stream,Arg),
	put(Stream,'}').
	
write_sequence(_,[]).
write_sequence(Stream,[Last|[]]):-
	write_tree(Stream,Last).
write_sequence(Stream,[Head|Tail]):-
	write_tree(Stream,Head),
	put(Stream,','),
	write_sequence(Stream,Tail).		