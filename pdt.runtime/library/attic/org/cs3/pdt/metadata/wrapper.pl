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

:-module(wrapper,[wrap/2,unwrap/2]).

unwrap(f(A),A):-
	var(A),
	!.    
unwrap(f(A),B):-
	is_list(A),
	!,
	unwrap_elms(A,B).
unwrap(f({A}),{B}):-
    unwrap(A,B).
unwrap(f(Term),Out):-
    (	compound(Term)
    ->	Term=..[Name|Args],
    	unwrap_elms(Args,OutArgs),
	    Out=..[Name|OutArgs]
	 ;	Out=Term
	 ).	    

wrap(A,f(A)):-
	var(A),
	!.    
wrap(A,f(B)):-
	is_list(A),
	!,
	wrap_elms(A,B).
wrap({A},f({B})):-
    wrap(A,B).
wrap(Term,f(Out)):-
    (	compound(Term)
    ->	Term=..[Name|Args],
    	wrap_elms(Args,OutArgs),
	    Out=..[Name|OutArgs]
	 ;	Out=Term
	 ).	    

        
wrap_elms([],[]).
wrap_elms([H|T],[OH|OT]):-
    wrap(H,OH),
    wrap_elms(T,OT).

unwrap_elms([],[]).
unwrap_elms([H|T],[OH|OT]):-
    unwrap(H,OH),
    unwrap_elms(T,OT).