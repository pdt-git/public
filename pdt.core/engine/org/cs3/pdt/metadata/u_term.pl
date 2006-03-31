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

:- module(u_term,[u_term/2,lr_u_term/7]).

:- use_module(plast).
:- use_module(plast_utils).

/*
 convert between various term representations.
 we have the following:
  * static terms, which are simply prolog ast nodes
  * temporal or conditional terms (better name wanted) 
    which are meant to represent terms constructed "dynamicaly" from
    static plast terms. They are written in the form
 	
 	   t_term(FrameId,HeadNode,[Var->t_term(...)|<more bindings>])
 	   
  * u_terms  are used for unification.
  	The idea here is to transform t_terms in a suitable form and 
  	let prolog do the nitty-gritty. The *u_terms look like this:
  	
  		lu_term(Term, t_term(...),[Symbol=Var|<more free vars>],_ ,_ )
  		ru_term(Term, _, _, t_term(...),[Symbol=Var|<more free vars>])
  		
	Unifying an lu_term with a ru_term, one only has to examing the
	var lists afterwards which yield exactly the unifying set of bindings.
	
	<braindump>We inspect the state of any prolog program as a function over time.
	This function maps the execution time domain of the program to 
	the power set of all possible variable bindings. This mapping we do not expect 
	to be injektiv, in particular, if we use an abstract variable domain.
	It does make sense to consider the equality classes induced that are induced on
	the time domain by this mapping. What i like to do (not sure yet, if it is possible)
	is collect all these classes for an adequat abstract domain. Somehow
	these classes should correspond to frames, but i'm not sure yet, how this
	relation should look like. </braindump>
	
	Time is a discret set of frames. (each frame is a point in time) 
	
	
	References to Variables and Terms always cary information about the frame
	the where introduced in. A new Frame is opened each time the meta programm
	enters a clause, and is closed when the clause is left (either success 
	or failure)
	
	The plast does not contain variables, it merily contains occurances of 
	variables. A variable symbol describes a variable as it exists at a given
	point of time (frame) in a given clause. Since bindings are a relation between
	variable SYMBOLS and t_terms, they do represent the state of the program as
	it is observable from within a given clause at that precise point in time.
	Since all references to variables happen from a well defined
	frame, variables that are local to this frame can 
  		
*/




lr_u_term(l,Term,TTerm,Vars,XTTerm,XVars,
	lu_term(Term,TTerm,Vars,XTTerm,XVars)).
lr_u_term(r,Term,TTerm,Vars,XTTerm,XVars,
	lu_term(Term,XTTerm,XVars,TTerm,Vars)).	
	
u_term(LR,t_term(FrameId,HeadNode,Bindings),Out):-
	lr_u_term(LR,Term,t_term(FrameId,HeadNode,Bindings),Vars,_,_,Out),
	plast_prop(HeadNode,type(compound)),
	plast_prop(HeadNode,functor(Fun/_)),
	plast_prop(HeadNode,arguments(Args)),
	lu_terms(LR,FrameId,Bindings,Args,UArgs,Vars),
	Term=..[Fun|UArgs].
u_term(LR,t_term(FrameId,HeadNode,Bindings),Out):-
	lr_u_term(LR,Term,t_term(FrameId,HeadNode,Bindings),Vars,_,_,Out),
	plast_prop(HeadNode,type(list)),
	plast_prop(HeadNode,elements(Elms)),
	u_terms(LR,FrameId,Bindings,Elms,UElms,UElmVars),
	(	plast_prop(HeadNode,tail(Tail))		
	->	u_term(LR,t_term(FrameId,Tail,Bindings),UTail,UTailVars),
		append(UElms,UTail,Term),
		merge_vars(UElmVars,UTailVars)
	;	Term=UElms,
		Vars=UElmVars
	).
u_term(LR,t_term(FrameId,HeadNode,Bindings),Out):-
	lr_u_term(LR,{UArg},t_term(FrameId,HeadNode,Bindings),Vars,_,_,Out),
	plast_prop(I,type(brace)),
	plast_prop(I,argument(Arg)),
	u_term(LR,t_term(FrameId,Arg,Bindings),UArg,Vars).
u_term(LR,t_term(FrameId,HeadNode,Bindings),Out):-
	lr_u_term(LR,Term,t_term(FrameId,HeadNode,Bindings),[],_,_,Out),
	plast_prop(I,type(atom)),
	plast_prop(I,term(Term)).
u_term(LR,t_term(FrameId,HeadNode,Bindings),TargetTerm):-
	plast_prop(HeadNode,type(variable)),		
	variable_symbol(HeadNode,FrameId,Sym),
	memberchk(Sym=Target,Bindings),
	u_term(LR,Target,TargetTerm).			
u_term(LR,t_term(FrameId,HeadNode,Bindings),Out):-
	lr_u_term(LR,_,t_term(FrameId,HeadNode,Bindings),[Sym=Value],Value,_,Out),
	plast_prop(HeadNode,type(variable)),	
	gen_symbol(FrameId,HeadNode,Sym),
	\+ memberchk(Sym=_,Bindings).
u_term(LR,t_term(FrameId,HeadNode,Bindings),Out):-
   	lr_u_term(LR,Term,t_term(FrameId,HeadNode,Bindings),[],_,_,Out),
	plast_prop(HeadNode,type(string)),
	plast_prop(HeadNode,term(Term)).

% u_terms(+LR,+FrameId,+Bindings,+Elms,-UElms,-UElmVars),
u_terms(_,_,[],[],[],[]).
u_terms(LR,FrameId,Bindings,[Elm|Elms],[UElm|UElms],UElmVars):-
	u_term(LR,t_term(FrameId,Elm,Bindings),UElm),
	lr_u_term(LR,_,_,V1,_,_,UElm),
	u_terms(LR,FrameId,Bindings,Elms,UElms,V2),
	merge_vars(V1,V2,UElmVars).	
	

	
	
merge_var(Vars,Symbol=Var,Vars):-
	memberchk(Symbol=Var,Vars).
merge_var(Vars,Symbol=Var,Merged):-
	merge(Vars,[Symbol=Var],Merged).

merge_vars(Vars,[],Vars).
merge_vars(Vars,[H|T],Merged):-
    merge_var(Vars,H,M),
    merge_vars(M,T,Merged).

		
		
	