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

:- module(meta_program,[]).


:- use_module(plast).
:- use_module(plast_util).
:- use_module(u_term).


% mp_unify(+Sin,+LUTerm,+RUTerm,-Sout)

mp_unify(Sin,LUTerm,RUTerm,Sout):-
    lr_u_term(l,_,_,LVars,_,_,LUTerm),
    lr_u_term(r,_,_,RVars,_,_,RUTerm),
	merge(LVars,RVars,Variables),
	LUTerm=RUTerm,
	merge_bound(Sin,Variables,Sout).

merge_bound(Sin,[],Sin).
merge_bound(Sin,[Sym=Val|Tail],Sout):-
    (	nonvar(Val)
    ->	merge(Sin, [Sym=Val],Sout)
    ;	Sout=Sin
    ),
    merge_bound(Sin,Tail,Sout).

    	
mp_subst(FrameId,Bindings,Node,ROutTerm):-
	u_term(r, t_term(FrameId,Node,Bindings),ROutTerm).
	
	

new_frame(FrameId):-
	gensym(fid,FrameId).	


meta_clause(Id,(mp_directive(Id):-MetaBody)):-
	toplevel_term(Id,directive,_,[],Body),
	unfold(Id,[],Body,_,MetaBody).
	
meta_clause(Id,(
	
	mp_rule(Module,LTerm_InHead,RTerm_OutHead,Id):-
    	new_frame(FrameId),
	   	mp_subst(FrameId,[],HeadNode,RTerm_HeadNode),
		mp_unify([],LTerm_InHead,RTerm_HeadNode,S0),
		mp_subst(FrameId,S0,HeadNode,RTerm_OutHead))
	)
	
	:-
	toplevel_term(Id,fact,Module,HeadNode,[]),

	plast_prop(Module,term(Module)).

meta_clause(Id,(
	
	mp_rule(Module,LTerm_InHead,RTerm_OutHead,Id):-
    	new_frame(FrameId),
	   	mp_subst(FrameId,[],HeadNode,RTerm_HeadNode),
		mp_unify([],LTerm_InHead,RTerm_HeadNode,S0),
		Body,
		mp_subst(FrameId,Sout,HeadNode,RTerm_OutHead))
	)
		
	:-
	toplevel_term(Id,rule,ModuleNode,HeadNode,BodyNode),
	plast_prop(ModuleNode,term(Module)),
	unfold(FrameId,Id,S0,BodyNode,Sout,Body).

unfold(FrameId,ClauseId,S0,Node,Sout,
(
	GoalA,GoalB
)):-
    plast_prop(Node,functor((,)/2)),
    plast_prop(Node,arguments([A,B])),
    unfold(FrameId,ClauseId,S0,A,S1,GoalA),
    unfold(FrameId,ClauseId,S1,B,Sout,GoalB).
    
unfold(FrameId,ClauseId,S0,Node,Sout,
(
	GoalA;GoalB
)):-
    plast_prop(Node,functor((;)/2)),
    plast_prop(Node,arguments([A,B])),
    unfold(FrameId,ClauseId,S0,A,S1,GoalA),
    unfold(FrameId,ClauseId,S1,B,Sout,GoalB).
	