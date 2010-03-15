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

:- module(meta_interpreter,[
	mi_call/1,
	mi_call_undefined/1,
	mi_call_variable/1,
	mi_call_undefined_built_in/1
]).
:- use_module(mi_meta_ops).
:- use_module(mi_built_ins).


% sim_call(Head)
%
% called by the meta interpreter when a goal is encountered.
% Should succeed if InHead is provable.
% on Success, should unify OutHead with a *COPY* of InHead that represents the instantiation 
% state of InHead after successfully proving it.
% Should *NOT* alter the instantiation state of InHead.
mi_call(Head):-
    mi_apply_subst(Head,SubstHead),
    (	var(SubstHead)
    ->	mi_call_variable(Head)
    ;	%format("subgraph \"cluster~w\" {~n label=\"~w\"~n",[SubstHead,SubstHead]),
    	writeln(enter(Head)),
    	(   do_mi_call(Head),
    		writeln(proven(Head))
	    	%format("} ~n",[])
	    ;	writeln(failed(Head)),
	    	%format("} ~n",[]),
	    	fail
    	)
    ).
    

    

do_mi_call(Head):-
    predicate_property(Head,built_in),!,
    mi_call_built_in(Head).
do_mi_call(Head):-
    functor(Head,Fun,Ar),
    (	current_predicate(Fun/Ar)
    ->	do_mi_clause(Head)
    ;	mi_call_undefined(Head)        
    ).
    
do_mi_clause(Head):-
    copy_term(Head,DupHead),
	clause(DupHead,Body),
	mi_unify(Head,DupHead),
	mi_apply_subst(Body,SubstBody),
	mi_call(SubstBody).

% mi_call_undefined(+Head)
% called if a Goal is called that cannot be bound to any
% predicate. 
mi_call_undefined(Head):-
%	format("subgraph \"cluster~w\" {label=\"undefined(~w)\"}~n",[Head,Head]).
	writeln(undefined(Head)).

% mi_call_undefined_built_in(+Head)
% called if a Goal is a builtin, but no semantic is defined
% in the meta interpreter
mi_call_undefined_built_in(Head):-
%	format("subgraph \"cluster~w\" {label=\"undefined_built_in(~w)\"}~n",[Head,Head]).
	writeln(undefined_built_in(Head)).

    
% mi_call_variable(+Head)
% called if a Goal is called but the called term is an
% unbound variable.
mi_call_variable(Head):-
%	format("subgraph \"cluster~w\" {label=\"variable(~w)\"}~n",[Head,Head]).
	writeln(variable(Head)).



some_goal(goal).
some_goal(no_goal).
    
notest(no_goal,Text):-
	writeln(Text).
notest(goal,Text):-
	Text.		
	
notest(A):-
    some_goal(IsGoal),
    notest(IsGoal,A).