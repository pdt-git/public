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

:- module(pdt_rewrite,[]).

:- multifile pdt_rewrite_goal/2.

rt_expand(A,B):-
	do_goal_expansion(A,B).


do_goal_expansion(A,(rt_expand(A,C),C)):-
	functor(A,':',2),arg(1,A,M),
	var(M),!.
  
do_goal_expansion(A,(rt_expand(A,C),C)):-
	functor(A,F,_),var(F),!.


do_goal_expansion(A,B):-
    pdt_rewrite_goal(A,B),!.
do_goal_expansion(A,A).    




user:goal_expansion(A,B):-
	do_goal_expansion(A,B).



pdt_proving(Goal):-
    (	watching_goal(Goal)
    ->	true
    ;	watch_goal(Goal)
    ),
    prove_key(Key),
    recorded(Key,Goal).
    
    

/*
memo:
what to do to avoid messing up lco/tro ?

simple. If the expanded goal is the last subgoal within the parent goal, we push as usual
before descending, but we do not insert a pop after ascending. (this would keep the original 
last call from beeing one, hence byebye tro - which is  bad, bad, bad)
instead, we mark the expanded goal as a last call. 

To eventualy clean the stack on ascending, any non-last-call goal has the obligation to pop any remaining last-call goals from the 
stack before popping itself. 
My, my, i am so clever :-) Only problem: how do i know if the expanded goal is a last call?
sure, if i see the whole clause body it's simple. i could use term_expansion/2...

a term is called
 - if it is arg to :-/1
 - if it is the second arg of :-/2
 - if it is a goal arg to any known meta call predicate (including ,/2 and ;/2)
 - if it is bound to a variable that satisfies the above conditions.

*/