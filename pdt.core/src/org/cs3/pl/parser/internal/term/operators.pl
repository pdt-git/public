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

infix_precs(Op,Prec,Prec-1,Prec-1):-
    current_op(Prec,xfx,Op).
infix_precs(Op,Prec,Prec,Prec-1):-
    current_op(Prec,yfx,Op).
infix_precs(Op,Prec,Prec-1,Prec):-
    current_op(Prec,xfy,Op).
infix_precs(Op,Prec,Prec,Prec):-
    current_op(Prec,yfy,Op).
    
prefix_precs(Op,Prec,Prec):-
    current_op(Prec,fy,Op).
prefix_precs(Op,Prec,Prec-1):-
    current_op(Prec,fx,Op).    

gen_infix_maps:-
    forall(
      infix_precs(Op,Prec,Lh,Rh),
      (
      	escape(Op,Esc),
	    format("		infix_map.put(\"~w\",new int[]{~w,~w,~w});~n",[Esc,Prec,Lh,Rh])
	   )
    ).

gen_prefix_maps:-
    forall(
      prefix_precs(Op,Prec,Rh),
      (
      escape(Op,Esc),
      format("		prefix_map.put(\"~w\",new int[]{~w,~w});~n",[Esc,Prec,Rh])
      )
    ).
    
gen_prefix_tks:-
	forall(
      prefix_precs(Op,_,_),
      (
      escape(Op,Esc),
      format("		|\"~w\"~n",[Esc])
      )
    ).

gen_infix_tks:-
	forall(
      infix_precs(Op,_,_,_),
      (
      escape(Op,Esc),
      format("		|\"~w\"~n",[Esc])
      )
    ).

gen_op_tks:-
	forall(
      current_op(_,_,Op),
      (
      escape(Op,Esc),
      format("		|\"~w\"~n",[Esc])
      )
    ).    
escape(In,Out):-
  atom_chars(In,InList),
  escape_l(InList,OutList),
  atom_chars(Out,OutList).

escape_l([],[]).
escape_l( ['\\'|ITail], ['\\','\\'|OTail]):-
    !,
  escape_l(ITail,OTail).
escape_l([A|ITail],[A|OTail]):-
    escape_l(ITail,OTail).
gen_maps:-
	gen_infix_maps,
	gen_prefix_maps.    
	
allops:-
  forall(infix_precs(Op,_,_,_);prefix_precs(Op,_,_),(write_canonical(Op),nl)).