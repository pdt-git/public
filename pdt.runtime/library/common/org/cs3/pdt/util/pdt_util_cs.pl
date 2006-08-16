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






%% Conditional Substitutions
% 
% This is an experiment. 
% 
% I wanted to have something like CTs, but on the basis of 
% Subterm substitutions. A conditional consists of
% - a Subterm pattern,
% - a Condition,
% - a Substitution template.
% - a Carrier term.
% 
% When applied to a term, the term is traversed in a top-down manner. For each
% encountered subterm, we try to unify this subterm with the subterm pattern. On success,
% the condition goal is evaluated. If it succeeds aswell, the subterm is substituted by the substitution 
% template.
% The idea is of course to exploit unification to transport information from the subterm pattern through the 
% condition into the substitution termplate.
%
% Problems: 
% - The decission to use top-down traversal was a technical one, but in theory, a bottom up traversal could also be employed. 
%   For some CS, the result depends on order 
%   of traversal. This is not very nice.
% - A CS may trigger itself on subterms, leading to infinit recursion. 
%   E.g. consider 
%		- cs(a(_),true,a(a(_)))
% 		- cs(T,true,a(T))
%	Sure, this is a pretty stupid example, but there are probably more serious ones.
%   Not very nice either.
%
% It may help to find an equivalence transformation to CTs, because there we already have 
% dependency analysis. Intuition suggests that something like this should be possible.
%
% For now, we ignore this problems.
:- module(pdt_util_cs,[
	pdt_cs/1,
	pdt_cs_subterm/2,
	pdt_cs_condition/2,
	pdt_cs_substitution/2,	
	pdt_cs_carrier/2,
	pdt_cs_apply/3
]).

:- use_module(library('org/cs3/pdt/util/pdt_util_aterm')).
%% pdt_cs(?CS).
% unifies CS with a generic CS term.
pdt_cs(cs(_T,_V,_C,_S)).

%% pdt_cs_subterm(+CS, ?Subterm).
% access the Subterm part of a CS term.
pdt_cs_subterm(cs(T,_V,_C,_S),T).

%% pdt_cs_condition(+CS, ?Condition).
% access the Condition part of a CS term.
pdt_cs_condition(cs(_T,_V,C,_S),C).


%% pdt_cs_substitution(+CS, ?Substitution).
% access the Substitution part of a CS term.
pdt_cs_substitution(cs(_T,_V,_C,S),S).


%% pdt_cs_substitution(+CS, ?Substitution).
% access the carrier part of a CS term.
%
% Before a cs is matched and possibly applied on a subterm, it is copied, i.e. fresh variables are used
% for each subterm match.
% In most cases this is exactly the desired behaviour, but there are some exceptions, for instance when
% the condition needs to refer to explicit variables apearing somewhere in the term, or in some other part of
% the program..
%
% So after copying the CS but before applying it, the carrier of the copy is unified with that of the
% original, and thus any variables it hold are sharing with the ones in the original.
pdt_cs_carrier(cs(_T,V,_C,_S),V).

%% pdt_cs_apply(+TermIn,+CS,-TermOut)
% apply a conditional substitution to a term.
%
% Works with both, annotated and plain terms.
%
%
pdt_cs_apply(TermIn,CS,TermOut):-
    pdt_cs(CS),
	tt(TermIn,CS,TermOut).

tt_args(T,_CS,T):-
    var(T),
    !.
tt_args(T,CS,Out):-
    pdt_aterm(T),
    !,
    tt_args_aterm(T,CS,Out).
tt_args(T,CS,Out):-
    tt_args_plain(T,CS,Out).


    
tt_args_plain(T,CS,Out):-
    functor(T,Name,Arity),
    functor(Out,Name,Arity),
    tt_args(T,1,Arity,CS,Out).
    
tt_args_aterm(T,CS,Out):-    
    pdt_term_annotation(T,ST,Annos),
    (	var(ST)
    ->	Out=T
    ;   pdt_functor(T,Name,Arity),
    	pdt_aterm(Out),
	    pdt_functor(Out,Name,Arity),
    	pdt_term_annotation(Out,_,Annos),
	    tt_args(T,1,Arity,CS,Out)
	).
    
    
tt_args(T,_,0,_,T):-    
    !.
tt_args(T,Arity,Arity,CS,Out):-
	tt_arg(T,Arity,CS,Out),
    !.
tt_args(T,N,Arity,CS,Out):-    
	tt_arg(T,N,CS,Out),
    M is N+1,
    tt_args(T,M,Arity,CS,Out).

tt_arg(T,N,CS,Out):-
	pdt_arg(N,T,Subterm),
	pdt_arg(N,Out,OutSubterm),
    tt(Subterm,CS,OutSubterm).



tt(T,CS,Out):-
	tt_apply_cs(T,CS,Temp),
	!,
	tt_args(Temp,CS,Out). 

tt(T,CS,Out):-
	tt_args(T,CS,Out). 

tt_apply_cs(T,CS,Out):-
	copy_term(CS,CSCopy),
	pdt_cs_carrier(CS,Vars),
	pdt_cs_carrier(CSCopy,Vars),
	pdt_cs_subterm(CSCopy,T),
	pdt_cs_condition(CSCopy,Condition),
	call(Condition),
	pdt_cs_substitution(CSCopy,Out).
	
tt_test_file(InFile,CS,OutFile):-
	open(InFile,read,In),
	open(OutFile,write,Out),
	repeat,
		read_term(In,Term,[]),
		tt(Term,CS,ModTerm),
		portray_clause(Out,ModTerm),
		nl(Out),
		Term==end_of_file,
	!,
	close(In),
	close(Out).
	
	
%tt(x(1),cs(x(N),(nonvar(N),M is N+1),x(M)),Out).	