/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:-module(singleton_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_cs')).


:- pdt_annotator([term],[library('/org/cs3/pdt/annotate/variable_name_annotator')]).


term_annotation_hook(_,_,_,InTerm,OutTerm):-
	pdt_aterm_term_annotation(InTerm,_,Annos),
    pdt_member(singletons(Singletons),Annos),
    pdt_member(variable_names(Variables),Annos),

	%% check the exports list 
	check_singletons(InTerm,Singletons,TmpTerm),
	check_no_singletons(TmpTerm,Variables,OutTerm).

    
check_singletons(In,Singletons,Out):-
	pdt_cs(CS),
	pdt_cs_subterm(CS,T),
	pdt_cs_carrier(CS,Singletons),
	pdt_cs_condition(CS,
		(	pdt_aterm_term_annotation(T,Var,Annos),
			var(Var),
			member(Name=XVar,Singletons),
			Var==XVar,
			\+atom_concat('_',_,Name),
			pdt_aterm_term_annotation(TT,Var,[problem(warning(singleton(Name)))|Annos])
		)
	),
	pdt_cs_substitution(CS,TT),
	pdt_cs_apply(In,CS,Out).




check_no_singletons(In,[],In).
check_no_singletons(In,[Variable|Variables],Out):-
	check_no_singleton(In,Variable,Tmp),
	check_no_singletons(Tmp,Variables,Out).

check_no_singleton(In,Name=Variable,Out):-
    atom_concat('_',_,Name),
    pdt_count((pdt_aterm_subterm(In,_,ST),pdt_aterm_term_annotation(ST,Term,_),Term==Variable),Count),
    Count > 1, 
    !,
    
    pdt_cs(CS),
	pdt_cs_subterm(CS,T),
	pdt_cs_carrier(CS,Variable),
	pdt_cs_condition(CS,
		(	pdt_aterm_term_annotation(T,Var,Annos),
			var(Var),			
			Var==Variable,
			pdt_aterm_term_annotation(TT,Var,[problem(warning(no_singleton))|Annos])
		)
	),
	pdt_cs_substitution(CS,TT),
	pdt_cs_apply(In,CS,Out).
check_no_singleton(In,_,In).


    


