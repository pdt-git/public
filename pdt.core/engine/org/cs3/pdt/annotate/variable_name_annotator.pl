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

:-module(variable_name_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('org/cs3/pdt/util/pdt_util_cs')).


:- pdt_annotator([term],[]).


term_annotation_hook(_,_,_,InTerm,OutTerm):-
	pdt_aterm_term_annotation(InTerm,_,Annos),
    pdt_member(variable_names(VariableNames),Annos),
    pdt_member(variable_ids(VariableIds),Annos),    
	propagate_variable_names(InTerm,VariableNames,NextTerm),
	propagate_variable_ids(NextTerm,VariableIds,OutTerm).

    

	

propagate_variable_names(In,VariableNames,Out):-
	pdt_cs(CS),
	pdt_cs_subterm(CS,T),
	pdt_cs_carrier(CS,VariableNames),
	pdt_cs_condition(CS,
		(	pdt_aterm_term_annotation(T,Var,Annos),
			var(Var),
			member(Name=XVar,VariableNames),
			Var==XVar,
			pdt_aterm_term_annotation(TT,Var,[variable_name(Name)|Annos])
		)
	),
	pdt_cs_substitution(CS,TT),
	pdt_cs_apply(In,CS,Out).

propagate_variable_ids(In,VariableIds,Out):-
	pdt_cs(CS),
	pdt_cs_subterm(CS,T),
	pdt_cs_carrier(CS,VariableIds),
	pdt_cs_condition(CS,
		(	pdt_aterm_term_annotation(T,Var,Annos),
			var(Var),
			member(XVar=Id,VariableIds),
			Var==XVar,
			pdt_aterm_term_annotation(TT,Var,[variable_id(Id)|Annos])
		)
	),
	pdt_cs_substitution(CS,TT),
	pdt_cs_apply(In,CS,Out).




