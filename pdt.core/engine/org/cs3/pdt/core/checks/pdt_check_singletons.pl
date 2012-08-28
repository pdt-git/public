/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(pdt_check_singletons,[pdt_check_singletons/2]).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_dfs')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_annotation')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).


%pdt_check_singletons(+File,-Occurance)
%succeeds if Occurance is an annotated subterm in File that is a singleton variable.
pdt_check_singletons(File,Occurance):-
    pdt_file_annotation(File,_,Terms),
    check_singletons(Terms,Occurance).


check_singletons_terms([Term|_],Occurance):-
    check_singletons_term(Term,Occurance).
check_singletons_terms([_|Terms],Occurance):-
	check_singletons_terms(Terms,Occurance).    
	
check_singletons_term(Term,Occurance):-
    pdt_aterm_term_annotation(Term,_,_),
    
	pdt_aterm_subterm(Term,_,Occurance),
	singleton(Occurance).
	
%singleton(Occurance)	:-
%    pdt_aterm(Occurance),
%    pdt_aterm_term_annotation(Occurance,_,Anno),
%    pdt_annotation_get(Anno,singletons,Singletons)


