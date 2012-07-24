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

:-module(export_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_source_term')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).

:- pdt_annotator([term,file],[]).

file_annotation_hook([File|_],_,In,[defines_module(Module), exports(SortedExports)|In]):-
	pdt_file_term(File,H),
	!,
    source_term_expand(H,':-'(module(Module,_))),   
    collect_exports(H,Exports),
    sort(Exports,SortedExports).

term_annotation_hook(_,_,_,InTerm,OutTerm):-
    %% check if term is a module declaration
    source_term_expand(InTerm,Stripped),    
    Stripped==':-'(module(_Module,_Exports)),
	%% check the exports list 
	check_position(InTerm,TmpTerm),
	check_exports(TmpTerm,OutTerm).

check_position(InTerm,OutTerm):-
    %pdt_aterm_term_annotation(InTerm,Term,Annos),
	%   pdt_member(n(N),Annos),
  	source_term_property(InTerm,n,N),

    (	N>1
    ->	%pdt_aterm_term_annotation(OutTerm,Term,[problem(error(module_definition_not_at_start))|Annos])
    	source_term_set_property(InTerm,problem,error(module_definition_not_at_start),OutTerm)
    ;	OutTerm=InTerm
    ).
    

collect_exports(In,ExportsOut):-
    source_term_subterm(In,[1,2],Exports0),
    findall(Elm,source_term_member(Exports0,_,Elm),Exports1),
    collect_exports_x(Exports1,ExportsOut).

collect_exports_x([],[]).
collect_exports_x([ExportIn|ExportsIn],ExportsOut):-
    source_term_property(ExportIn,problem,error(_)),
%    pdt_aterm_term_annotation(ExportIn,_,Ann),
%    pdt_member(problem(error(_)),Ann), %skip malformed exports
    !,
    collect_exports_x(ExportsIn,ExportsOut).
collect_exports_x([ExportIn|ExportsIn],[ExportOut|ExportsOut]):-    
    source_term_expand(ExportIn,ExportOut),
    collect_exports_x(ExportsIn,ExportsOut).
    
check_exports(In,Out):-
	source_term_subterm(In,[1,2],Exports),
	%% find ill-formed elements in the exports list
	findall(
		ill_formed(Path,AExport),
		(	source_term_member(Exports,Path,AExport),
			source_term_expand(AExport,Export),
			\+ well_formed_export(Export)
		),
		IllFormedExports
	),
	add_ill_formed_annos(In,IllFormedExports,Out).

add_ill_formed_annos(In,[],In).
add_ill_formed_annos(In,[ill_formed(Path,InExport)|IFEs],Out):-
    pdt_aterm_term_annotation(InExport,Term,Annotation),
    pdt_aterm_term_annotation(OutExport,Term,[problem(error(malformed_export))|Annotation]),
    pdt_aterm_subst(In,[1,2|Path],OutExport,Next),
    add_ill_formed_annos(Next,IFEs,Out).
    
well_formed_export(Name/Arity):-
    atom(Name),
    integer(Arity).
    


