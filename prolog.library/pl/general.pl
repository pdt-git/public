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

:- use_module(library(apply)).
:- use_module(library(charsio)).

/*
 * mgh(+Literal, ?MostGeneralLiteral)
 *
 * Arg2 is the most general form of the literal in Arg1
 */
mgh(Mod:Term, Mod:Term_mgh):-
    !,
    functor(Term, Functor, Arity),
    functor(Term_mgh, Functor, Arity).

mgh(Term, Term_mgh):-
    functor(Term, Functor, Arity),
    functor(Term_mgh, Functor, Arity).


/*
 * built_in(?Head)
 *
 * Check whether the specified term is the head of a built-in
 * predicate or enumerate all built-in predicates (if called 
 * with a free argument).
 */     
built_in(Head) :-
    predicate_property(Head, built_in).    
    
    
/*
 * repeat_n_times(+Goal,+N)
 */
repeat_n_times(Goal,N) :-             % initialize loop counter
  flag(repeat_counter,_,0),            
  repeat_n_times_loop(Goal,N).
  
repeat_n_times_loop(_,N) :-        % stop if counter = N
  flag(repeat_counter,N,N),
  !.
repeat_n_times_loop(Goal,N) :-        % loop
  once(Goal),                           % execute Goal once
  flag(repeat_counter,I,I+1),           % increment counter
  repeat_n_times_loop(Goal,N).          % repeat

   
:- module_transparent prolog_iteration_via_backtracking/1, all/1.

all(G) :- prolog_iteration_via_backtracking(G) .

prolog_iteration_via_backtracking(G) :- (call(G), fail) ; true .


/*
 * has_property(+Pred, ?Prop, ?HasProp) is det
 * 
 * Arg3 is 1 if the predicate referenced by Arg1 has the predicate of Arg2.
 * Else Arg3 is 0. 
 */
has_property(Pred,Prop,1) :- 
	predicate_property(Pred,Prop),
	!.
has_property(_Pred,_Prop,0).


%:- use_module(library(apply)).
%    
%get_var_names(Goal, _) :-
%    not(atomic(Goal)),
%    !,
%    throw('first argument has to be atomic').
%    
%get_var_names(Goal, VarNames) :-
%    format(atom(Query), '~w.', [Goal]),
%    open_chars_stream(Query,Stream),
%    read_term(Stream,_,[variable_names(VarNameList)]),
%    maplist(extract_var_name, VarNameList, ExtractedList),
%    list_2_comma_separated_list(ExtractedList,VarNames).
%    
%extract_var_name(=(VarName, _), VarName) :- !.
%extract_var_name(VarName, VarName) :- !.
    




