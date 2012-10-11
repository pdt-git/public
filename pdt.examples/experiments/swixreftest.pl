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

:- module(swixreftest,[p1/1,p2/1,p3/1,invoke_super/2]).

:- use_module(swixreftest_super).

%:-multifile swixreftest_super:invoke_super/2.
%
%invoke_super(overriding,X).


invoke(1,X) :- call(p2(X)).
invoke(2,X) :- user_defined_meta1(p3(X)).
invoke(3,X) :- user_defined_meta2(p3,[X]).
invoke(4,X) :- user_defined_meta3(p3,[X]).

user_defined_meta1(G) :-                       call(G).
user_defined_meta2(F,Args) :- G =..  [F|Args], call(G).
user_defined_meta3(F,Args) :- alias(F,F1),
	                          G =.. [F1|Args], call(G).

alias(X,X).

p1(p1). % not called
p2(p2). % called via instantiated metacall
p3(p3). % apparently not called but actually there is
        %  - a variable metacall in this module
        %  - an instantiated metacall in a submodule
        %  - an instantiated metacall in a supermodule

blub :- meta_predicate(bla).


