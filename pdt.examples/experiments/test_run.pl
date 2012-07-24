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

:- module( find_reference_test, [go/2] ).

:- use_module(swixreftest_sub).
% Includes ... and ..._super

go(Ref,X) :- swixreftest_sub:invoke_sub(Ref,X).
go(Ref,X) :- swixreftest_sub:invoke_super(Ref,X).
go(Ref,X) :- swixreftest:invoke(Ref,X).


