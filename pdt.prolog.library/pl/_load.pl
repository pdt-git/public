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



 :- consult(logging).       % <- THIS BE FIRST!
  
 :- consult(compatiblitySWI).
  
 :- consult(database).      % Assert, retract, ...
 :- consult(files).         % File handling
 :- consult(database_cache). 	    % Caching
 :- consult(contains). 	    % Contains for Strings

 :- consult(general).       % Various
 :- consult(listing).       % Print clauses of preds
 :- consult(lists).         % List handling
 :- consult(count).         % Counting
 :- consult(time).          % Runtime measurement
 :- consult(utils4modules). % Module handling

 :- consult(utils4modules_visibility). % Visibility handling
 
 :- use_module(junitadapter).
 
% :- consult(pdt_xref_experimental).     % find_references, ...


