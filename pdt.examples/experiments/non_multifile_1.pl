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

:- module(a1,[aaa/1]).

aaa(1).                  % This definition is found by 
                         %   visible_in_module(Module, Name, Arity) and
                         %   visible_in_module(Module, Name, Arity, File, Line)
                         % in addition to the independent definition in module a2.

:- multifile bbb/1.

bbb(1).


