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

/* The module pdtplugin provides helper predicates for the PDT Eclipse Plugin */

:- module(pdtplugin,[]). 

:- reexport(split_file_path).
:- reexport(pdt_search).                % find definitions, declarations (and autocompletion)
:- reexport(pdt_editor_reload).         % pdt_reload/1 & friends
:- reexport(pdt_editor_manual_entry).   % for quick outline (find_pred/5)
%:-reexport(pdt_editor_highlighting).   % no noeed to include here, used (directly) only in PLScanner.java



