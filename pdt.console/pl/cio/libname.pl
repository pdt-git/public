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


:- prolog_load_context(directory,A), user:assertz(file_search_path(library,A)).
:- use_module(lib_pdt_console_pl('cio/single_char_interceptor.pl')).

full_name:-
	arch_lib_name(Name),writeln(Name).
	
base_name:-
	sci_setting(cio_base_name,Name),writeln(Name).	


