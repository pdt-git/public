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

extension_point_location_fact.

add_extension_point_file_search_paths :-
    clause(extension_point_location_fact,_,Ref),
    clause_property(Ref,file(File)),
	file_directory_name(File,Dir),
	assert(file_search_path(org_cs3_lp_extension,Dir)).
	
:- add_extension_point_file_search_paths.
		
:- ['extension_point'].


