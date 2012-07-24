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

:- module(source_files, [pdt_source_files/1, pdt_source_file/2]).


%% pdt_source_files(String) is nondet.
%
% TRHO: obsolete once improved parser is available (PDT-412)
pdt_source_files(String) :-
	findall(File,
		source_file(File),
		Files),
	ctc_lists:list_2_comma_separated_list(Files, String).

pdt_source_file(File, State) :-
	source_file(File),
	(	exists_file(File)
	->	source_file_property(File, 	modified(ModifiedAtConsult)),
		time_file(File, ModifiedNow),
		(	ModifiedNow > ModifiedAtConsult
		->	State = old
		;	State = current
		)
	;	State = current
	).


