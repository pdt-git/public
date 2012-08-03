/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- object(logtalk_reload_adapter).

:- if(current_logtalk_flag(version, version(3, _, _))).
	:- multifile(logtalk::message_hook/3).
	:- dynamic(logtalk::message_hook/3).

	logtalk::message_hook(Term, _Kind, _Tokens) :-
%		nonvar(Term),
%		arg(1, Term, Path),
%		is_absolute_file_path(Path),
%		arg(2, Term, Lines),
%		(	integer(Lines)
%			% if integer(Lines), Lines =< 0 -> no line number available
%		;	Lines = Begin - End,
%		 	integer(Begin),
%			integer(End)
%		),
		fail.
:- endif.

               /*************************************
                * PDT RELOAD                       *
                *************************************/

:- public(pdt_reload/1).
%% pdt_reload(File) is det.
%
% wrapper for consult. Only used to ignore PLEditor triggered consults in the history.

% Logtalk
pdt_reload(FullPath) :-
	write(FullPath), nl,
	split_file_path:split_file_path(FullPath, Directory, File, BaseName, lgt),
	setup_call_cleanup(
		working_directory(Current, Directory),     % SWI-Prolog
		logtalk_reload(Directory, File, BaseName),
		working_directory(_, Current)              % SWI-Prolog
   ).

:- private(logtalk_reload/3).
logtalk_reload(Directory, File, BaseName) :-
	(	logtalk::loaded_file(File, Directory, Options) ->
		% we're reloading a source file; use the same explicit compilation options as before
		logtalk_load(BaseName, Options)
	;	% first time; assume only implicit compilation options
		logtalk_load(BaseName)
	).


:- end_object.