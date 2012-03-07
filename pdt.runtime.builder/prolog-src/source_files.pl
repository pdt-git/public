:- module(source_files, [pdt_source_files/1]).


%% pdt_source_files(String) is nondet.
%
% TRHO: obsolete once improved parser is available (PDT-412)
pdt_source_files(String) :-
	findall(File,
		source_file(File),
		Files),
	ctc_lists:list_2_comma_separated_list(Files, String).

	