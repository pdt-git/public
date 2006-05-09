
:- prolog_load_context(directory,A), user:assert(file_search_path(library,A)).
:- use_module(library(single_char_interceptor)).

full_name:-
	arch_lib_name(Name),writeln(Name).
	
base_name:-
	sci_setting(cio_base_name,Name),writeln(Name).	