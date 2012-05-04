:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).
logtalk_library_path(pdt_console_pl_lgt, Library) :-
	absolute_file_name(lib_pdt_console_pl('lgt/loader.pl'), FilePath),
	file_directory_name(FilePath,Directory),
	atom_concat(Directory, '/', Library).

load_lgt_adapter :-
    (current_predicate(user:logtalk_load/1)
    -> logtalk_load([
            library(types_loader),
            library(metapredicates_loader),
		    pdt_console_pl_lgt(utils4entities),
			pdt_console_pl_lgt(logtalk_adapter)
       ])
	;  true
	).
	
:- initialization( load_lgt_adapter ). 
