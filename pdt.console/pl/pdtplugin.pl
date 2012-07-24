/* $LICENSE_MSG$ */

/* The module pdtplugin provides helper predicates for the PDT Eclipse Plugin */

:- module(pdtplugin,[]). 

:- reexport(split_file_path).
:- reexport(pdt_search).                % find definitions, declarations (and autocompletion)
:- reexport(pdt_editor_reload).         % pdt_reload/1 & friends
:- reexport(pdt_editor_manual_entry).   % for quick outline (find_pred/5)
%:-reexport(pdt_editor_highlighting).   % no noeed to include here, used (directly) only in PLScanner.java


