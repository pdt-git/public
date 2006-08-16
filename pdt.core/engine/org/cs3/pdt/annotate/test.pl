file_search_path(library,'v:/workspace/pdt.core/engine').
file_search_path(library,'v:/workspace/pdt.runtime/library/common').
file_search_path(library,'v:/workspace/pdt.runtime/library/swipl').
file_search_path(library,'v:/workspace/pdt.runtime/library/pif').


:- use_module(pdt_annotator).
:- use_module(export_annotator).
:- use_module(fileref_annotator).
:- use_module(indexer).
:- use_module(member_annotator).
:- use_module(op_annotator).
:- use_module(singleton_annotator).
:- use_module(undefined_exports_annotator).
:- use_module(variable_name_annotator).



