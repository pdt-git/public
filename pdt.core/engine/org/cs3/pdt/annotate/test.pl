:- ensure_loaded(library(test_wiki)).
find_doc(Name,File,DOM):-
    % fetch comment raw data for some named predicate
	pdt_index_load(predicate_definitions,IX),
    pdt_index_get(IX,Name,H),
    pdt_property(H,comments,Comments),
    member(Pos-String,Comments),   
    
    % fetch name of the file defining the predicate
    pdt_property(H,file,File),
    
    % create DOM
    process_comment(File, Pos-String, DOM).
    
dom2html(File,Dom,HTML):-
    new_memory_file(MF),
    open_memory_file(MF,write,Out),
    doc_write_html(Out, File, Dom),
    close(Out),
    memory_file_to_atom(MF,HTML).    

doc_test(Name,HTML):-
    find_doc(Name,File,Dom),
    dom2html(File,Dom,HTML).

%% aja(+A,-B) 
% 
%
% aja macht beim Testen Spass.
%
%
% * MyItem 
%   * My SubItem
%
% * Item 1
% * sub item 1
%	* sub item 2
%		* Item 2
%			1. ordered item 1
%			2. ordered item 2
% @param A wichtiges Argument
% @param B fürchterlich wichtiges Argument
%
aja(_A,_B).

