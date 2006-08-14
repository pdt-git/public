:- use_module(library('org/cs3/pdt/model/pdt_index')).
:- use_module(library('org/cs3/pdt/model/pdt_handle')).
:- use_module(library('org/cs3/pdt/util/pdt_util_comments')).
find_doc(Name,File,Pos,String):-
    % fetch comment raw data for some named predicate
	find_pred(Name,H),
    pdt_property(H,comments,Comments),
    member(Pos-String,Comments),   
    
    % fetch name of the file defining the predicate
    pdt_property(H,file,File).

find_pred(Name,H):-    
	pdt_index_load(predicate_definitions,IX),
    pdt_index_get(IX,Name,H).

find_pred(Name/Arity,H):-    
	pdt_index_load(predicate_definitions,IX),
    pdt_index_get(IX,Name,H),
    pdt_property(H,arity,Arity).


name_dom(Name,Dom):-
	find_doc(Name,File,Pos,String),
    pdt_comment_dom(File,Pos,String,Dom).

doc_test(Name):-
    find_doc(Name,File,Pos,String),
    pdt_comment_dom(File,Pos,String,Dom),
    pdt_comment_summary(File,Pos,String,Modes,Summary),
    writeln(modes(Modes)),
    writeln(summary(Summary)),
	pdt_dom_html(File,Dom,Html),
	writeln(Html).

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

