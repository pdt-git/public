
:- module( pdt_editor_manual_entry,
         [ predicate_manual_entry/4    % (_Module,Pred,Arity,Content)
         , manual_entry/3              % Preferably use predicate_manual_entry/4 
           % Duplicates most of the above and a predicate from 
           % pdt.runtime.ui/library/pdt/facade/pdt_content_assistant.pl
           % Called only from 
           % pdt.core/src/org/cs3/pl/metadata/internal/classic/DefaultMetainfoProvider.java
         ]).
         

:- use_module(library(pldoc/doc_library)).
:- use_module(library(explain)).
:- use_module(library(help)).
:- use_module(library(make)).
:- use_module(library('pldoc')).
:- use_module(library('pldoc/doc_html')).
:- use_module(library('http/html_write')).

:- use_module(pdt_prolog_library(utils4modules)).


               /****************************************
                * GET THE MANUAL ENTRY FOR A PREDICATE *
                ****************************************/

%% predicate_manual_entry(+Module, +Pred,+Arity,-Content) is det.
%
%
predicate_manual_entry(_Module,Pred,Arity,Content) :-
    help_index:predicate(Pred,Arity,_,FromLine,ToLine),
    !,
    online_help:line_start(FromLine, From),
    online_help:line_start(ToLine, To),
    online_help:online_manual_stream(Manual),
% TODO: Isn't what comes after this comment just a complicated way to say
%   with_output_to(atom(Content), (     
%      seek(Manual,From,bof,_NewOffset),
%      Range is To - From,
%      online_help:copy_chars(Range, Manual, MemStream)
%    )).
% TODO: Check, replace, test.
    new_memory_file(Handle),
    open_memory_file(Handle, write, MemStream),
    seek(Manual,From,bof,_NewOffset),
    Range is To - From,
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle), 
    !.


predicate_manual_entry(Module, Pred,Arity,Content) :-
    %pldoc:doc_comment(Module:Pred/Arity,_File:_,,Content),
    %TODO: The html code is now available:
	pldoc:doc_comment(Module:Pred/Arity,File:_,_Summary,_Content),
	gen_html_for_pred_(File,Pred/Arity,Content),
    !.
	
predicate_manual_entry(_Module, Pred,Arity,Content) :-
	catch(ast_node_signature_doc(_Language, Pred, Arity, Doc), _, fail),
	sformat(Content,'~w',[Doc]),
	!.
	
predicate_manual_entry(_Module,_Pred,_Arity,'nodoc').

gen_html_for_pred_(FileSpec,Functor/Arity,Html) :-    
	doc_file_objects(FileSpec, _File, Objects, FileOptions, []),
	member(doc(Signature,FilePos,Doc),Objects),
	(Functor/Arity=Signature;_Module:Functor/Arity=Signature),
	phrase(html([ 
	     		\objects([doc(Functor/Arity,FilePos,Doc)], FileOptions)
	     ]),List),
	maplist(replace_nl_,List,AtomList),
	concat_atom(AtomList,Html), 
	!.


replace_nl_(nl(_),''):-!. 
replace_nl_(A,A).

/*  apparently dead code:
write_ranges_to_file(Ranges, Outfile) :-
    online_manual_stream(Manual),
    help_tmp_file(Outfile),
    open(Outfile, write, Output),
    show_ranges(Ranges, Manual, Output),
    close(Manual),
    close(Output).
*/

%% manual_entry(Pred,Arity,Content) is det.
%
% TODO: Remove duplicate code by using predicate_manual_entry.
% Only difference: Use of stream_position versus use of seek.
%
manual_entry(Pred,Arity,Content) :-
    predicate(Pred,Arity,_,From,To),
    !,
    online_help:online_manual_stream(Manual),
    new_memory_file(Handle),
    open_memory_file(Handle, write, MemStream),
    stream_position(Manual, _, '$stream_position'(From, 0, 0)),
    Range is To - From,
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle).
/*
manual_entry(Pred,Arity,Content) :-
    meta_data_help(_,Pred,Arity,ContentString),
    string_to_atom(ContentString,Content).

manual_entry(Pred,-1,Content) :-
    meta_data_module(_,Pred,ContentString),
    string_to_atom(ContentString,Content).
*/



