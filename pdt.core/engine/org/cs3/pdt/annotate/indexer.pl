%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-module(clause_indexer,[
	pdt_update_index/1,
	pdt_clear_index/1
]).


%this module can be used as an annotator to updates the clause index according to the clauses defined (or not defined) in
%the parsed file. It can also be used stand alone, provided that the file to be indexed has already been annotated.
%The index table id used by the indexer is 'predicate_definitions'.



:- use_module(library('org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('org/cs3/pdt/model/pdt_index')).
:- use_module(library('org/cs3/pdt/model/pdt_handle')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).


:- pdt_annotator([file],[
	library('org/cs3/pdt/annotate/export_annotator'),
	library('org/cs3/pdt/annotate/member_annotator')
]).

file_annotation_hook([File|_],_,Annos,[indexed(IxTime)|Annos]):-
    time_file(File,ModTime),
    time_index(File,IxTime),
    update_index(File,Annos,ModTime,IxTime).

cleanup_hook(File):-
    pdt_clear_index(File).

cleanup_hook(File,Annos):-
    pdt_clear_index(File,Annos).

    
pdt_update_index(FileSpec):-
    pdt_file_spec(FileSpec,File),
    time_file(File,ModTime),
    time_index(File,IxTime),
    pdt_file_annotation(File,Annos,_),
    update_index(File,Annos,ModTime,IxTime).

pdt_clear_index(FileSpec):-
    pdt_file_spec(FileSpec,File),
    pdt_file_annotation(File,Annos,_),
    clear_index(File,Annos).

pdt_clear_index(FileSpec,Annos):-
    pdt_file_spec(FileSpec,File),
    clear_index(File,Annos).

    
update_index(_,_,ModTime,IxTime):-
	    ModTime @=< IxTime,
	    !.
	    
update_index(File,Annos,_,_):-	    
	get_time(IxTime),
	set_index_time(File,IxTime),		
	update_predicate_definitions(File,Annos),
	update_module_definitions(File,Annos).
	
	
clear_index(File,Annos):-
	clear_index_time(File),
	clear_predicate_definitions(File,Annos),
	clear_module_definitions(File,Annos).

clear_predicate_definitions(File,Annos):-
	member(defines(Definitions),Annos),
	pdt_index_load(predicate_definitions,IX),
	unindex_clauses(File,Definitions,IX,NewIX),
	pdt_index_store(predicate_definitions,NewIX).

update_predicate_definitions(File,Annos):-
	pdt_index_load(predicate_definitions,IX),
	index_clauses(File,Annos,IX,NewIX),
	pdt_index_store(predicate_definitions,NewIX).

clear_module_definitions(File,Annos):-
	pdt_index_load(module_definitions,Ix),	
	get_module_exports(Annos,Module,Handles),
	module_definition_index_entry(Module,File,Handles,Key,Value),
	pdt_index_remove(Ix,Key,Value,NewIX),
	pdt_index_store(module_definitions,NewIX).
debugme.
update_module_definitions(File,Annos):-
    (	File=='/tmp/test_input_11.pl'
    ->	debugme
    ;	true
    ),
	pdt_index_load(module_definitions,Ix),
	get_module_exports(Annos,Module,Handles),
	module_definition_index_entry(Module,File,Handles,Key,Value),
	pdt_index_put(Ix,Key,Value,NewIX),
	pdt_index_store(module_definitions,NewIX).
	
get_module_exports(Annos,Module,Handles):-
	memberchk(defines_module(Module),Annos),
	!,
	memberchk(exports(Exports),Annos),
	exports_to_handles(Module,Exports,Handles).		
get_module_exports(Annos,user,Handles):-	
	memberchk(defines(Defs),Annos),
	defines_to_handles(Defs,Handles).




defines_to_handles([],[]).
defines_to_handles([Module:Name/Arity|Defines],[Handle|Handles]):-
    !,
    pdt_virtual_handle(predicate_definition,[module(Module),name(Name),arity(Arity)],Handle),
    defines_to_handles(Defines,Handles).

exports_to_handles(_,[],[]).
exports_to_handles(Module,[Name/Arity|Exports],[Handle|Handles]):-
    !,
    pdt_virtual_handle(predicate_definition,[module(Module),name(Name),arity(Arity)],Handle),
    exports_to_handles(Module,Exports,Handles).
exports_to_handles(Module,[_|Exports],[Handles]):-
    exports_to_handles(Module,Exports,Handles).


index_clauses(File,Annos,IX,NewIX):-
    	member(defines(Defs),Annos),
   	member(defines_multifile(MultifileDefs),Annos),
   	member(defines_dynamic(DynamicDefs),Annos),
   	member(defines_module_transparent(TransparentDefs),Annos),
   	(	member(exports(Exports),Annos)
   	->	true
   	;	Exports=[]
   	),   	
	index_clauses(IX,File,Defs,DynamicDefs,MultifileDefs,TransparentDefs,Exports,NewIX).
	

index_clauses(IX,_,[],_,_,_,_,IX).
index_clauses(IX,File,[Def|Defs],DynamicDefs,MultifileDefs,TransparentDefs,Exports,OutIX):-
    index_clause(IX,File,Def,DynamicDefs,MultifileDefs,TransparentDefs,Exports,
	                      NextDynamicDefs,NextMultifileDefs,NextTransparentDefs,NextExports,NextIX),
	index_clauses(NextIX,File,Defs,NextDynamicDefs,NextMultifileDefs,NextTransparentDefs,NextExports,OutIX).


index_clause(IX,File,Def,DynamicDefs,MultifileDefs,TransparentDefs,Exports,
	                 NextDynamicDefs,NextMultifileDefs,NextTransparentDefs,NextExports,NextIX):-
	M:N/A=Def,
	Props0=[file(File),module(M),name(N),arity(A)],
	matcher(Def,DynamicDefs,dynamic(true),Props0,Props1,NextDynamicDefs),
	matcher(Def,MultifileDefs,multifile(true),Props1,Props2,NextMultifileDefs),
	matcher(Def,TransparentDefs,transparent(true),Props2,Props3,NextTransparentDefs),	
	matcher(N/A,Exports,exported(true),Props3,Props4,NextExports),	
	index_clause(IX,File,Def,Props4,NextIX).

index_clause(IX,File,Def,Props,NextIX):-
    clause_definition_index_entry(Def,File,Props,Key,Value),
    pdt_index_put(IX,Key,Value,NextIX).

unindex_clauses(_,[],IX,IX).
unindex_clauses(File,[Definition|Definitions],IX,NewIX):-
    clause_definition_index_entry(Definition,File,_,Key,Value),    
    pdt_index_remove(IX,Key,Value,NextIX),
    unindex_clauses(File,Definitions,NextIX,NewIX).

% clause_definition_index_entry(+Signature,
%								+File
%                               +Props,
%                               -Key,
%                               -Value)
clause_definition_index_entry(Module:Name/Arity,
                              File,
                              Props,
                              Name,
                              handle(id(File,Module:Name/Arity), predicate_definition,Props)).
                              
module_definition_index_entry(Name,File,Exports,Name,handle(id(Name,File), module_definition,[name(Name),file(File),exports(Exports)])).
	    
:- dynamic index_time/2.
time_index(File,IxTime):-
    index_time(File,IxTime),
    !.    
time_index(_, -1).    

set_index_time(File,Time):-
    retractall(index_time(File,_)),
    assert(index_time(File,Time)).
    
clear_index_time(File):-
    retractall(index_time(File,_)).


% matcher(+Elm,+Elms,+Prop,+InProps,-OutProps,-OutElms)
%
% Elms should be a sorted, list.
% Searches in Elms for elements matching Elm. If one is found,
% OutProps is unified with [Prop|OutProps].
% OutElms is unified with the first suffix of elms whos elements
% are stricly greater then Elm.
% 
matcher(Elm,[Elm|Elms],Prop,InProps,[Prop|InProps],OutElms):-
    !,%head matches
    pdt_chop_after(Elm,Elms,OutElms).
matcher(Elm,Elms,_,InProps,InProps,Elms):-
    %head > Elm. There cannot be another match.
    pdt_chop_before(Elm,Elms,Elms), % same as chop_after in this case.
    !.
matcher(Elm,Elms,Prop,InProps,OutProps,OutElms):-
	%head < Elms. There may be a match further down the list.
	pdt_chop_before(Elm,Elms,NextElms),	    
	matcher(Elm,NextElms,Prop,InProps,OutProps,OutElms).



