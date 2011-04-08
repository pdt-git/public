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
% ad 1: A program part is said to 'depend, either directly or indirectly,
%   on parts of the Eclipse framework', if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the 'pdt' package
%   fragment in their name fall into this category.
%
% ad 2: 'Third-party code' means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    The module pdtplugin provides helper predicates
    for the PDT Eclipse Plugin  */

:- module(pdtplugin,[
    find_pred/8,
    find_declaration/6,
    get_references/8,
    get_pred/7,
    pdt_reload/1,
    activate_warning_and_error_tracing/0,
    deactivate_warning_and_error_tracing/0,
    predicates_with_property/3,
    
    manual_entry/3, % still in use, but probably broken, see predicat_manual_entry
    errors_and_warnings/4
    ]).

:- use_module(library(pldoc/doc_library)).
:- use_module(library(explain)).
:- use_module(library(help)).
:- use_module(library(make)).
:- use_module(library('pldoc')).
:- use_module(library('pldoc/doc_html')).
:- use_module(library('http/html_write')).



%% get_pred(+File, -Name,-Arity,-Line,-Dyn,-Mul,-Public) is nondet.
%
% Looks up all clauses for Name/Arity defined in File.
%
% @param Line
%
% boolean properties are bound to 1 or 0.
%
get_pred(_file, _name,_arity,Line,_dyn,_mul,Public) :-
    source_file(_module_pred, _file),
	strip_module(_module_pred,_,_pred),
    nth_clause(_module_pred,_,Ref),
    functor(_pred,_name,_arity),
    clause_property(Ref,file(_file)),
    clause_property(Ref,line_count(Line)),
    has_property(_module_pred,dynamic,_dyn),
    has_property(_module_pred,multifile,_mul),
    has_property(_module_pred,exported,Public).


%%  term_for_signature(+Name, +Arity, -Term) is det.
%
term_for_signature(Name,0,Term):-
    atom_to_term(Name,Term,_).

term_for_signature(Name,Arity,Term):-
    not(Arity = 0),
    freeVariables(Arity, '', Vars),    sformat(S,'~w(~w)',[Name,Vars]),
    atom_to_term(S, Term,_).
    %functor(Term, Name, Arity).

freeVariables(0, Vars, Vars) :-
    !.

freeVariables(Arity, Prefix, Complete) :-
    plus(1, Count, Arity),
    nextFreeVar(Count, VarAtom),
    atom_concat(Prefix, VarAtom, Left),
    freeVariables(Count, Left, Complete).

nextFreeVar(0, '_') :-
    !.
nextFreeVar(_, '_, ').



write_reference(Pred,Name, Arity, Nth):-
    term_for_signature(Name,Arity,Term),
    nth_clause(Term,Nth,Ref),
    clause_property(Ref,file(FileName)),
    clause_property(Ref,line_count(Count)),
    term_to_atom(Pred,Atom),
    format('REFERENCE: ~w:~w: (~w)\n',[FileName,Count,Atom]),
    flush_output.



%% get_references(+EnclFile,+PredSignature,?Module, -FileName,-Line,-RefModule,-Name,-Arity) is nondet.
%
%  @param PredSignature PredName/Arity
%  @author TRHO
%
get_references(EnclFile, PredName/PredArity,Module, FileName,Line,RefModule,Name,Arity):-
    functor(Pred,PredName,PredArity),
	resolve_module(EnclFile,Module),
    % INTERNAL, works for swi 5.11.X
    prolog_explain:explain_predicate(Module:Pred,Explanation), 
%    writeln(Explanation),
    decode_reference(Explanation,Nth, RefModule,Name, Arity),
    number(Arity),
    functor(EnclClause,Name,Arity),
    %term_for_signature(Name,Arity,EnclClause),
    nth_clause(RefModule:EnclClause,Nth,Ref),
    clause_property(Ref,file(FileName)),
    clause_property(Ref,line_count(Line)).

      

%% decode_reference(+RefStr,-Nth, +Pred,-Arity) is nondet.
%
% Reference string from explain/2 predicate
% 
%  IMPORTANT: Hardcoded reference to the user module!
%  Only works for predicates defined in the user module!
%
decode_reference(RefStr,Nth, RefModule,Pred,Arity):-
    atom_concat('        Referenced from ',Rest,RefStr),
    atom_concat(NthAtom,'-th clause of ',RefModule,':', Pred,'/',ArityAtom,Rest),
    atom_number(NthAtom,Nth),
    atom_number(ArityAtom,Arity),
    !.

%% find_pred(+EnclFile,+Prefix,-EnclModule,-Name,-Arity,-Exported,-Builtin,-Help) is nondet.
%
% looks up all predicates with prefix Prefix defined or imported in file EnclFile.
%
% Used by the PLEditor content assist.
%
% For performance reasons an empty prefix with an unspecified module
% will only bind predicates if File is specified.
%
% <File> specifies the file from where this query is triggered
% <Prefix> specifies the prefix of the predicate
% <Module> specifies the defining module
%
%
find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help):-
	setof(
	   (Name,Arity),
	   Prefix^Module^
	   ( resolve_module(EnclFile,Module),
	     find_pred_(Prefix,Module,Name,Arity,true)
	   ),
	   All
	),
	member((Name,Arity),All),
	
	% no enclosing module specified in the code via modulename:..
	get_defining_module(EnclFile,Module,Name,Arity),
	functor(Term,Name,Arity),
	( predicate_property(Module:Term,exported)->
	  Exported=true
	; Exported=false
	),
	( predicate_property(Module:Term,built_in)->
	  Builtin=true
	; Builtin=false
	),
	predicate_manual_entry(Module,Name,Arity,Help).


find_pred(_EnclFile,Prefix,EnclModule,Name,-1,true,false,'nodoc'):-
    var(EnclModule),
	current_module(Name),
    atom_concat(Prefix,_,Name).

find_pred_(Prefix,Module,Name,Arity,true):-
    ( var(Module)->
    	not(Prefix == '')
    ; true
    ), % performance tweak:
    current_predicate(Module:Name/Arity),
    atom_concat(Prefix,_,Name),
    % rule out used built-ins, like =../2, in case the enclosing module is given (in this case the prefix might be empty):   
    ( nonvar(Module) ->
      ( functor(Term,Name,Arity),
    	(not(Prefix == '');not(built_in(Term))) )
      ; true
    ).

get_defining_module(_EnclFile,EnclModule,_Name,_Arity):-
	nonvar(EnclModule),
	!.
get_defining_module(EnclFile,Module,Name,Arity):-
     resolve_module(EnclFile,ModuleCandidate),
     current_predicate(ModuleCandidate:Name/Arity),
     functor(Head,Name,Arity),
     ( predicate_property(ModuleCandidate:Head,imported_from(Module))
     ; Module = ModuleCandidate
     ),
     !.

%% predicate_manual_entry(+Module, +Pred,+Arity,-Content) is det.
%
%
predicate_manual_entry(_Module,Pred,Arity,Content) :-
    predicate(Pred,Arity,_,FromLine,ToLine),
    !,
    online_help:line_start(FromLine, From),
    online_help:line_start(ToLine, To),
    online_help:online_manual_stream(Manual),
    new_memory_file(Handle),open_memory_file(Handle, write, MemStream),
    seek(Manual,From,bof,_NewOffset),
    Range is To - From,
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle),
    !.


predicate_manual_entry(Module, Pred,Arity,Content) :-
    %pldoc:doc_comment(Module:Pred/Arity,_File:_,_Summary,Content),
    %TODO: The html code is now available:
	pldoc:doc_comment(Module:Pred/Arity,File:_,_,_Content),
	gen_html_for_pred_(File,Pred/Arity,Content),
    !.
	
predicate_manual_entry(_Module,_Pred,_Arity,'nodoc').

gen_html_for_pred_(FileSpec,Functor/Arity,Html):-    
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

write_ranges_to_file(Ranges, Outfile) :-
    online_manual_stream(Manual),
    help_tmp_file(Outfile),
    open(Outfile, write, Output),
    show_ranges(Ranges, Manual, Output),
    close(Manual),
    close(Output).

%% manual_entry(Pred,Arity,Content) is det.
%
% TODO: take over code, or reference predicate_manual_entry
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


%% find_declaration(+EnclFile,+Name,+Arity,?Module,-File,-Line)
%
% used for the open declaration action
%
find_declaration(EnclFile,Name,Arity,Module,File,Line):-
    resolve_module(EnclFile,Module),
%	current_module(Module),
	functor(Goal,Name,Arity),
	nth_clause(Module:Goal,_,Ref),
	clause_property(Ref,file(File)),
    clause_property(Ref,line_count(Line)).


resolve_module(EnclFile,Module):-
 	var(Module),
    ( ( nonvar(EnclFile),module_property(Module,file(EnclFile)) )
    ;  Module=user
    ).

% Necessary for find_pred/8, TODO: move to find_pred/8
resolve_module(_EnclFile,_Module).


:- dynamic traced_messages/3.
:- dynamic warning_and_error_tracing/0.

activate_warning_and_error_tracing :- 
	assert(warning_and_error_tracing).

deactivate_warning_and_error_tracing:-
	retractall(warning_and_error_tracing),
	retractall(traced_messages(_,_,_)).
 
 
%% message_hook(+Term, +Level,+ Lines) is det. 
%
% intercept prolog messages to collect term positions and 
% error/warning messages in traced_messages/3
% 
% @author trho
%  
user:message_hook(_Term, Level,Lines) :-
  warning_and_error_tracing,
  prolog_load_context(term_position, '$stream_position'(_,Line,_,_,_)),
  assert(traced_messages(Level, Line,Lines)),
  fail.

%% errors_and_warnings(Level,Line,0,Message) is nondet.
%
%
errors_and_warnings(Level,Line,0,Message):-
    traced_messages(Level, Line,Lines),
%	traced_messages(error(syntax_error(_Message), file(_File, StartLine, Length, _)), Level,Lines),
    new_memory_file(Handle),
   	open_memory_file(Handle, write, Stream),
	print_message_lines(Stream,'',Lines),
    close(Stream),
	memory_file_to_atom(Handle,Message),
    free_memory_file(Handle).
      

%% pdt_reload(File) is det.
%
% wrapper for consult. Only used to ignore PLEditor triggered consults in the history.
%
pdt_reload(File):-
    writeln(File),
    make:reload_file(File).

%% predicates_with_property(+Property,-Predicates) is det.
%
% Look up all Predicates with property Property, including atomic
% properties (e.g. dynamic, built_in) AND properties that are 
% functions (e.g. meta_predicate(Head)).

% GK, 5. April 2011: Extended the implementation to deal with unary
% functors. The combination of findall and setof is essentail for 
% this added functionality. The findall/3 call finds all results
%   (even if the arguments are free variables -- note that setof/3
%   would return results one by one in such a case, not a full list!). 
% Then the setof/3 call eliminates the duplicates from the results
% of findall/3. 
% DO NOT CHANGE, unless you consider yourself a Prolog expert.

% Property = undefined | built_in | dynamic | transparent | meta_predicate(_)    

% Look for undefined predicates only in the local context 
% (of the file whose editor has just been opened):
%predicates_with_property(undefined, FileName, Predicates) :-
%    !,
%    module_of_file(FileName,Module), 
%	findall(Name, predicate_name_with_property_(Module,Name,undefined), AllPredicateNames),
%	make_duplicate_free_string(AllPredicateNames,Predicates).

predicates_with_property(Property, _, Predicates) :-
    findall(Name, predicate_name_with_property_(_,Name,Property), AllPredicateNames),
	make_duplicate_free_string(AllPredicateNames,Predicates).


module_of_file(FileName,Module) :-
    setof( Module, 
           Pred^predicate_property(Module:Pred,file(FileName)),
           Set),
    member(Module,Set).
    	
predicate_name_with_property_(Module,Name,Property):-
	predicate_property(Module:Head,Property),
	functor(Head,Name,_),
	Name \= '[]'.
	
make_duplicate_free_string(AllPredicateNames,Predicates) :-
    setof(Name, member(Name,AllPredicateNames), UniqueNames),
	sformat(S,'~w',[UniqueNames]),
	string_to_atom(S,Predicates).


	
%% predicates_with_unary_property(+Property,?Predicates,?PropertyParams) is det.
%
% Look up all Predicates with the unary property Property, e.g. meta_predicate(Head) 
% The element at position i in Predicates is the name of a predicate that has  
% the property Property with the parameter at position i in PropertyParams.
%
% Author: GK, 5 April 2011
% TODO: Integrate into the editor the ability to show the params as tool tips,
% e.g. show the metaargument specifications of a metapredicate on mouse over.
predicates_with_unary_property(Property,Predicates,PropertyArguments):-
	setof((Name,Arg),
	   predicate_name_with_unary_property_(Name,Property,Arg),
	   PredArgList),
	findall(Pred, member((Pred,_),PredArgList), AllProps),
	findall(Arg,  member((_,Arg), PredArgList), AllArgs),
	sformat(S1,'~w',[AllProps]),
	sformat(S2,'~w',[AllArgs]),
	string_to_atom(S1,Predicates),
	string_to_atom(S2,PropertyArguments).
	   	  
% helper
predicate_name_with_unary_property_(Name,Property,Arg):-
    Property =.. [__F,Arg],
	predicate_property(_M:Head,Property),
	functor(Head,Name,_),
	Name \= '[]'.
	
%%%%%%%%%% Tests %%%%%%%%%%%

user:setUp(decode_reference) :-
	assert(user:testpred(1,2)).
user:test(decode_reference) :-
    decode_reference('        Referenced from 1-th clause of user:testpred/2',
                     1, 'testpred',2).

user:tearDown(decode_reference) :-
	retract(user:testpred(1,2)).


	