/*
    The module pdtplugin provides helper predicates
    for the PDT Eclipse Plugin  */

:- module(pdtplugin,[
    get_file_pos/7,
    get_pred/6,
    find_pred/6,
    atom_concat/4,atom_concat/5,
    atom_concat/6,atom_concat/7,
    get_references/5,
    manual_entry/3,
    pef_and_spec/5]).

:- use_module(library(help)).
/*
meta_data(?Filename,?Module,?Name,?Arity,?Public,Position,?Length,?Dynamic,?Multifile)
*/
:- dynamic user:meta_data/9.
:- multifile user:meta_data/9.
/*
meta_data_help(?Module,?Name,?Arity,?Comment)
*/
:- dynamic user:meta_data_help/4.
:- multifile user:meta_data_help/4.
:- dynamic user:meta_data_module/3.
:- multifile user:meta_data_module/3.

/*
 atom_concat(?a,?b,?c,?d)
 concats the atoms ?a, ?b and ?c to the atom ?d.

 Uses the buildin atom_concat/3:
    atom_concat(_1,_tmp,_4),
    atom_concat(_2,_3,_tmp).
 
*/
%measure:

atom_concat(_1,_2,_3,_4) :-
    atom_concat(_1,_tmp,_4),
    atom_concat(_2,_3,_tmp).

atom_concat(_1,_2,_3,_4,_5) :-
    atom_concat(_1,_2,_tmp,_5),
    atom_concat(_3,_4,_tmp).

atom_concat(_1,_2,_3,_4,_5,_6) :-
    atom_concat(_1,_2,_3,_tmp,_6),
    atom_concat(_4,_5,_tmp).


atom_concat(_1,_2,_3,_4,_5,_6,_7) :-
    atom_concat(_1,_2,_3,_4,_tmp,_7),
    atom_concat(_5,_6, _tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7,_8) :-
    atom_concat(_1,_2,_3,_4,_5,_tmp,_8),
    atom_concat(_6,_7, _tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7,_8,_9) :-
    atom_concat(_1,_2,_3,_4,_5,_6,_tmp,_9),
    atom_concat(_7,_8, _tmp).


/*
pdtplugin_get_file_pos(_pred,_arity, _file,_pos,_dynamic, _multifile) :-
    explain(_pred,_info),
    atom_concat(_basic_info,'\n        ',_filepos,_info),
    atom_concat(_module,':',_pred,'/',_arity,' is a ',_dynMul,'predicate defined in',_basic_info),
    (atom_concat('dynamic',_,_dynMul) ->
        _dynamic = 1;
        _dynamic = 0),
    (atom_concat(_,'multifile ',_dynMul) ->
        _multifile = 1;
        _multifile = 0),
    atom_concat(_file_we,'.pl:',_pos_atom,_filepos),
    atom_number(_pos_atom,_pos),
    atom_concat(_file_we,'.pl',_file).
*/

% this is needed for modules (in the PrologElementData the arity -1 is used for modules
get_file_pos(_,Name,-1, File,0,0, 0):-
    !,
    meta_data_module(File,Name,_).

get_file_pos(_, Name,Arity, File,Pos,0, 0) :-
    get_file_pos_(Name,Arity, File,Pos,0, 0),!.

get_file_pos(Context, Name,Arity, Context,Pos,0, 0) :- 
%    get_file_pos_context_(Context, Name,Arity, File,Pos,0, 0).
    meta_data(Context, _, Name, Arity, _,Pos,_,_,_).
    
get_file_pos_(Name,Arity, File,Pos,0, 0) :-
    term_for_signature(Name,Arity,Pred),
    nth_clause(Pred,_,Ref),
    clause_property(Ref,file(File)),
    clause_property(Ref,line_count(Pos)).

get_file_pos_context_(Context, Name,Arity, Context,Pos,0, 0) :-
    meta_data(Context, _, Name, Arity, _,Pos,_,_,_).
%    sformat(Functor, '~a:~a',[Module, Name]),
%    get_file_pos(_, Functor,Arity, File,Pos,0, 0).
/*
    term_for_signature(Name,Arity,Pred),
    nth_clause(Pred,_,Ref),
    clause_property(Ref,file(File)),
    !,
    clause_property(Ref,line_count(Pos)).
*/

/*    atom_concat(_basic_info,'\n        ',_filepos,_info),
    atom_concat(_module,':',_pred,'/',_arity,' is a ',_dynMul,'predicate defined in',_basic_info),
    (atom_concat('dynamic',_,_dynMul) ->
        _dynamic = 1;
        _dynamic = 0),
    (atom_concat(_,'multifile ',_dynMul) ->
        _multifile = 1;
        _multifile = 0),
    atom_concat(_file_we,'.pl:',_pos_atom,_filepos),
    atom_number(_pos_atom,_pos),
    atom_concat(_file_we,'.pl',_file).*/

%:- dynamic pdtplugin_get_pred_exists/2.

% pdtplugin_get_pred(_file, _name,_arity,_pos,_dyn,_mul) :-
%   priv_pdtplugin_get_pred(_file, _name,_arity,_pos,_dyn,_mul),
%    retractall(pdtplugin_get_pred_exists(_,_)).

get_pred(_file, _name,_arity,_pos,_dyn,_mul) :-
    source_file(_module_pred, _file),
%    print(_module_pred),
%    format('~n',[]),
    remove_module_prefix(_module_pred,_pred),    
%    print(_pred),
%    format('~n~n',[]),
    nth_clause(_pred,_,Ref),
    functor(_pred,_name,_arity),
    flush_output,
%   not(pdtplugin_get_pred_exists(_name,_arity)),
    clause_property(Ref,file(_file)),
    clause_property(Ref,line_count(_pos)),
    has_property(_pred,dynamic,_dyn),
    has_property(_pred,multifile,_mul).
%   assert(pdtplugin_get_pred_exists(_name,_arity)),
%   format('~a , ~a , ~a , ~a , ~a~n',[_name,_arity,_file,_dyn,_mul]).
    

%pdtplugin_get_pred(_file, _name,_arity,_pos,_dyn,_mul) :-
%    retractall(pdtplugin_get_pred_exists(_,_)),
%    !.

/*
pdtplugin_has_property(_pred,_prop,1) :- 
    predicate_property(_pred,_prop),
    !.
    
pdtplugin_has_property(_pred,_prop,0).

% +,-,-
pdtplugin_get_pred(_file, _name,_arity,_pos,_dyn,_mul) :-
    source_file(_module_pred, _file),
    remove_module_prefix(_module_pred,_pred),
    functor(_pred,_name,_arity),
%    !,
    pdtplugin_get_file_pos(_name,_arity, _file,_pos,_dyn,_mul).
*/


/*
   term_for_signature(+Name, +Arity, -Term)
*/
term_for_signature(Name,0,Term):-
    atom_to_term(Name,Term,_).

term_for_signature(Name,Arity,Term):-
    not(Arity = 0),
    freeVariables(Arity, '', Vars),
    sformat(S,'~a(~a)',[Name,Vars]),
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
    format('REFERENCE: ~a:~a: (~a)\n',[FileName,Count,Atom]),
    flush_output.


mybreakpoint:-
    true.

/*
pdt_plugin_get_references(Pred):-
    findall(_Name,(
      explain(Pred,_e),
      decode_reference(_e,Nth, Name, Arity),
      number(Arity),
      write_reference(Pred, Name, Arity, Nth)
    ),_).
*/
    
get_references(Pred,FileName,Line,Name,Arity):-
      explain(Pred,_e),
      decode_reference(_e,Nth, Name, Arity),
      number(Arity),
      term_for_signature(Name,Arity,EnclClause),
      nth_clause(EnclClause,Nth,Ref),
      clause_property(Ref,file(FileName)),
      clause_property(Ref,line_count(Line)).

decode_reference(RefStr,Nth, Pred,Arity):-  
    atom_concat('        Referenced from ',Rest,RefStr),
    atom_concat(NthAtom,'-th clause of user:', Pred,'/',ArityAtom,Rest),
    atom_number(NthAtom,Nth),
    atom_number(ArityAtom,Arity).
    
    
    
user:test(decode_reference) :-
    decode_reference('        Referenced from 1-th clause of pdtplugin:get_pred/6',
    '1', 'pdtplugin:get_pred','6').


    
remove_module_prefix(_module_pred,_pred) :-
    term_to_atom(_module_pred,_atom),
    atom_concat(_,':',_pred_atom,_atom),
    atom_to_term(_pred_atom,_pred,_),
    !.
remove_module_prefix(_pred,_pred).
    
    
user:test(atom_concat5):-
    atom_concat(a,b,c,d,abcd),
    atom_concat(a,_1,c,d,abcd),
    _1 == b,
    atom_concat(a,b,_c,d,abcd),
    _c == c.

user:test(atom_concat7):-
    atom_concat(1,2,3,4,5,6,123456).


user:test(atom_concat4):-
    atom_concat(a,b,c,abc),
    atom_concat(a,_1,c,abc),
    _1 == b,
    atom_concat(a,b,_c,abc),
    _c == c.

%pdtplugin:a
/*
    pdtplugin_find_pred(+File,+Prefix,?Module,-Name,-Arity,-Public)
    
    The more specific the the arguments are specified, the lesser
    is the number of the retrieved Predicates: Public Name/Arity 
    
    <Public> represents the module visibility (true/false)
    For performance reasons an empty prefix with an unspecified module
    will only bind predicates if File is specified.
    
    <File> specifies the file from where this query is triggered 
    <Prefix> specifies the prefix of the predicate
    <Module> specifies the defining module
    
    
    TODO: By now also the modules are bound to name (Arity == 0, Public == true)
*/

find_pred(_,Prefix,Module,Name,Arity,true):-
    var(Module),
    not(Prefix == ''), % performance tweak:
    current_predicate(Name/Arity),
    atom_concat(Prefix,_,Name).

find_pred(_,Prefix,Module,Name,Arity,Public):-
    nonvar(Module),
    setof([Pos,Len], meta_data(_,Module,Name, Arity,Public,Pos,Len,_,_),_),
    atom_concat(Prefix,_,Name).

find_pred(File,Prefix,Module,Name,Arity,Public):-
    nonvar(File),
    setof([Pos,Len], meta_data(File,Module,Name, Arity,Public,Pos,Len,_,_),_),
    atom_concat(Prefix,_,Name).

find_pred(_,Prefix,Module,Name,-1,true):-
    var(Module),
    meta_data_module(_,Name,_),
    atom_concat(Prefix,_,Name).

write_ranges_to_file(Ranges, Outfile) :-
    online_manual_stream(Manual),
    help_tmp_file(Outfile),
    open(Outfile, write, Output),
    show_ranges(Ranges, Manual, Output),
    close(Manual),
    close(Output).

manual_entry(Pred,Arity,Content) :-
    predicate(Pred,Arity,_,From,To),
    !,
    online_help:online_manual_stream(Manual),
    new_memory_file(Handle),
    open_memory_file(Handle, write, MemStream),
    stream_position(Manual, _, '$stream_position'(From, 0, 0)),
    Range is To - From,
%   current_output(Out),
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
%    set_output(Out),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle).

manual_entry(Pred,Arity,Content) :-
    meta_data_help(_,Pred,Arity,ContentString),
    string_to_atom(ContentString,Content).

manual_entry(Pred,-1,Content) :-
    meta_data_module(_,Pred,ContentString),
    string_to_atom(ContentString,Content).

user:test('pdtplugin:find_pred') :-
    bagof([Name,Arity,Public],
        find_pred('/JTransformer Engine/pdtplugin_.pl', 'a', pdtplugin_, Name, Arity,Public),
    [[atom_concat_,5,true]|[[atom_concat_,9,false]|_]]).

user:setUp('pdtplugin:find_pred') :-
    assert(user:meta_data_module('/JTransformer Engine/pdtplugin_.pl','pdtplugin_',"")),
    assert(user:meta_data('/JTransformer Engine/pdtplugin_.pl',pdtplugin_,atom_concat_,5,true,726,11,false,false)),
    assert(user:meta_data('/JTransformer Engine/pdtplugin_.pl',pdtplugin_,atom_concat_,9,false,823,11,false,false)),
    assert(user:meta_data('/JTransformer Engine/pdtplugin_.pl',pdtplugin_,get_file_pos_,7,true,1956,12,false,false)). 
user:tearDown('pdtplugin:find_pred') :-
    retract(user:meta_data_module('/JTransformer Engine/pdtplugin_.pl','pdtplugin_',"")),
    retract(user:meta_data('/JTransformer Engine/pdtplugin_.pl',pdtplugin_,atom_concat_,5,true,726,11,false,false)),
    retract(user:meta_data('/JTransformer Engine/pdtplugin_.pl',pdtplugin_,atom_concat_,9,false,823,11,false,false)),
    retract(user:meta_data('/JTransformer Engine/pdtplugin_.pl',pdtplugin_,get_file_pos_,7,true,1956,12,false,false)). 

/**
 * pef_and_spec(+Id,-Functor,-Args,-ArgDescriptions)
 *
 * binds the functor and the argument descriptions.
 * 
 * The descriptions are lists for every argument containing:
 * 1. Argumentname (parent,..)
 * 2. Kind         (id, attr)
 * 3. List or Atom (list,atom)
 * 4. constraints  (atom,classDefT,...)
 * 5. isVariable   (yes|no) (yes only in the error case!)
 */

pef_and_spec(Id,Functor,Args,ArgDescr,AtomTerm):-
    getTerm(Id,Term),
    term_to_atom(Term,AtomTerm),
    Term =.. [Functor|Args],
    ast_node_def('Java',Functor,ArgDefs),
    retrieve_argument_description(Args,ArgDefs,ArgDescr).
    
retrieve_argument_description([],_,[]).
retrieve_argument_description([Arg|Args],
        [ast_arg(Name, mult(_,_,Ord),Kind,Constraints)|ArgDefs], 
        [[Name,Kind,List,Constraints,IsVar]|ArgDescr]):-
    ((Ord = ord) ->
       List = list; List = atom),
    (var(Arg) ->
       IsVar = yes; IsVar = no),
	retrieve_argument_description(Args,ArgDefs,ArgDescr).
         