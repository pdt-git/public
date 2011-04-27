 /*:- style_check(-singleton).     % No warnings for singleton vars
 :- style_check(-discontiguous). % No warnings for discontig. preds
 
 :- set_prolog_flag( verbose_consult, true ). 
 :- set_prolog_flag( unknown, warning ). % Behaviour for undef pred
 :- set_prolog_flag( toplevel_print_options,
     [ quoted(true),       % enclose atoms and functors in quotes, 
                           % if necessary
       max_depth(50),      % maximun printing depth of terms
       attributes(portray),% write_term(+Term) hands attributed 
                           % variables to attr_portray_hook/2
       portray(true)       % write_term(+Term) hands non-variable
                           % terms to portray/1
     ]
    ).
         
% ----------------------------------------------------------------
% Representation of program as factbase
% ----------------------------------------------------------------

 :- dynamic exists/1.
 :- multifile exists/1.
 :- dynamic ct/3.
 :- multifile ct/3.
 :- multifile ctc_test/1.
 :- multifile ctc_test/4.  
 
% ----------------------------------------------------------------
% Load CTC
% ----------------------------------------------------------------

 % Define search path aliases used below  
 % and load general utility predicates:   

set_file_search_path(Alias,Path) :- 
    retractall(file_search_path(Alias,_)),
    assert(file_search_path(Alias,Path)).
    
set_ctcroot_path :-
    predicate_property(set_ctcroot_path, file(File)),
    file_directory_name(File,Path),
    atom_concat(RootPath, '/ct-core', Path),
    set_file_search_path(ctcroot,RootPath),
    set_file_search_path(ctcore,Path).

% Path initialisation for all CTC components: -------------------
% :- set_ctcroot_path.
 
 :- set_file_search_path( ctc_interpreter,  ctcore('/ct_interpreter')).
 :- set_file_search_path( ctc_utils,        ctcore('/ctc_utils')). 
% :- consult(ctc_utils(database)).      % Assert, retract, ...*/
 :- consult(database).
 
 % Load CTC (Conditional Transformation Core): 

% :- use_module(ctc_interpreter(ctc_admin)).  
 :- use_module(ctc_admin).