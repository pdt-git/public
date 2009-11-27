/** 
 * This module provides the infrastructure for managing CTC data 
 * (PEFs) and CTC programs (CTs and CT sequences) in multiple  
 * modules without having to add a module parameter to each CTC 
 * operation.
 * 
 * The directive ":- ctc_module(name, F/N_list)." to be added at the  
 * beginning of a CTC program file declares that this file contains 
 * CT and/or CT sequence definitions in the Prolog module 'name' 
 * with their heads' functor/arity according to F/N_list.  
 */
 
:- module(ctc_admin,[
     % These two rather belong to the interpreter:
     ctc_id_init/0,
     new_node_id/1                    % (-NewId)
]).


/*****************************************************************
 * Predefined predicates: prev_ctc_id/1, ctc_id_init/0, new_node_id/1
 */

/**
 * Identity counter. Initialized to 10000.
 */
:- dynamic prev_ctc_id/1.

ctc_id_init :-
  retractall(prev_ctc_id(_)),
  assert(prev_ctc_id(10000)).

?- ctc_id_init.

/**
 * new_node_id(-Id): Bind id to a unique term.
 */
new_node_id(NewId) :-
   clause(user:new_id(_),_) % If new_id is defined (=JT is running)
    -> new_id(NewId)        % ... use new_id to ensure consistency
     ; ( var(NewId)         % ... otherwise use own implementation
         -> ( prev_ctc_id(Last),
              NewId is Last+1,
              retract(prev_ctc_id(Last)),
              !,
              assert(prev_ctc_id(NewId))
            )
          ; ( term_to_atom(NewId,Id),
              ctc_warning('Ignored call of operation new_node_id(Id) with non-variable Parameter (Id = ~w).' ,[Id]) 
            )
       ).
