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

/**
 * Synchronizes updates and queries
 * on the factbase.
 *
 * The transaction mechanism is simple.
 * Use add/1 and delete/1 to make
 * changes to the database and commit/0 to commit
 * these changes for the current thread (PrologSession).
 * With query/1 you can synchronize arbitrary
 * queries with the commit/1 predicate.
 * Use rollback/0 to dismiss the operations for the current 
 * thread.
 *
 * 
 *
 * The binding of observed predicates will be 
 * saved in idb/1 facts. So, these facts should
 * be queried with 
 * sync:query(idb(observed_predicate(A,B))) instead of
 *
 * sync:query(observed_predicate(A,B)).
 *
 * Details: The add/1 and delete/1 operation are temporarily
 * stored in a thread local predicate until 
 * you use commit/0 predicate to write these changes 
 * to the current factbase.
 * 
 * @author Andreas Linder
 * @changed Tobias Rho
 * @date   27.09.2005
 */
 
:- module(sync,
  [
%
% Schmatz (2006-10-29):
%
% I've commented out all predicates from the public list
% because otherwise name clashes occur!
%
% You now have to prefix explicitly all 'calls' to this
% module with 'sync:'.
% Anyway, that's no problem so far because all accesses
% to this module are performed explicitly with 'sync:'.
%
% end - Schmatz
%
%    add/1,
%    delete/1,
%    deleteAll/1,
%    query/1,
%    commit/0,
%    rollback/0,
%    rollback/1,
%    depends/2
	increment_snapshot_count/1
  ]).
  
/**
 * enable debugging of the sync messages via debug(sync).
 */
  
:- dynamic depends/2.
:- multifile depends/2.

:- thread_local edb_local/3.
:- dynamic idb/2.
:- dynamic idb_copy/2.
:- thread_local depends_local/1.

:- dynamic num_observers_for_term/2.

/* depends_fact(a, b). */
/* depends_fact(b, c). */
/**
 * Examples:
 *
 * depends_fact(drogen, drogensuechtiger).
 * depends_fact(eltern, kind).
 * depends_fact(grosseltern, eltern).
 */

:- use_module(library(pif_observe)).

pif_observe:pif_observe_hook(_,Subject,_):-
	init_idb(Subject).

pif_observe:pif_unobserve_hook(_,Subject,_):-
	unregister_observer(Subject). 
	
	
:- dynamic depends_fact/2.	
	
depends_fact(Term, Term).

depends(Var, _):-
  var(Var),
  throw('sync:depends_fact/2: first argument must be bound.').

depends(Master, Slave) :-
  depends_fact(Master, Slave).
  
depends(Master, Slave) :-
  depends_fact(Master, B),
  B \= Master,
  depends(B, Slave).



/********  API *************/

/**
 * add(+Term)
 *
 * Der Term wird als hinzuzufuegender Fakt 
 * im Format edb_local/3 aufgenommen.
 *
 */
add(Term) :- 
  debug(sync,'ADDED TERM: ~w~n',[Term]),
  thread_self(ThreadID), 
  assert(edb_local(ThreadID, Term, assert)). 

/**
 * delete(+Term)
 * Der Term wird als zu loeschender Fakt 
 * im Format edb_local/3 aufgenommen.
 */
delete(Term) :- 
  debug(sync,'DELETED TERM: ~w~n',[Term]),
  thread_self(ThreadID), 
  assert(edb_local(ThreadID, Term, retract)).
  
/**
 * delete(+Term)
 * Der Term wird als zu loeschender Fakt 
 * im Format edb_local/3 aufgenommen.
 */
deleteAll(Term) :- 
  debug(sync,'DELETED ALL TERMS: ~w~n',[Term]),
  thread_self(ThreadID), 
  assert(edb_local(ThreadID, Term, retractall)).

/**
 * query(+Term)
 *
 * Use query/1 to evaluate a query transaction-safe.
 * All changes on the extensional database (edb/1)
 * synchronized with the query.
 * 
 * To access the pre evalutated observed predicates
 * wrapp the observed predicates with idb/1.
 */
query(Term) :-
  with_mutex(readwrite, call(Term)).

/** 
 * rollback
 *
 * Alle in edb_local/3 angenommenen Fakten werden geloescht.
 * Die Transaktion wird zurueckgesetzt.
 */
rollback :-
  thread_self(ThreadID),
  rollback(ThreadID).



/********  internal *************/
internal_notify(Functor, Term, Arity) :-
  sformat(Str,'~w/~w',[Functor,Arity]),
  string_to_atom(Str,Subject),
  debug(sync,'Notifying subject: ~w~nTerm: ~w~n',[Subject,Term]),
  assert1(depends_local(Subject)).


assert1(Term):-
    call(Term),
    !.
assert1(Term):-
    assert(Term).
    
transact(ThreadID, Op, Term) :-
  retract(edb_local(ThreadID, Term, Op)),
  display(Op, Term),
  call(Op, Term),
  functor_module_safe(Term,ModuleFunctor, Arity),
  separate_functor_arity_module_safe(Signature,ModuleFunctor,Arity),
  forall(depends(Signature, Dependant),
      ( separate_functor_arity_module_safe(Dependant, DFunctor, DArity),
      internal_notify(DFunctor, Term, DArity))).

internal_commit(ThreadID) :- 
  debug(sync,'COMMIT~n',[]),
  increment_snapshot_count(NewSnapshotCount),
  debug(sync,'new snapshot count ~n',[NewSnapshotCount]),
  forall(edb_local(ThreadID, Term, Op), 
         transact(ThreadID, Op, Term)),
  findall((Subject),(depends_local(Subject),
         notify_if_predicate_updated(Subject)),_),
  retractall(depends_local(_)).

:- dynamic snapshot_count/1.

increment_snapshot_count(NewSnapshotNumber) :-
  snapshot_count(SnapshotNumber),
  deleteAll(sync:snapshot_count(_)),
  plus(SnapshotNumber,1,NewSnapshotNumber),
  add(sync:snapshot_count(NewSnapshotNumber)),
  !.
increment_snapshot_count(_) :-
  add(sync:snapshot_count(1)).

    
/**
 * notify_if_predicate_updated(+Signature)    
 *
 */
notify_if_predicate_updated(Signature) :-

   debug(sync,'~w~n',[notify_if_predicate_updated(Signature)]),
   term_to_signature(Term,Signature),
   debug(sync,'~w~n',[term_to_signature(Term,Signature)]),
   recorded(term_ref,Term),
   debug(sync,'~w~n',[succeed:recorded(term_ref,Term)]),
   forall(term_ref(Term,Ref), (
     update_idb(Term,Ref),
     (changed(Ref) ->
      pif_notify(Term, 'update');
     true)
   )),
   debug(sync,'~w~n',[succeed:forall]).

notify_if_predicate_updated(_Subject).
  
/* commit/0
   Die in edb_local/3 angenommenen Fakten werden in die
   Faktenbasis uebernommen.
*/
commit :- 
  
  thread_self(ThreadID), 
  with_mutex(readwrite, internal_commit(ThreadID)). 
  



internal_rollback(ThreadID, Term, Op) :-
  display(Op, Term), 
  retract(edb_local(ThreadID, Term, Op)).


/**
 * rollback(+ThreadID)
 *
 * Alle in edb_local/3 fuer den angegebenen Thread gespeicherten Daten werden
 *  entfernt.
 */
rollback(ThreadID) :-
  debug(sync,'ROLLBACK~n',[]),
  forall(edb_local(ThreadID, Term, Op), internal_rollback(ThreadID, Term, Op)),
  retractall(depends_local(_)).

/**
 * init_idb(+Subject)
 *
 * Subject currently MUST be a literal.
 * If idb for Functor/Arity does not exist
 * evaluate the predicate and store bindings
 * in idb(Functor(...)) facts.
 * If the predicates fails, the clause
 * idb(Functor(..)) -> fail
 * is added.
 * 
 * Evaluated by the consult server when
 * the the first observer is interested in this predicate.
 */
init_idb(AtomSubject):-
%    debug('init_idb BEGIN', AtomSubject),
%    separate_functor_arity_module_safe(Signature,Functor,Arity),
%    functor_module_safe(Term, Functor, Arity),
     %ensure, that this is the first evaluation:
    %atom_to_term(AtomSubject, Term,_),
    init_term_ref(AtomSubject,Ref),
    not(clause(idb(Ref,AtomSubject),_)),
    !,
	evaluate_and_store_idb(Ref).
% debug('init_idb END', AtomSubject).
	
init_idb(_).

/**
 * unregister_observer(+Subject)
 *
 * Subject currently MUST be a literal.
 * Removes one observer
 * for the term Subject.
 *
 * If this is the last oberver
 * the term is removed from the list.
 */
unregister_observer(Subject) :- 
  %atom_to_term(AtomSubject, Subject,_),
  term_ref(Subject, Ref),
  dec_term_ref(Ref),
  gc_idb.

/**
 * gc_idb
 *
 * Looks up all term references
 * which are not used by an
 * observer, deletes them
 * from the reference list
 * and deletes the idb facts
 * for this term.
 */

gc_idb :-
    forall((
        recorded(term_ref, _Term, Ref), 
        not(num_observers_for_term(Ref,_))
        ),
        remove_term_idb(Ref)
    ).
    


/**
 * term_ref(?Term,?Ref)
 *
 * Return unique reference
 * to the structure of Term.
 *
 * Only succeeds if the term
 * was initialized with init_term_ref/2.
 */
 
term_ref(Term, Ref) :-
  nonvar(Ref),
  var(Term),
  !,
  recorded(term_ref,Term,Ref).
  
term_ref(Term, Ref) :-
  recorded(term_ref,RecordedTerm,Ref),
  RecordedTerm =@= Term,
  !.

/**
 * init_term_ref(+Term,-Ref)
 *
 * Looks up a reference to
 * the given term. Hereby the 
 * structure of the term is relevant.
 * If the idb for this term was 
 * initialized before the =@= equivalence
 * is used to look up the term.
 *  
 */
init_term_ref(Term,Ref) :-
  recorded(term_ref,RecordedTerm,Ref),
  RecordedTerm =@= Term,
  inc_term_ref(Ref),
  !.
init_term_ref(Term,Ref) :-
  recordz(term_ref,Term,Ref),
  inc_term_ref(Ref).  

/**
 * remove_term_idb(+Ref)
 *
 * Removes a term from the term_ref
 * record and removes the observer counter
 * num_observers_for_term/2.
 */
remove_term_idb(Ref) :-
  erase(Ref),
  retractall(num_observers_for_term(Ref,_)),
  retractall(idb(Ref,_)),
  retractall(idb_copy(Ref,_)).
  
  
/**
 * inc_term_ref(+TermReference)
 *
 * Increments the number 
 * of observers for this term.
 */
inc_term_ref(Ref) :-
  not(num_observers_for_term(Ref,_)),
  assert(num_observers_for_term(Ref,1)),
  !.

inc_term_ref(Ref) :-
  num_observers_for_term(Ref,Num),
  retract(num_observers_for_term(Ref,Num)),
  plus(Num, 1, Inc),
  assert(num_observers_for_term(Ref,Inc)).

/**
 * dec_term_ref(+TermReference)
 *
 * Decrements the number 
 * of observers for this term.
 * 
 * Ensures that the number 
 * of observers is never < 1.
 * In this case the predicate num_observers_for_term
 * is removed.
 */
dec_term_ref(Ref) :-
  not(num_observers_for_term(Ref,_)),
  !.

dec_term_ref(Ref) :-
  num_observers_for_term(Ref,1),
  retract(num_observers_for_term(Ref,1)).
 
dec_term_ref(Ref) :-
  num_observers_for_term(Ref,Num),
  retract(num_observers_for_term(Ref,Num)),
  plus(Dec, 1, Num),
  assert(num_observers_for_term(Ref,Dec)).

/**
 * evaluate_and_store_idb(+Ref)
 *
 * reevaluates the query defined by the
 * reference Ref 
 * and stores the bindings in 
 * idb(Term) facts.
 * If now binding can be resolved
 * the fact (idb(Term) :- fail.) is generated.
 *
 * IMPORTANT (OPEN TO DISCUSSION): 
 * To avoid runtime errors the predicate is made 
 * dynamic and multifile if the predicate 
 * does not exist.
 *
 */
debugme :- true.
 
evaluate_and_store_idb(Ref):-
    term_ref(Term, Ref),
    catch(
	   catch(
			create_idb_facts(Ref,Term),
		     error(existence_error(_,_),_), 
		     % make 'Term' dynamic and multifile:
		     recover_from_non_existing_predicate(Ref, Term)
		    ),
	   AnyError,
       (format('~nsync:evaluate_and_store_idb/1: IGNORING ERROR ~w~n IN EVALUATION OF the term: ~w~n~n',[AnyError, Term]),
      % functor_module_safe(Term, Functor, Arity),
      % separate_functor_arity_module_safe(Signature, Functor, Arity),
       assert1(idb(Ref, instantiation_error(Term))))
    ).
	    
	    
create_idb_facts(Ref,Term) :-
    forall(call(Term), assert(idb(Ref, Term))),
	debugme,
	(
	  clause(idb(Ref, Term),_)->
	  true;
	  assert((idb(Ref, Term) :- fail))
	).
	    
recover_from_non_existing_predicate(Ref, Term):-
%   dynamic(Term),
%   multifile(Term),
   assert((idb(Ref, Term) :- fail)).

/**
 * updated_idb(TermReference)
 *
 * Delete all idb_copy/1 clauses
 * for this predicate if available.
 * Move the current idb/1 facts to idb_copy
 * and reevaluate idb/1 for this predicate.
 */
update_idb(Term,Ref):- 
    term_ref(Term,Ref),
    %Note: retractall(idb(Term)) will also delete the idb(Term):- fail fact.
    retractall(idb_copy(Ref,Term)),
    forall(clause(idb(Ref,Term),Body),
           assert((idb_copy(Ref,Term):-Body))),
    retractall(idb(Ref,_)),
    evaluate_and_store_idb(Ref).
    
/**
 * changed_idb(Term)
 *
 * TODO: Documentation
 */
%changed(Term):-
%    term_to_atom(Signature,SignatureAtom),
%    idb(instantiation_error(Signature)),
%    !.

changed(Ref):-
    findall([Term,Body], clause(idb(Ref,Term),Body), IdbBinding),
    !,
    not(findall([Term,Body], clause(idb_copy(Ref,Term),Body), IdbBinding)).
   
/******************************
 *    auxiliary predicates    * 
 ******************************/
    
debug(Functor, Attribute) :-
    format('=========== ~w: ~w===============~n~n ',[Functor, Attribute]).   

display(Op, Term) :-
  debug(sync,'~w(~w).~n',[Op,Term]).
 

/**
 * separate_functor_arity_module_safe(?PredicateSignature, ?FunctorInclModule, ?Arity)
 *
 * examples:  test:f/1, test:f, 1
 *            f/2,      f     , 2
 */
separate_functor_arity_module_safe(Atom, Functor, Arity):-
	atom(Atom),
	!,
	term_to_atom(Term,Atom),
	separate_functor_arity_module_safe_(Term, Functor, Arity).

separate_functor_arity_module_safe(Term, Functor, Arity):-
	separate_functor_arity_module_safe_(Term, Functor, Arity).
		
separate_functor_arity_module_safe_(Module:Functor/Arity, Module:Functor, Arity):-!.
separate_functor_arity_module_safe_(Functor/Arity, Functor, Arity).
    
functor_module_safe(Term, Module:Functor, Arity):-  
  Term =.. [:, Module, UnqualifiedTerm],
  !,
  functor(UnqualifiedTerm, Functor,Arity).

functor_module_safe(Term, Functor, Arity):-  
   functor(Term, Functor, Arity).


term_to_signature(Term, Signature):-
    nonvar(Signature),
    !,
    separate_functor_arity_module_safe(Signature,Functor,Arity),
    functor_module_safe(Term, Functor, Arity).
 
term_to_signature(Term, Signature):-
    nonvar(Term),
    !,
    functor_module_safe(Term, Functor, Arity),
    separate_functor_arity_module_safe(Signature,Functor,Arity).

/*
open_wordnet :-
        odbc_connect('WordNet', _,
                     [ user(jan),
                       password(xxx),
                       alias(wordnet),
                       open(once)
                     ]).
open_local_host :-
        odbc_connect('PostgreSQL ANSI', _,
                     [ user(swiuser),
                       password(s5j9a5d5),
                       alias(postgres),
                       open(once)
                     ]).
   
sync:open_local_host.
odbc_current_connection(Connection, DSN).                     
odbc_query(postgres, 'SELECT (arg1) FROM switest', row(Arg)).

odbc_get_connection
*/
 