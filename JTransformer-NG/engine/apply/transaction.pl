:- dynamic dirty_tree/1. 
:- dynamic changed/1. 
:- dynamic rollback/1.
:- multifile rollback/1.

/**
 * debug_rollback_output
 * 
 * deactivated by default.
 */
:- dynamic debug_rollback_output/0.
:- dynamic tmp_rollback_file/1.
%debug_rollback_output.


markEnclAsDirty(Elem):-
    getTerm(ID,Elem),
    not(packageT(ID,_)),
    (
    	(enclosing(ID,Encl), not(Encl = 'null'),not(packageT(Encl,_)));
     	Encl = ID
    ),
    assert1T(dirty_tree(Encl)),
    !.

markEnclAsDirty(_).
    
toggle_rollback_debug :-
    debug_rollback_output,
    !,
    format('no rollback debug~n',[]),
    retractall(debug_rollback_output).
    
toggle_rollback_debug :-
    format('rollback debug~n',[]),
        assert(debug_rollback_output).

/**
 * checkpoint_new(+Checkpoint)
 *
 * define new check point for the
 * rollback facility.
 */
checkpoint_new(Checkpoint):-
    new_id(ID),
    atom_concat(checkpoint_,ID,Checkpoint),
    asserta(rollback(rollback_checkpoint(Checkpoint))).

/**
 * checkpoint_commit(+Checkpoint)
 *
 * commit the rollback check point Checkpoint.
 * This will not commit the facts, it
 * will just remove the check point.
 * use commit/0 to remove all existing 
 * rollback information.
 *
 * TODO: rename to remove_checkpoint
 */
checkpoint_commit(Checkpoint) :-
    retractall(rollback_checkpoint(Checkpoint)).

/**
 * commit
 *
 * commits all rollback information.
 * This predicate deletes all rollback logging information.
 */
commit :-
    retractall(rollback(_)),
    retractall(changed(_)).
    
/**
 * checkpoint_rollback(+Checkpoint)
 *
 * rollback to the checkpoint Checkpoint.
 */
checkpoint_rollback(Checkpoint) :-
    catch(
	    findall(Term, 
	    (
	      rollback(Term),
	      lookup_checkpoint(Term,Checkpoint),
	      call(Term),
  	      retractall(rollback(Term)),
	      retractall(changed(Term)),
	      rollback_debug_format('~w~n',[Term]),
		  Term =.. [_| [ID|_]],
		  tree(ID, _, _),
		  assert(dirty_tree(ID))
	    ),
	    _),%catcher:
	    checkpoint_reached,
	    rollback_debug_format(
	         'rollback to checkpoint ~w was successfull.',
	         [Checkpoint]) 
	),    
    % replace deleted with created files
    findall(File, (deleted_file(File),assert(tmp_rollback_file(File)),retract(deleted_file(File))),_),
    findall(File, (created_file(File),assert(deleted_file(File)),retract(created_file(File))),_),
    findall(File, (tmp_rollback_file(File),assert(created_file(File)),retract(deleted_file(File))),_).    

lookup_checkpoint(Term, Checkpoint):-
    Term = rollback_checkpoint(Checkpoint) ->
     (delete_checkpoint(rollback_checkpoint(Checkpoint)),throw(checkpoint_reached)
     );true.    

delete_checkpoint(rollback_checkpoint(Checkpoint)):-
    retract(rollback(rollback_checkpoint(Checkpoint))),
	rollback_debug_format('~w~n',[retract(rollback_checkpoint(Checkpoint))]),
    !.

/**
 * rollback/0
 *
 * undoes all add/1, delete/1 and replace/2 operations
 * since the last rollback evaluation.
 * 
 */
rollback :-
    findall(Term, 
    (
      rollback(Term),
      (delete_checkpoint(Term);
       (
	      call(Term),
	      rollback_debug_format('~w~n',[Term]),
		  Term =.. [_| [ID|_]],
		  tree(ID, _, _),
		  assert(dirty_tree(ID))
	   ))
    ),
    _),
    retractall(rollback(_)),
    retractall(changed(_)),
    % replace deleted with created files
    findall(File, (deleted_file(File),assert(tmp_rollback_file(File)),retract(deleted_file(File))),_),
    findall(File, (created_file(File),assert(deleted_file(File)),retract(created_file(File))),_),
    findall(File, (tmp_rollback_file(File),assert(created_file(File)),retract(deleted_file(File))),_).    
    

rollback_debug_format(FormatString,List):-
	debug_rollback_output -> 
	      	format(FormatString,List)
	      	;true.
		 

    
logChange(Elem):-
	Elem =.. [_|[Id|_]],
	slT(Id,_,_),
	!,
	assert1T(changed(Id)).
logChange(_).
    
test(double_fact):-
    assert(t(term(a))),
    assert(t(term(a))),
    findall(Term, 
    	(
    	t(Term),
      term_to_atom(Term,Atom),
      format('~a~n',[Atom])),_),
      retractall(t(_)).
    	
remove_dirty_flags :-
    retractall(dirty_tree(_)).
    
    
    
test(checkpoint_rollback):-
    add(classDefT(r1,2,3,4)),
    add(classDefT(r2,2,3,4)),
    checkpoint_new(New),
    add(classDefT(r3,2,3,4)),
	checkpoint_rollback(New),
    assert_true(classDefT(r1,2,3,4)),
    assert_true(classDefT(r2,2,3,4)),
    assert_fail(classDefT(r3,2,3,4)).
      
tearDown(checkpoint_rollback) :-
	rollback,
    assert_fail(classDefT(r1,2,3,4)),
    assert_fail(classDefT(r2,2,3,4)).
	