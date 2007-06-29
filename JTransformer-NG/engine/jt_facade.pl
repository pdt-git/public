:- module(jt_facade,
  [deepRetractExternFQN/1,
  removeFileAndTrackDeletion/1,
  removeFile/1,
  removeJavaErrorMarker/0,
  addJavaErrorMarker/0,
  jt_facade/1,
  unmodifiedPersistantFactbase/0,
  setUnmodifiedPersistantFactbase/1
  ]).

% will not work if linker is not in build path
:- use_module(library('linker/delta.pl')).

jt_facade(removeFileAndTrackDeletion/1).
jt_facade(removeFile/1).
jt_facade(deepRetractExternFQN/1).
jt_facade(remove_contained_global_ids/1).
jt_facade(unresolved_types/1).
jt_facade(removeJavaErrorMarker/0).
jt_facade(addJavaErrorMarker/0).
jt_facade(retractLocalSymtab/0).
jt_facade(clearTreeFactbase/1).
jt_facade(clearTreeFactbase/0).

/**
 * deepRetractExternFQN(+FQN) 
 *
 */
deepRetractExternFQN(FQN) :-
	globalIds(FQN, CID), 
	externT(CID), 
	deepRetractToplevel(CID).
	
removeFileAndTrackDeletion(Path):-
  toplevelT(Toplevel, _,Path, _),
  deepRetractToplevel(Toplevel),
  walkTree(retract_with_delta, Toplevel).
  
removeFile(Path):-

  toplevelT(Toplevel, _,Path, _),
  walkTree(retract_with_delta, Toplevel).


removeJavaErrorMarker :-
  retractall(errors_in_java_code).
  
addJavaErrorMarker :-
  assert(errors_in_java_code).
  
/**
 * inlinedAnnotation(AnnotationID, annotation(AnnotationTypeFQN, Args)) *<p>
 * e.g.
 * annotation('org.cs3.AnnotationX', [(Name=Value),...]).
 * <p>
 * Allowed Values: 
 *  </menu>
 *  <li>annotation(AnnotationID | [AnnotationID1,...])</li>
 *  <li>literal(Atom| [Atom1,...],ValueType) </li>
 *  <li>type(FQN|[FQN1,...])               </li>
 * </menu>
 */  
inlinedAnnotation(AnnotatedID, annotation(AnnotationTypeFQN, InlinedMemberValues)):-
    inlined_annotation(AnnotatedID, annotation(AnnotationTypeFQN, InlinedMemberValues)).
		      

activate_delta :-
    delta:activate.

deactivate_delta :-
    delta:deactivate.
    

/**
 * unmodifiedPersistantFactbase/0
 *
 * This flag is used on the Java side
 * to track the status of the factbase.
 * When loaded from disk, this flag is set.
 * If the factbase is modified this flag is removed.
 *
 * The flag is set via the convinient predicate setUnmodifiedPersistantFactbase/1.
 */
:- dynamic unmodifiedPersistantFactbase/0.

/**
 * setUnmodifiedPersistantFactbase(true|false)
 *
 * See unmodifiedPersistantFactbase/0.
 */
setUnmodifiedPersistantFactbase(true):-
  retractall(unmodifiedPersistantFactbase),
  assert(unmodifiedPersistantFactbase).
 
setUnmodifiedPersistantFactbase(false):-
  retractall(unmodifiedPersistantFactbase).
    