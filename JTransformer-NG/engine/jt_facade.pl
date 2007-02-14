:- module(jt_facade,
  [deepRetractExternFQN/1,
  removeFileAndTrackDeletion/1,
  removeFile/1,
  removeJavaErrorMarker/0,
  addJavaErrorMarker/0,
  jt_facade/1]).

jt_facade(removeFileAndTrackDeletion/1).
jt_facade(removeFile/1).
jt_facade(deepRetractExternFQN/1).
jt_facade(remove_contained_global_ids/1).
jt_facade(unresolved_types/1).
jt_facade(removeJavaErrorMarker/0).
jt_facade(addJavaErrorMarker/0).
jt_facade(retractLocalSymtab/0).
jt_facade(clearTreeFactbase/1).

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
  deepRetractToplevel(Toplevel).
  
removeFile(Path):-
  toplevelT(Toplevel, _,Path, _),
  deepRetract(Toplevel).

removeJavaErrorMarker :-
  retractall(errors_in_java_code).
  
addJavaErrorMarker :-
  assert(errors_in_java_code).