

/**
 * delete_toplevel(+Path)
 * 
 * Deletes all facts related to the toplevel 
 * specified by Path.
 * Path is the full path to the toplevel: 
 * /Project/OptionalSourcepaths/Package1/../Classname.java"
 */

delete_toplevel(Path):-
	rollback, 
	toplevelT(ID, _, Path, _), 
	deepRetract(ID).

delete_toplevel(_).