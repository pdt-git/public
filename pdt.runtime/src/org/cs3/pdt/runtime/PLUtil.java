package org.cs3.pdt.runtime;

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PrologSession;

/**
 * some frequently used code fragments related to the setup of prolog runtimes.
 * 
 */
public class PLUtil {

	public static void configureFileSearchPath(PrologLibraryManager mgr,
			PrologSession session, String[] libIds) {

		StringBuffer sb = new StringBuffer();
		PrologLibrary[] required = getRequiredLibs(mgr, libIds);
		for (int i = 0; i < required.length; i++) {
			PrologLibrary lib = required[i];

			if (sb.length() > 0) {
				sb.append(',');
			}
			sb.append("(	user:file_search_path(" + lib.getAlias() + ", '"
					+ lib.getPath() + "')" + "->	true"
					+ ";	user:assert(file_search_path(" + lib.getAlias()
					+ ", '" + lib.getPath() + "'))" + ")");
		}

		session.queryOnce(sb.toString());

	}

	public static PrologLibrary[] getRequiredLibs(PrologLibraryManager mgr,
			String[] libIds) {

		Stack todo = new Stack();
		Set required = new HashSet();
		for (int i = 0; i < libIds.length; i++) {
			if (mgr.resolveLibrary(libIds[i]) == null) {
				throw new IllegalArgumentException("library id " + libIds[i]
						+ " is unresolved");
			}
			if (mgr.getBrokenLibraries().contains(libIds[i])) {
				throw new IllegalArgumentException("library id " + libIds[i]
						+ " is broken");
			}
			todo.add(libIds[i]);

		}

		while (!todo.isEmpty()) {
			String key = (String) todo.pop();
			PrologLibrary lib = mgr.resolveLibrary(key);
			if (lib == null) {
				// this should not happen
				throw new IllegalStateException("unresoved: " + key
						+ ". Bug in LibraryManager?");
			}
			if (!required.contains(lib)) {
				required.add(lib);
				todo.addAll(lib.getDependencies());
			}

		}

		return (PrologLibrary[]) required.toArray(new PrologLibrary[required
				.size()]);
	}

	public static CTerm[] listAsArray(CTerm term) {
		Vector v = new Vector();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			v.add(compound.getArgument(0));
			term = compound.getArgument(1);
		}
		return (CTerm[]) v.toArray(new CTerm[v.size()]);
	}
	
	/**
	 * Converts a property list term to a Map.
	 * 
	 * The argument should be list containing elements of the form property(value).
	 * Atomic elements are interpreted as if they were of the form property(true).
	 * The returned map will contain a single value for each property found.
	 * If the same property name is encountered more than once, the returned map will
	 * reflect only the last occurance.
	 * @see listAsMultiMap(CTerm) TODO
	 * @param term a term with functor "./2"
	 *  
	 * @return a map with keytype string, value type CTerm
	 */
	public static Map listAsMap(CTerm term) {
		Map m = new HashMap();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			CTerm propertyTerm = compound.getArgument(0);
			if(propertyTerm instanceof CCompound && propertyTerm.getArity()==1){
				m.put(propertyTerm.getFunctorValue(), ((CCompound)propertyTerm).getArgument(0));
			}
			else if(propertyTerm.getArity()==0){
				m.put(propertyTerm.getFunctorValue(), "true");
			}
			
			term = compound.getArgument(1);
		}
		return m;
	}
}
