package org.cs3.pl.prolog;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class FileSearchPathConfigurator {

	public static void configureFileSearchPath(PrologLibraryManager mgr,
			PrologSession session, String[] libIds) throws PrologException,
			PrologInterfaceException {
	
		StringBuffer sb = new StringBuffer();
		PrologLibrary[] required = FileSearchPathConfigurator.getRequiredLibs(mgr, libIds);
		for (int i = 0; i < required.length; i++) {
			PrologLibrary lib = required[i];
	
			if (sb.length() > 0) {
				sb.append(',');
			}
			sb.append("(	user:file_search_path(" + lib.getAlias() + ", '"
					+ lib.getPath() + "')" + "->	true"
					+ ";	user:assert(file_search_path(" + lib.getAlias()
					+ ", '" + lib.getPath() + "'))");
			if ("true".equals(lib.getAttributeValue("hidden"))) {
				sb.append(", pdt_util:assert(pdt_hidden_path('" + lib.getPath()
						+ "'))");
			}
			sb.append(")");
		}
		session.queryOnce(sb.toString());
	}

	public static PrologLibrary[] getRequiredLibs(PrologLibraryManager mgr,
			String[] libIds) {
		Stack<String> neededLibIds = new Stack<String>();
		Set<PrologLibrary> required = new HashSet<PrologLibrary>();
		for (int i = 0; i < libIds.length; i++) {
			if (mgr.resolveLibrary(libIds[i]) == null) {
				throw new IllegalArgumentException("library id " + libIds[i]
						+ " is unresolved");
			}
			if (mgr.getBrokenLibraries().contains(libIds[i])) {
				throw new IllegalArgumentException("library id " + libIds[i]
						+ " is broken");
			}
			neededLibIds.add(libIds[i]);
		}
		while (!neededLibIds.isEmpty()) {
			String key = neededLibIds.pop();
			PrologLibrary lib = mgr.resolveLibrary(key);
			if (lib == null) {
				// this should not happen
				throw new IllegalStateException("unresoved: " + key
						+ ". Bug in LibraryManager?");
			}
			if (!required.contains(lib)) {
				required.add(lib);
				neededLibIds.addAll(lib.getDependencies());
			}
		}
		return required.toArray(new PrologLibrary[required.size()]);
	}

}
